(in-package #:clos-alchemy)

(defun compile-extractor (class-designator &key (mode :full)
                                                slot-list
                                                slot-types
                                                user-prompt)
  "Pre-compile an extraction pipeline for CLASS-DESIGNATOR.
MODE: :full (all slots), :required (mandatory only), :custom (only SLOT-LIST).
SLOT-TYPES: alist of (slot-name . type-spec) overrides.
USER-PROMPT: optional domain context appended to system prompt."
  (let* ((schema (class-to-schema class-designator
                                  :mode mode
                                  :slot-list slot-list
                                  :slot-types slot-types))
         (prompt (generate-system-prompt schema :user-prompt user-prompt)))
    (make-extraction-compilation :schema schema :prompt prompt)))

(defun extract (backend compilation-or-class document
                &key (max-retries 3) (temperature 0.0) max-tokens user-prompt)
  "Extract structured data from DOCUMENT and construct a CLOS instance.
COMPILATION-OR-CLASS: an extraction-compilation or a class name (auto-compiles).
Returns an extraction-result with the CLOS instance."
  (let ((compilation (etypecase compilation-or-class
                       (extraction-compilation compilation-or-class)
                       (symbol (compile-extractor compilation-or-class)))))
    (%extract-with-retry backend compilation document
                         max-retries temperature max-tokens user-prompt)))

(defun extract-list (backend compilation-or-class document
                     &key (max-retries 3) (temperature 0.0) max-tokens user-prompt)
  "Extract a list of CLOS instances from DOCUMENT.
Returns an extraction-result whose instance slot is a list."
  (let* ((compilation (etypecase compilation-or-class
                        (extraction-compilation compilation-or-class)
                        (symbol (compile-extractor compilation-or-class))))
         (schema (extraction-compilation-schema compilation))
         (list-schema (make-ir-schema
                       :name (format nil "~A_list" (ir-schema-name schema))
                       :class-name nil
                       :fields (list
                                (make-ir-field
                                 :name "items"
                                 :type (make-ir-type-list
                                        :element-type (make-ir-type-object :schema schema))
                                 :required-p t
                                 :slot-name 'items))))
         (list-compilation (make-extraction-compilation
                            :schema list-schema
                            :prompt (format nil "~A~%~%Extract ALL matching items as a JSON object with an \"items\" array."
                                            (extraction-compilation-prompt compilation)))))
    (let ((result (%extract-with-retry backend list-compilation document
                                       max-retries temperature max-tokens user-prompt)))
      (setf (extraction-result-instance result)
            (gethash "items" (extraction-result-instance result)))
      result)))

(defun %extract-with-retry (backend compilation document
                            max-retries temperature max-tokens user-prompt)
  (let* ((schema (extraction-compilation-schema compilation))
         (system-prompt (extraction-compilation-prompt compilation))
         (json-schema (schema-to-json-schema schema))
         (accumulated-errors '())
         (accumulated-attempts '())
         (accumulated-usage nil)
         (last-raw-response nil)
         (last-raw-data nil)
         (retry-context nil))
    (loop for attempt from 0 to max-retries
          do (let* ((user-content
                      (with-output-to-string (s)
                        (when user-prompt (format s "~A~%~%" user-prompt))
                        (write-string document s)
                        (when retry-context (format s "~%~%~A" retry-context))))
                    (messages
                      (list (list :role "system" :content system-prompt)
                            (list :role "user" :content user-content))))
               (multiple-value-bind (raw-response info)
                   (llm:backend-generate backend messages
                                         :output-schema json-schema
                                         :temperature temperature
                                         :max-tokens (or max-tokens 1024))
                 (setf last-raw-response raw-response)
                 (setf accumulated-usage (%accumulate-usage accumulated-usage info))
                 (handler-case
                     (let ((raw-data (parse-json-response raw-response)))
                       (multiple-value-bind (valid-p errors)
                           (validate-data raw-data schema)
                         (if valid-p
                             (let* ((instance (construct-from-data raw-data schema))
                                    (instance-errors
                                      (%run-instance-validators instance)))
                               (if instance-errors
                                   (progn
                                     (setf last-raw-data raw-data)
                                     (push (list :raw-response raw-response
                                                 :raw-data raw-data
                                                 :errors (copy-list instance-errors))
                                           accumulated-attempts)
                                     (setf accumulated-errors
                                           (nconc accumulated-errors instance-errors))
                                     (setf retry-context
                                           (%format-retry-context instance-errors)))
                                   (return-from %extract-with-retry
                                     (make-extraction-result
                                      :instance instance
                                      :raw-data raw-data
                                      :raw-response raw-response
                                      :retries attempt
                                      :usage (%info-to-usage info)))))
                             (progn
                               (setf last-raw-data raw-data)
                               (push (list :raw-response raw-response
                                           :raw-data raw-data
                                           :errors (copy-list errors))
                                     accumulated-attempts)
                               (setf accumulated-errors
                                     (nconc accumulated-errors errors))
                               (setf retry-context
                                     (%format-retry-context errors))))))
                   (generation-error (e)
                     (setf last-raw-data nil)
                     (let ((err (format nil "Parse failure: ~A"
                                        (generation-error-reason e))))
                       (push err accumulated-errors)
                       (push (list :raw-response raw-response
                                   :raw-data nil
                                   :errors (list err))
                             accumulated-attempts))
                     (setf retry-context
                           (%format-parse-retry-context e raw-response)))))))
    (error 'max-retries-error
           :retries max-retries
           :errors accumulated-errors
           :raw-response last-raw-response
           :raw-data last-raw-data
           :usage accumulated-usage
           :attempts (nreverse accumulated-attempts))))

(defun %info-to-usage (info)
  (when info
    (list :prompt-tokens (llm:response-info-prompt-tokens info)
          :completion-tokens (llm:response-info-completion-tokens info))))

(defun %accumulate-usage (accumulated info)
  (let ((usage (%info-to-usage info)))
    (if (null accumulated)
        (when usage
          (list :prompt-tokens (or (getf usage :prompt-tokens) 0)
                :completion-tokens (or (getf usage :completion-tokens) 0)))
        (if (null usage)
            accumulated
            (list :prompt-tokens (+ (getf accumulated :prompt-tokens)
                                    (or (getf usage :prompt-tokens) 0))
                  :completion-tokens (+ (getf accumulated :completion-tokens)
                                        (or (getf usage :completion-tokens) 0)))))))

(defun %run-instance-validators (instance)
  "Run validate-instance on the constructed object.
Returns nil if valid, or a list of validation-error conditions."
  (let ((error-strings (validate-instance instance)))
    (when error-strings
      (mapcar (lambda (msg)
                (make-condition 'validation-error
                                :field-name "(instance)"
                                :expected msg
                                :actual "constraint violated"))
              error-strings))))

(defun %format-retry-context (errors)
  "Format validation errors as natural language for the retry prompt."
  (with-output-to-string (s)
    (format s "Previous attempt produced invalid output. Errors:~%")
    (dolist (err errors)
      (format s "- ~A~%" err))
    (format s "Please fix these errors and try again.")))

(defun %format-parse-retry-context (error raw-response)
  "Format a parse failure as corrective context for the retry prompt."
  (with-output-to-string (s)
    (format s "Previous response could not be parsed as JSON: ~A~%"
            (generation-error-reason error))
    (format s "Raw response (first 200 chars): ~A~%"
            (subseq raw-response 0 (min 200 (length raw-response))))
    (format s "Please respond with ONLY valid JSON matching the schema. No markdown fences, no explanatory text.")))
