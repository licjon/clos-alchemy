(in-package #:clos-constructor)

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
         (accumulated-errors '())
         (retry-context nil))
    (loop for attempt from 0 to max-retries
          do (let* ((full-prompt (if retry-context
                                     (format nil "~A~%~%~A" document retry-context)
                                     document))
                    (result (backend-generate backend schema full-prompt
                                             :system-prompt system-prompt
                                             :temperature temperature
                                             :max-tokens max-tokens
                                             :user-prompt user-prompt)))
               (let ((raw-response (extraction-result-raw-response result))
                     (raw-data (extraction-result-raw-data result)))
                 (multiple-value-bind (valid-p errors)
                     (validate-data raw-data schema)
                   (if valid-p
                       (let ((instance (construct-from-data raw-data schema)))
                         (return-from %extract-with-retry
                           (make-extraction-result
                            :instance instance
                            :raw-data raw-data
                            :raw-response raw-response
                            :retries attempt
                            :usage (extraction-result-usage result))))
                       (progn
                         (setf accumulated-errors (nconc accumulated-errors errors))
                         (setf retry-context
                               (%format-retry-context errors))))))))
    (error 'max-retries-error
           :retries max-retries
           :errors accumulated-errors)))

(defun %format-retry-context (errors)
  "Format validation errors as natural language for the retry prompt."
  (with-output-to-string (s)
    (format s "Previous attempt produced invalid output. Errors:~%")
    (dolist (err errors)
      (format s "- ~A~%" err))
    (format s "Please fix these errors and try again.")))
