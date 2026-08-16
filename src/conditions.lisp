(in-package #:clos-alchemy)

(define-condition extraction-error (error) ()
  (:documentation "Base condition for clos-alchemy errors."))

(define-condition schema-error (extraction-error)
  ((class-name :initarg :class-name :reader schema-error-class-name)
   (reason :initarg :reason :reader schema-error-reason))
  (:report (lambda (c s)
             (format s "Schema error for ~A: ~A"
                     (schema-error-class-name c)
                     (schema-error-reason c)))))

(define-condition validation-error (extraction-error)
  ((field-name :initarg :field-name :reader validation-error-field-name)
   (expected :initarg :expected :reader validation-error-expected)
   (actual :initarg :actual :reader validation-error-actual))
  (:report (lambda (c s)
             (format s "Validation error for field ~S: expected ~A, got ~S"
                     (validation-error-field-name c)
                     (validation-error-expected c)
                     (validation-error-actual c)))))

(define-condition generation-error (extraction-error)
  ((backend :initarg :backend :reader generation-error-backend)
   (reason :initarg :reason :reader generation-error-reason))
  (:report (lambda (c s)
             (format s "Generation error (~A): ~A"
                     (generation-error-backend c)
                     (generation-error-reason c)))))

(define-condition max-retries-error (extraction-error)
  ((retries :initarg :retries :reader max-retries-error-retries)
   (errors :initarg :errors :reader max-retries-error-errors)
   (raw-response :initarg :raw-response :reader max-retries-error-raw-response
                 :initform nil)
   (raw-data :initarg :raw-data :reader max-retries-error-raw-data
             :initform nil)
   (usage :initarg :usage :reader max-retries-error-usage
          :initform nil)
   (attempts :initarg :attempts :reader max-retries-error-attempts
             :initform nil))
  (:report (lambda (c s)
             (format s "Extraction failed after ~D retries with ~D errors"
                     (max-retries-error-retries c)
                     (length (max-retries-error-errors c))))))

;;; Lifecycle observability (issue #30). These are plain CONDITIONs, never
;;; ERRORs — SIGNAL on an unhandled condition is a no-op, so %extract-with-retry
;;; can signal them unconditionally with zero behavior change for callers who
;;; never bind a handler.

(define-condition extraction-event (condition)
  ((attempt-number :initarg :attempt-number :reader extraction-event-attempt-number)
   (raw-response :initarg :raw-response :reader extraction-event-raw-response)
   (raw-data :initarg :raw-data :reader extraction-event-raw-data :initform nil)
   (parse-error :initarg :parse-error :reader extraction-event-parse-error :initform nil)
   (validation-errors :initarg :validation-errors :reader extraction-event-validation-errors
                      :initform nil)
   (usage :initarg :usage :reader extraction-event-usage :initform nil))
  (:documentation "Base class for extraction lifecycle observability conditions."))

(define-condition extraction-attempt (extraction-event) ()
  (:documentation
   "Signaled once per attempt, success or failure, right after that attempt's
raw response has been parsed and validated."))

(define-condition extraction-retry (extraction-event) ()
  (:documentation
   "Signaled when an attempt failed and another attempt will run. Establishes
the ABORT-EXTRACTION and RETRY-WITH-BACKEND restarts."))

(define-condition extraction-exhausted (extraction-event) ()
  (:documentation
   "Signaled immediately before MAX-RETRIES-ERROR is raised, whether reached
by running out of attempts or via the ABORT-EXTRACTION restart."))
