(in-package #:clos-constructor)

(define-condition extraction-error (error) ()
  (:documentation "Base condition for clos-constructor errors."))

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
   (errors :initarg :errors :reader max-retries-error-errors))
  (:report (lambda (c s)
             (format s "Extraction failed after ~D retries with ~D errors"
                     (max-retries-error-retries c)
                     (length (max-retries-error-errors c))))))
