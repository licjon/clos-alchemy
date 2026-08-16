(in-package #:clos-alchemy/telemetry)

(defun %log-extraction-event (stream label event)
  (let ((*print-pretty* nil))
    (format stream "~A attempt=~D validation-errors=~D parse-error=~A usage=~S~%"
            label
            (extraction-event-attempt-number event)
            (length (extraction-event-validation-errors event))
            (or (extraction-event-parse-error event) "none")
            (extraction-event-usage event))))

(defmacro with-extraction-logging ((stream) &body body)
  "Wrap BODY, logging one line per extraction-attempt/-retry/-exhausted signal
to STREAM. A minimal reference consumer for clos-alchemy's lifecycle
conditions (issue #30) — a starting point for logging, metrics, or
circuit-breaking integrations, not a full observability platform."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (handler-bind ((extraction-attempt
                         (lambda (e) (%log-extraction-event ,s "ATTEMPT" e)))
                       (extraction-retry
                         (lambda (e) (%log-extraction-event ,s "RETRY" e)))
                       (extraction-exhausted
                         (lambda (e) (%log-extraction-event ,s "EXHAUSTED" e))))
         ,@body))))
