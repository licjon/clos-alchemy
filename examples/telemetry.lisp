;;;; telemetry.lisp
;;;;
;;;; Demonstrates clos-alchemy/telemetry — clos-alchemy's lifecycle
;;;; observability protocol (issue #30): EXTRACTION-ATTEMPT, EXTRACTION-RETRY,
;;;; and EXTRACTION-EXHAUSTED conditions signaled from inside the retry loop,
;;;; plus the ABORT-EXTRACTION and RETRY-WITH-BACKEND restarts a handler can
;;;; invoke to intervene.
;;;;
;;;; Unlike the other examples, this one needs no GGUF model — it scripts
;;;; CL-LLM-BACKEND's MOCK-BACKEND so every demo is deterministic and runs
;;;; instantly.
;;;;
;;;; Four demos:
;;;;   1. Observing every attempt with a raw HANDLER-BIND
;;;;   2. The same run through WITH-EXTRACTION-LOGGING
;;;;   3. ABORT-EXTRACTION — circuit-break out of the retry loop early
;;;;   4. RETRY-WITH-BACKEND — fail over to a healthier backend mid-loop
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-alchemy/examples)
;;;;   (clos-alchemy/examples/telemetry:run)

(defpackage #:clos-alchemy/examples/telemetry
  (:use #:cl #:clos-alchemy #:clos-alchemy/telemetry)
  (:import-from #:cl-llm-backend #:make-mock-backend)
  (:export #:run))

(in-package #:clos-alchemy/examples/telemetry)

;;; ── Domain class ───────────────────────────────────────────────────

(defclass ticket ()
  ((subject  :initarg :subject  :accessor ticket-subject  :type string)
   (priority :initarg :priority :accessor ticket-priority
             :type (member :low :medium :high)))
  (:documentation "A support ticket."))

;;; ── Scripted responses ───────────────────────────────────────────

(defun good-data ()
  "{\"subject\":\"Login page returns 500\",\"priority\":\"high\"}")

(defun bad-data ()
  "\"urgent\" isn't a member of (:low :medium :high), so this fails validation."
  "{\"subject\":\"Login page returns 500\",\"priority\":\"urgent\"}")

;;; ── Helpers ────────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

;;; ── Demo 1: raw HANDLER-BIND ──────────────────────────────────────

(defun demo-raw-handler-bind ()
  (banner "DEMO 1: Observing every attempt with HANDLER-BIND")
  (format t "One bad attempt (\"urgent\" isn't a valid priority) then a good one.~2%")
  (let ((backend (make-mock-backend :responses (list (bad-data) (good-data)))))
    (handler-bind ((extraction-attempt
                     (lambda (e)
                       (format t "  [ATTEMPT ~D] validation-errors=~D usage=~S~%"
                               (extraction-event-attempt-number e)
                               (length (extraction-event-validation-errors e))
                               (extraction-event-usage e))))
                   (extraction-retry
                     (lambda (e)
                       (declare (ignore e))
                       (format t "  [RETRY] another attempt will run~%"))))
      (let* ((result (extract backend 'ticket "Users can't log in."))
             (r (extraction-result-instance result)))
        (format t "~%  Final: ~S / ~S (retries: ~D)~%"
                (ticket-subject r) (ticket-priority r)
                (extraction-result-retries result))))))

;;; ── Demo 2: WITH-EXTRACTION-LOGGING ──────────────────────────────

(defun demo-logging-macro ()
  (banner "DEMO 2: The same run through WITH-EXTRACTION-LOGGING")
  (format t "Same scripted backend, but observed by the library's own logger.~2%")
  (let ((backend (make-mock-backend :responses (list (bad-data) (good-data)))))
    (with-extraction-logging (*standard-output*)
      (extract backend 'ticket "Users can't log in."))))

;;; ── Demo 3: ABORT-EXTRACTION ──────────────────────────────────────

(defun demo-abort-extraction ()
  (banner "DEMO 3: ABORT-EXTRACTION — circuit-break out early")
  (format t "Every scripted attempt is bad; a handler gives up after one retry~%")
  (format t "instead of burning all 5 configured retries.~2%")
  (let ((backend (make-mock-backend :responses (list (bad-data) (bad-data)))))
    (handler-bind ((extraction-retry
                      (lambda (e)
                        (format t "  [RETRY ~D] giving up — not worth 5 attempts on this input~%"
                                (extraction-event-attempt-number e))
                        (when (>= (extraction-event-attempt-number e) 1)
                          (invoke-restart 'abort-extraction)))))
      (handler-case
          (extract backend 'ticket "Users can't log in." :max-retries 5)
        (max-retries-error (err)
          (format t "~%  Gave up after ~D attempt(s) (~D retries) — configured max-retries was 5~%"
                  (1+ (max-retries-error-retries err))
                  (max-retries-error-retries err)))))))

;;; ── Demo 4: RETRY-WITH-BACKEND ────────────────────────────────────

(defun demo-retry-with-backend ()
  (banner "DEMO 4: RETRY-WITH-BACKEND — fail over to a healthier backend")
  (format t "The primary backend is unreliable; a handler fails over to a~%")
  (format t "fallback the moment the primary produces an invalid attempt.~2%")
  (let ((primary (make-mock-backend :responses (list (bad-data))))
        (fallback (make-mock-backend :responses (list (good-data)))))
    (handler-bind ((extraction-retry
                      (lambda (e)
                        (declare (ignore e))
                        (format t "  [RETRY] primary produced invalid output — failing over~%")
                        (invoke-restart 'retry-with-backend fallback))))
      (let* ((result (extract primary 'ticket "Users can't log in." :max-retries 3))
             (r (extraction-result-instance result)))
        (format t "~%  Final: ~S / ~S~%" (ticket-subject r) (ticket-priority r))
        (format t "  primary calls: ~D, fallback calls: ~D~%"
                (cl-llm-backend:mock-backend-calls primary)
                (cl-llm-backend:mock-backend-calls fallback))))))

;;; ── Entry point ────────────────────────────────────────────────────

(defun run ()
  "Run all clos-alchemy/telemetry demos. No GGUF model required — every
backend call is scripted, so this is deterministic and runs instantly."
  (demo-raw-handler-bind)
  (demo-logging-macro)
  (demo-abort-extraction)
  (demo-retry-with-backend)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  All demos complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
