;;;; validate-llama.lisp
;;;;
;;;; Demonstrates custom validation predicates with clos-alchemy.
;;;; Defines a hotel booking class with per-slot predicates (:validate)
;;;; and a cross-field instance validator (validate-instance). When the
;;;; model extracts semantically invalid data — a negative guest count,
;;;; check-out before check-in — the validator catches it, feeds the
;;;; error back, and the retry produces a corrected extraction.
;;;;
;;;; The input text is deliberately adversarial: ambiguous phrasing that
;;;; small models frequently misparse into constraint-violating values.
;;;; Grammar-constrained decoding guarantees the JSON *shape* is correct,
;;;; so the retry loop can only fire on these semantic predicates.
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-alchemy/examples)
;;;;   (setf clos-alchemy/examples/validate-llama::*model-path*
;;;;         "/path/to/gemma-3-1b.gguf")
;;;;   (clos-alchemy/examples/validate-llama:run)

(defpackage #:clos-alchemy/examples/validate-llama
  (:use #:cl #:clos-alchemy)
  (:export #:run))

(in-package #:clos-alchemy/examples/validate-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Domain class with validators ─────────────────────────────────

(defclass hotel-booking ()
  ((guest-name   :initarg :guest-name   :accessor booking-guest-name
                 :type string
                 :documentation "Full name of the primary guest")
   (num-guests   :initarg :num-guests   :accessor booking-num-guests
                 :type integer
                 :validate (lambda (v) (if (plusp v) t "num_guests must be positive"))
                 :documentation "Number of guests in the party (must be > 0)")
   (num-nights   :initarg :num-nights   :accessor booking-num-nights
                 :type integer
                 :validate (lambda (v) (if (>= v 1) t "num_nights must be at least 1"))
                 :documentation "Duration of stay in nights (must be >= 1)")
   (check-in     :initarg :check-in     :accessor booking-check-in
                 :type string
                 :documentation "Check-in date in YYYY-MM-DD format")
   (check-out    :initarg :check-out    :accessor booking-check-out
                 :type string
                 :documentation "Check-out date in YYYY-MM-DD format")
   (room-type    :initarg :room-type    :accessor booking-room-type
                 :type (member :single :double :suite)
                 :documentation "Type of room requested"))
  (:metaclass constructor-class)
  (:documentation "A hotel booking with semantic constraints."))

(defun parse-date (date-string)
  "Parse YYYY-MM-DD to a universal time for arithmetic."
  (encode-universal-time
   0 0 0
   (parse-integer date-string :start 8 :end 10)
   (parse-integer date-string :start 5 :end 7)
   (parse-integer date-string :start 0 :end 4)
   0))

(defun date-diff-nights (start end)
  "Compute nights between two YYYY-MM-DD date strings."
  (round (- (parse-date end) (parse-date start)) 86400))

(defmethod validate-instance ((b hotel-booking))
  "Cross-field checks: date ordering and nights consistency."
  (let ((errors '()))
    (when (string<= (booking-check-out b) (booking-check-in b))
      (push (format nil "Dates are reversed. Swap them: set check_in to ~A and check_out to ~A"
                    (booking-check-out b) (booking-check-in b))
            errors))
    (when (and (string> (booking-check-out b) (booking-check-in b))
               (/= (booking-num-nights b)
                    (date-diff-nights (booking-check-in b)
                                      (booking-check-out b))))
      (push (format nil "num_nights (~D) does not match date span (~D nights: ~A to ~A)"
                    (booking-num-nights b)
                    (date-diff-nights (booking-check-in b)
                                      (booking-check-out b))
                    (booking-check-in b) (booking-check-out b))
            errors))
    errors))

;;; ── Adversarial inputs ───────────────────────────────────────────

(defvar *scenarios*
  '((:title "Negative guest count from ledger"
     :text "HOTEL LEDGER ENTRY — 2025-01-05
Guest: Matsuda Kenji | Room: double | Nights: 3
Guests: -2 (credit/debit ledger format)
Check-in: 2025-01-05 | Check-out: 2025-01-08")

    (:title "Reversed date fields"
     :text "Park Jisoo | single | 1 guest | 3 nights
check_in: 2025-03-23
check_out: 2025-03-20")

    (:title "Zero nights in record"
     :text "Okafor, Chidinma — suite, 2 guests
num_nights: 0
Dates: 2025-02-10 to 2025-02-14")))

;;; ── Demo runner ──────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

(defun run-scenario (backend compilation scenario)
  (let* ((title (getf scenario :title))
         (text (getf scenario :text)))
    (format t "~&── ~A ──~%" title)
    (format t "~%Input:~%~A~2%" text)
    (format t "Extracting...~%")
    (let* ((result (extract backend compilation text :max-retries 3))
           (b (extraction-result-instance result)))
      (format t "~%Result (~D retries):~%" (extraction-result-retries result))
      (format t "  guest-name: ~S~%" (booking-guest-name b))
      (format t "  num-guests: ~D~%" (booking-num-guests b))
      (format t "  num-nights: ~D~%" (booking-num-nights b))
      (format t "  check-in:   ~A~%" (booking-check-in b))
      (format t "  check-out:  ~A~%" (booking-check-out b))
      (format t "  room-type:  ~A~%" (booking-room-type b))
      (format t "  valid:      ~A~2%" (null (validate-instance b))))))

;;; ── Entry point ──────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096)
                 chat-template)
  "Run the semantic validation demo.
Uses adversarial text to provoke semantic extraction errors that per-slot
and instance validators catch. The retry feeds the error message back to
the model for correction.

Retry counts will vary per model — smaller models (gemma-3-1b) are more
likely to trip the validators on first attempt."
  (unless model-path
    (format t "Set *model-path* or export LLAMA_MODEL before calling run.~%")
    (return-from run (values)))

  (banner "Semantic Validation Demo")
  (format t "Loading model: ~A~%" model-path)
  (format t "~%Validators active:~%")
  (format t "  • num_guests must be positive~%")
  (format t "  • num_nights must be >= 1~%")
  (format t "  • check_out must be after check_in~%")
  (format t "  • num_nights must match date span~2%")

  (cl-llama-cpp:with-backend ()
    (cl-llama-cpp:set-log-callback
     (lambda (level text)
       (when (>= level 3)
         (format *error-output* "~a" text))))
    (cl-llama-cpp:with-model (model model-path :n-gpu-layers n-gpu-layers)
      (cl-llama-cpp:with-context (ctx model :n-ctx n-ctx)
        (let* ((backend (cl-llm-backend/llama:make-llama-backend
                         :model model :context ctx
                         :chat-template chat-template))
               (compilation (compile-extractor 'hotel-booking)))
          (dolist (scenario *scenarios*)
            (handler-case
                (run-scenario backend compilation scenario)
              (max-retries-error (e)
                (format t "~%  FAILED after ~D retries (model could not self-correct)~%"
                        (max-retries-error-retries e))
                (format t "  Last errors: ~{~A~^; ~}~2%"
                        (mapcar #'princ-to-string
                                (last (max-retries-error-errors e) 3))))))))))

  (format t "~&~A~%" (make-string 64 :initial-element #\═))
  (format t "  Demo complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
