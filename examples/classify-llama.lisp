;;;; classify-llama.lisp
;;;;
;;;; Zero-shot support ticket classifier using clos-constructor.
;;;; Define a CLOS class whose slots are enums — the grammar constraint
;;;; forces the LLM to pick from the allowed values, giving you a
;;;; structured classifier with no training data and no string matching.
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-constructor/examples)
;;;;   (setf clos-constructor/examples/classify-llama::*model-path*
;;;;         "/path/to/model.gguf")
;;;;   (clos-constructor/examples/classify-llama:run)

(defpackage #:clos-constructor/examples/classify-llama
  (:use #:cl #:clos-constructor)
  (:export #:run))

(in-package #:clos-constructor/examples/classify-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Classification schema ─────────────────────────────────────────

(defclass ticket-classification ()
  ((urgency  :initarg :urgency  :accessor ticket-urgency
             :type (member :low :medium :high :critical))
   (category :initarg :category :accessor ticket-category
             :type (member :billing :technical :account :general))
   (sentiment :initarg :sentiment :accessor ticket-sentiment
              :type (member :positive :negative :neutral))
   (reason   :initarg :reason   :accessor ticket-reason
             :type string))
  (:documentation "Structured classification of a support ticket."))

;;; ── Test tickets ──────────────────────────────────────────────────

(defvar *tickets*
  '("I've been charged twice for my subscription this month. This is
the third time this has happened and I'm really frustrated. I need a
refund immediately or I'm cancelling my account."

    "Hey, just wanted to say the new dashboard update is fantastic!
Everything loads so much faster now. Great work team!"

    "URGENT: Our entire team of 50 people cannot log in since this
morning. We have a critical demo with a client in 2 hours and
absolutely need this resolved NOW."

    "I'd like to change the email address associated with my account.
The current one is my old work email. Could you help me with that?"))

;;; ── Demo ──────────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

(defun classify-tickets (backend)
  (let ((compilation (compile-extractor 'ticket-classification)))
    (loop for ticket in *tickets*
          for i from 1
          do (format t "~&── Ticket ~D ──~%" i)
             (format t "  ~A~2%" (string-trim '(#\Newline #\Space) ticket))
             (let* ((result (extract backend compilation ticket))
                    (c (extraction-result-instance result)))
               (format t "  Urgency:   ~A~%" (ticket-urgency c))
               (format t "  Category:  ~A~%" (ticket-category c))
               (format t "  Sentiment: ~A~%" (ticket-sentiment c))
               (format t "  Reason:    ~S~%" (ticket-reason c))
               (format t "  Retries:   ~D~2%" (extraction-result-retries result))))))

;;; ── Entry point ───────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096))
  "Run the zero-shot ticket classifier demo."
  (unless model-path
    (format t "Set *model-path* or export LLAMA_MODEL before calling run.~%")
    (return-from run (values)))

  (banner "Zero-Shot Support Ticket Classifier")
  (format t "Loading model: ~A~%" model-path)

  (cl-llama-cpp:with-backend ()
    (cl-llama-cpp:set-log-callback
     (lambda (level text)
       (when (>= level 3)
         (format *error-output* "~a" text))))
    (cl-llama-cpp:with-model (model model-path :n-gpu-layers n-gpu-layers)
      (cl-llama-cpp:with-context (ctx model :n-ctx n-ctx)
        (let ((backend (clos-constructor/llama:make-llama-backend
                        :model model :context ctx)))
          (classify-tickets backend)))))

  (format t "~&~A~%" (make-string 64 :initial-element #\═))
  (format t "  Classification complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
