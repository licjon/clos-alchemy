;;;; union-llama.lisp
;;;;
;;;; Demonstrates discriminated unions and free-form maps with clos-alchemy.
;;;;
;;;; Scenario: given a product review, extract structured feedback. The model
;;;; must decide whether the review is actionable (with per-category scores
;;;; in a free-form map) or not actionable (with a reason). A discriminated
;;;; union constrains the model to pick exactly one shape.
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-alchemy/examples)
;;;;   (setf clos-alchemy/examples/union-llama::*model-path*
;;;;         "/path/to/model.gguf")
;;;;   (clos-alchemy/examples/union-llama:run)

(defpackage #:clos-alchemy/examples/union-llama
  (:use #:cl #:clos-alchemy)
  (:export #:run))

(in-package #:clos-alchemy/examples/union-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Domain classes ─────────────────────────────────────────────────
;;;
;;; Two shapes share a `kind` discriminator slot.  Each shape's `kind`
;;; is a single-value (member ...) — clos-alchemy reads that to wire up
;;; the anyOf schema and discriminator-based construction.

(defclass actionable-feedback ()
  ((kind    :initarg :kind    :type (member :actionable)
            :documentation "Discriminator — always \"actionable\"")
   (summary :initarg :summary :type string
            :documentation "One-sentence summary of the feedback")
   (scores  :initarg :scores  :type hash-table :map-of integer :initform nil
            :documentation "Per-category quality scores, 1-10")
   (tags    :initarg :tags    :type list
            :documentation "Freeform topic tags extracted from the review"))
  (:metaclass constructor-class)
  (:documentation "Actionable product feedback with per-category scores.
The :map-of slot option on `scores` produces a JSON Schema with
additionalProperties — the model invents the category keys."))

(defclass not-actionable ()
  ((kind   :initarg :kind   :type (member :not_actionable)
           :documentation "Discriminator — always \"not_actionable\"")
   (reason :initarg :reason :type string
           :documentation "Why this review does not contain actionable feedback"))
  (:documentation "A review that doesn't contain actionable product feedback."))

;;; ── Accessors ──────────────────────────────────────────────────────

(defgeneric feedback-kind (obj)
  (:method ((obj actionable-feedback)) (slot-value obj 'kind))
  (:method ((obj not-actionable))      (slot-value obj 'kind)))

;;; ── Test reviews ───────────────────────────────────────────────────

(defvar *reviews*
  '("I've been using the wireless headphones for about a month now. The
sound quality is excellent — clear mids and solid bass. Noise cancellation
works well on planes. My only complaints: the ear cushions get warm after
an hour, and the Bluetooth range drops noticeably past 15 feet. Battery
life is stellar though, easily lasting two full workdays."

    "lol nice product 👍"

    "Returned the standing desk after two weeks. The motor is incredibly
loud — my coworkers on Zoom calls kept asking what the noise was. The
wobble at max height makes it unusable for writing. Build quality of
the frame feels cheap for the price point. Cable management tray is
a nice touch but doesn't make up for the fundamental issues. Customer
service was responsive at least."))

;;; ── Helpers ────────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

(defun print-scores (scores)
  (maphash (lambda (k v)
             (format t "      ~A: ~D/10~%" k v))
           scores))

(defun print-result (instance)
  (etypecase instance
    (actionable-feedback
     (format t "  → ACTIONABLE~%")
     (format t "    Summary: ~S~%" (slot-value instance 'summary))
     (format t "    Scores:~%")
     (print-scores (slot-value instance 'scores))
     (format t "    Tags: ~{~S~^, ~}~%" (slot-value instance 'tags)))
    (not-actionable
     (format t "  → NOT ACTIONABLE~%")
     (format t "    Reason: ~S~%" (slot-value instance 'reason)))))

;;; ── Demo ───────────────────────────────────────────────────────────

(defun analyze-reviews (backend)
  (let ((compilation (compile-union-extractor
                      '(actionable-feedback not-actionable)
                      :discriminator 'kind
                      :user-prompt "Classify the product review. If it contains
specific, actionable feedback about product qualities, extract it as
actionable with per-category scores (1-10) and topic tags. If the review
is too vague, off-topic, or contains no substantive feedback, mark it as
not actionable.")))
    (loop for review in *reviews*
          for i from 1
          do (format t "~&── Review ~D ──~%" i)
             (format t "  ~A~2%"
                     (subseq (string-trim '(#\Newline #\Space) review)
                             0 (min 72 (length (string-trim '(#\Newline #\Space) review)))))
             (let* ((result (extract-union backend compilation review
                                          :max-tokens 512))
                    (instance (extraction-result-instance result)))
               (print-result instance)
               (format t "  Kind:    ~S  (keywordp: ~A)~%"
                       (feedback-kind instance)
                       (keywordp (feedback-kind instance)))
               (format t "  Type:    ~A~%" (type-of instance))
               (format t "  Retries: ~D~2%" (extraction-result-retries result))))))

;;; ── Entry point ────────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096)
                 chat-template)
  "Run the discriminated union + map demo.
CHAT-TEMPLATE overrides the model's embedded chat template — pass a llama.cpp
built-in name (e.g. \"gemma\") when the embedded template is unsupported."
  (unless model-path
    (format t "Set *model-path* or export LLAMA_MODEL before calling run.~%")
    (return-from run (values)))

  (banner "Discriminated Union: Product Review Analyzer")
  (format t "Loading model: ~A~%" model-path)

  (cl-llama-cpp:with-backend ()
    (cl-llama-cpp:set-log-callback
     (lambda (level text)
       (when (>= level 3)
         (format *error-output* "~a" text))))
    (cl-llama-cpp:with-model (model model-path :n-gpu-layers n-gpu-layers)
      (cl-llama-cpp:with-context (ctx model :n-ctx n-ctx)
        (let ((backend (cl-llm-backend/llama:make-llama-backend
                        :model model :context ctx
                        :chat-template chat-template)))
          (analyze-reviews backend)))))

  (format t "~&~A~%" (make-string 64 :initial-element #\═))
  (format t "  Analysis complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
