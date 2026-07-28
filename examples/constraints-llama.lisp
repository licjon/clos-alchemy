;;;; constraints-llama.lisp
;;;;
;;;; Demonstrates JSON Schema constraint emission from CL type specifiers
;;;; and metaclass slot options. The constraints are emitted into the
;;;; JSON Schema sent to the LLM, so the model is constrained during
;;;; sampling — and the same constraints are enforced by the validator
;;;; so violations are caught even when the backend can't enforce them.
;;;;
;;;; Three demos:
;;;;   1. Type-inferred constraints — CL type specifiers like (integer 0 100)
;;;;      automatically emit JSON Schema bounds
;;;;   2. Slot option constraints — :min-length, :pattern, :min-items etc.
;;;;      for constraints CL types can't express
;;;;   3. Combined — inferred bounds overridden by explicit slot options,
;;;;      plus string and array constraints on a single class
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-alchemy/examples)
;;;;   (setf clos-alchemy/examples/constraints-llama::*model-path*
;;;;         "/path/to/model.gguf")
;;;;   (clos-alchemy/examples/constraints-llama:run)

(defpackage #:clos-alchemy/examples/constraints-llama
  (:use #:cl #:clos-alchemy)
  (:export #:run))

(in-package #:clos-alchemy/examples/constraints-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Demo 1: Type-inferred constraints ────────────────────────────

(defclass test-score ()
  ((student-name :initarg :student-name :accessor score-student-name
                 :type string
                 :documentation "Student's full name")
   (score        :initarg :score        :accessor score-value
                 :type (integer 0 100)
                 :documentation "Test score as a percentage (0-100)")
   (grade-point  :initarg :grade-point  :accessor score-grade-point
                 :type (float 0.0 4.0)
                 :documentation "GPA equivalent (0.0-4.0)"))
  (:documentation "A test result with type-inferred numeric bounds."))

;;; ── Demo 2: Slot option constraints ──────────────────────────────

(defclass user-profile ()
  ((username  :initarg :username  :accessor profile-username
              :type string
              :min-length 3 :max-length 20
              :pattern "^[a-zA-Z][a-zA-Z0-9_]*$"
              :documentation "Username: 3-20 chars, starts with letter, alphanumeric + underscore")
   (bio       :initarg :bio       :accessor profile-bio
              :type string
              :max-length 140
              :documentation "Short bio, max 140 characters")
   (interests :initarg :interests :accessor profile-interests
              :type list :list-of string
              :min-items 1 :max-items 5
              :documentation "1-5 interests or hobbies"))
  (:metaclass constructor-class)
  (:documentation "A user profile with string and array constraints via slot options."))

;;; ── Demo 3: Combined constraints ─────────────────────────────────

(defclass product-review ()
  ((product-name :initarg :product-name :accessor review-product-name
                 :type string
                 :min-length 1 :max-length 100
                 :documentation "Name of the reviewed product")
   (rating       :initarg :rating       :accessor review-rating
                 :type (integer 1 10)
                 :minimum 1 :maximum 5
                 :documentation "Rating 1-5 (slot options narrow the type's 1-10 range)")
   (pros         :initarg :pros         :accessor review-pros
                 :type list :list-of string
                 :min-items 1 :max-items 3
                 :documentation "1-3 positive points")
   (cons-list    :initarg :cons-list    :accessor review-cons
                 :type list :list-of string
                 :max-items 3
                 :documentation "Up to 3 negative points")
   (summary      :initarg :summary      :accessor review-summary
                 :type (string 200)
                 :documentation "Summary, max 200 characters (from CL type specifier)"))
  (:metaclass constructor-class)
  (:documentation "A product review combining type-inferred and slot option constraints."))

;;; ── Helpers ──────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\=))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\=)))

(defun print-json-schema (class-name)
  "Print the JSON Schema emitted for a class, showing constraint keywords."
  (let* ((schema (class-to-schema class-name))
         (js (schema-to-json-schema schema))
         (json (with-output-to-string (s)
                 (yason:encode js s))))
    (format t "JSON Schema:~%~A~2%" json)))

;;; ── Demo 1 ──────────────────────────────────────────────────────

(defun demo-type-inferred (backend)
  (banner "DEMO 1: Type-Inferred Constraints")

  (format t "Class TEST-SCORE has CL type specifiers:~%")
  (format t "  score      : (integer 0 100)  -> minimum: 0, maximum: 100~%")
  (format t "  grade-point: (float 0.0 4.0)  -> minimum: 0.0, maximum: 4.0~2%")
  (print-json-schema 'test-score)

  (let ((text "Alice scored 95% on her chemistry final. Her GPA for the course
is 3.8 out of 4.0."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting...~2%")
    (let* ((result (extract backend 'test-score text))
           (s (extraction-result-instance result)))
      (format t "Result (~D retries):~%" (extraction-result-retries result))
      (format t "  student-name: ~S~%" (score-student-name s))
      (format t "  score:        ~D (in [0,100]: ~A)~%"
              (score-value s) (<= 0 (score-value s) 100))
      (format t "  grade-point:  ~F (in [0.0,4.0]: ~A)~%"
              (score-grade-point s) (<= 0.0 (score-grade-point s) 4.0)))))

;;; ── Demo 2 ──────────────────────────────────────────────────────

(defun demo-slot-options (backend)
  (banner "DEMO 2: Slot Option Constraints")

  (format t "Class USER-PROFILE has metaclass slot options:~%")
  (format t "  username:  min-length 3, max-length 20, pattern ^[a-zA-Z][a-zA-Z0-9_]*$~%")
  (format t "  bio:       max-length 140~%")
  (format t "  interests: min-items 1, max-items 5~2%")
  (print-json-schema 'user-profile)

  (let ((text "My name is Bob Park and I go by bob_park99 online. I'm a software
developer who loves hiking, cooking, reading sci-fi novels, and playing
guitar. My bio: Coder by day, chef by night."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting...~2%")
    (let* ((result (extract backend 'user-profile text))
           (p (extraction-result-instance result)))
      (format t "Result (~D retries):~%" (extraction-result-retries result))
      (format t "  username:  ~S (length ~D, in [3,20]: ~A)~%"
              (profile-username p)
              (length (profile-username p))
              (<= 3 (length (profile-username p)) 20))
      (format t "  bio:       ~S (length ~D, <= 140: ~A)~%"
              (profile-bio p)
              (length (profile-bio p))
              (<= (length (profile-bio p)) 140))
      (format t "  interests: ~S (~D items, in [1,5]: ~A)~%"
              (profile-interests p)
              (length (profile-interests p))
              (<= 1 (length (profile-interests p)) 5)))))

;;; ── Demo 3 ──────────────────────────────────────────────────────

(defun demo-combined (backend)
  (banner "DEMO 3: Combined Constraints")

  (format t "Class PRODUCT-REVIEW combines inferred + slot option constraints:~%")
  (format t "  rating:  type (integer 1 10) narrowed by :minimum 1 :maximum 5~%")
  (format t "  pros:    :min-items 1 :max-items 3~%")
  (format t "  summary: type (string 200) -> maxLength 200~2%")
  (print-json-schema 'product-review)

  (let ((text "I bought the Acme Blender Pro. I'd give it a 4 out of 5. Pros: it's
very powerful, easy to clean, and looks great on the counter. Cons: a
bit loud. Summary: A solid kitchen workhorse with minor noise issues."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting...~2%")
    (let* ((result (extract backend 'product-review text))
           (r (extraction-result-instance result)))
      (format t "Result (~D retries):~%" (extraction-result-retries result))
      (format t "  product-name: ~S~%" (review-product-name r))
      (format t "  rating:       ~D (in [1,5]: ~A)~%"
              (review-rating r) (<= 1 (review-rating r) 5))
      (format t "  pros:         ~S (~D items)~%"
              (review-pros r) (length (review-pros r)))
      (format t "  cons:         ~S (~D items)~%"
              (review-cons r) (length (review-cons r)))
      (format t "  summary:      ~S (length ~D, <= 200: ~A)~%"
              (review-summary r)
              (length (review-summary r))
              (<= (length (review-summary r)) 200)))))

;;; ── Entry point ─────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096)
                 chat-template)
  "Run all constraint demos.
Shows how CL type specifiers and metaclass slot options emit JSON Schema
constraints that the model respects during sampling."
  (unless model-path
    (format t "Set *model-path* or export LLAMA_MODEL before calling run.~%")
    (return-from run (values)))

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
          (demo-type-inferred backend)
          (demo-slot-options backend)
          (demo-combined backend)))))

  (format t "~&~%~A~%" (make-string 64 :initial-element #\=))
  (format t "  All constraint demos complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\=))
  (values))
