;;;; value-llama.lisp
;;;;
;;;; Demonstrates extract-value for extracting bare primitives and
;;;; unwrapped values without defining wrapper classes. Covers:
;;;;   1. Integer — extract a single number
;;;;   2. Boolean — yes/no question answering
;;;;   3. Enum — sentiment classification via (member ...)
;;;;   4. String list — extract keywords via (vector string)
;;;;   5. Constrained integer — extract with type bounds
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-alchemy/examples)
;;;;   (setf clos-alchemy/examples/value-llama::*model-path*
;;;;         "/path/to/model.gguf")
;;;;   (clos-alchemy/examples/value-llama:run)

(defpackage #:clos-alchemy/examples/value-llama
  (:use #:cl #:clos-alchemy)
  (:export #:run))

(in-package #:clos-alchemy/examples/value-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Helpers ───────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

;;; ── Demo 1: Extract an integer ────────────────────────────────────

(defun demo-integer (backend)
  (banner "DEMO 1: Extract Integer")

  (let ((text "The Eiffel Tower was completed in 1889 for the World's Fair
in Paris. It stands 330 metres tall including its antenna."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting an INTEGER...~2%")
    (let ((result (extract-value backend 'integer text
                                 :user-prompt "Extract the height in metres.")))
      (format t "  Value:   ~D~%" (extraction-result-instance result))
      (format t "  Type:    ~A~%" (type-of (extraction-result-instance result)))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Demo 2: Boolean extraction ─────────────────────────────────────

(defun demo-boolean (backend)
  (banner "DEMO 2: Boolean Extraction")

  (let ((text "The company announced today that its proposed merger with Acme
Corp will not proceed. Regulatory authorities raised concerns about
market concentration, and both boards agreed to terminate the deal."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting a BOOLEAN...~2%")
    (let ((result (extract-value backend 'boolean text
                                 :user-prompt "Did the merger complete?")))
      (format t "  Value:   ~A~%" (extraction-result-instance result))
      (format t "  Boolean? ~A~%" (typep (extraction-result-instance result) 'boolean))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Demo 3: Enum classification ───────────────────────────────────

(defun demo-enum (backend)
  (banner "DEMO 3: Enum Classification")

  (let ((text "I've been waiting three weeks for my order and customer support
keeps giving me the runaround. Absolutely unacceptable."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting a (MEMBER :POSITIVE :NEGATIVE :NEUTRAL)...~2%")
    (let ((result (extract-value backend '(member :positive :negative :neutral) text
                                 :user-prompt "What is the sentiment of this text?")))
      (format t "  Value:    ~S~%" (extraction-result-instance result))
      (format t "  Keyword?: ~A~%" (keywordp (extraction-result-instance result)))
      (format t "  Retries:  ~D~%" (extraction-result-retries result)))))

;;; ── Demo 4: List of strings ──────────────────────────────────────

(defun demo-keywords (backend)
  (banner "DEMO 4: Keyword Extraction (vector of strings)")

  (let ((text "Common Lisp is a multi-paradigm programming language known for
its powerful macro system, dynamic typing, interactive development via
REPL, and the CLOS object system with multiple dispatch."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting a (VECTOR STRING)...~2%")
    (let* ((result (extract-value backend '(vector string) text
                                  :user-prompt "Extract the key technical features mentioned."))
           (kws (extraction-result-instance result)))
      (format t "  Value:   ~S~%" (coerce kws 'list))
      (format t "  Type:    ~A~%" (type-of kws))
      (format t "  Count:   ~D~%" (length kws))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Demo 5: Constrained integer ──────────────────────────────────

(defun demo-constrained (backend)
  (banner "DEMO 5: Constrained Integer (1-5 star rating)")

  (let ((text "I tried the new Thai place on Oak Street last night. The critic
from the Herald gave it 4 out of 5 stars, praising the green curry but
noting that dessert options were limited."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting an (INTEGER 1 5)...~2%")
    (let ((result (extract-value backend '(integer 1 5) text
                                 :user-prompt "Extract the star rating the critic gave.")))
      (format t "  Value:   ~D~%" (extraction-result-instance result))
      (format t "  Type:    ~A~%" (type-of (extraction-result-instance result)))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Entry point ──────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096)
                 chat-template)
  "Run all extract-value demos.
CHAT-TEMPLATE overrides the model's embedded chat template — pass a llama.cpp
built-in name (e.g. \"gemma\") when the embedded template is unsupported."
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
          (demo-integer backend)
          (demo-boolean backend)
          (demo-enum backend)
          (demo-keywords backend)
          (demo-constrained backend)))))

  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  All demos complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
