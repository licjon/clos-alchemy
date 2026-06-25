(defpackage #:clos-constructor/llama
  (:use #:cl #:clos-constructor)
  (:export
   #:llama-backend
   #:llama-backend-p
   #:llama-backend-model
   #:llama-backend-context
   #:make-llama-backend))

(in-package #:clos-constructor/llama)

(defstruct (llama-backend (:copier nil))
  (model nil)
  (context nil))

(defun %schema-to-gbnf (schema)
  "Convert an IR schema to a GBNF grammar string via JSON Schema."
  (cl-llama-cpp-extras/json-schema:json-schema-to-grammar
   (schema-to-json-schema schema)))

(defun %build-prompt (backend system-prompt document user-prompt)
  "Build a chat-formatted prompt string, falling back to raw concatenation."
  (let* ((model (llama-backend-model backend))
         (has-template (ignore-errors (cl-llama-cpp:model-chat-template model)))
         (messages (list (list :role "system" :content system-prompt)
                         (list :role "user"
                               :content (if user-prompt
                                             (format nil "~A~%~%~A" user-prompt document)
                                             document)))))
    (if has-template
        (cl-llama-cpp:format-chat model messages)
        (format nil "~A~%~%~A" system-prompt document))))

(defmethod backend-output-schema ((backend llama-backend) schema)
  (%schema-to-gbnf schema))

(defmethod backend-generate ((backend llama-backend) schema document
                             &key model temperature max-tokens
                                  system-prompt user-prompt)
  (declare (ignore model))
  (let* ((ctx (llama-backend-context backend))
         (grammar (%schema-to-gbnf schema))
         (prompt (%build-prompt backend system-prompt document user-prompt))
         (raw-response (cl-llama-cpp:generate ctx prompt
                                              :grammar grammar
                                              :max-tokens (or max-tokens 1024)
                                              :temp (or temperature 0.0)))
         (raw-data (parse-json-response raw-response)))
    (make-extraction-result :raw-response raw-response
                            :raw-data raw-data)))
