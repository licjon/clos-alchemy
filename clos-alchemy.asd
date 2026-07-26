(defsystem "clos-alchemy"
  :version "0.1.0"
  :author "Jonathan Hustad"
  :license "MIT"
  :description "Extract structured data from text via LLMs, constructing typed CLOS instances."
  :depends-on ("closer-mop" "yason" "cl-llm-backend")
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
                 (:file "conditions")
                 (:file "ir")
                 (:file "type-mapping")
                 (:file "metaclass")
                 (:file "introspection")
                 (:file "json-schema")
                 (:file "construction")
                 (:file "validation")
                 (:file "prompt")
                 (:file "json-parse")
                 (:file "extract")))))

(defsystem "clos-alchemy/tests"
  :description "Tests for clos-alchemy"
  :depends-on ("clos-alchemy" "rove")
  :components ((:module "tests"
                :components
                ((:file "ir")
                 (:file "type-mapping")
                 (:file "metaclass")
                 (:file "introspection")
                 (:file "json-schema")
                 (:file "validation")
                 (:file "construction")
                 (:file "prompt")
                 (:file "json-parse")
                 (:file "extract")
                 (:file "security")
                 (:file "unsupported-types")
                 (:file "schema-conformance")
                 (:file "custom-validation"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

;;; Gated: needs llama.cpp built as a shared library, so it stays out of the
;;; core test system. Asserts emitted schemas compile to GBNF grammars.
(defsystem "clos-alchemy/tests/gbnf"
  :description "GBNF conformance tests for clos-alchemy (requires cl-llama-cpp-extras)"
  :depends-on ("clos-alchemy" "cl-llama-cpp-extras/json-schema" "rove")
  :components ((:module "tests"
                :components
                ((:file "gbnf-conformance"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "clos-alchemy/examples"
  :description "Example programs for clos-alchemy"
  :depends-on ("clos-alchemy" "cl-llm-backend/llama")
  :components ((:module "examples"
                :components
                ((:file "extract-llama")
                 (:file "classify-llama")
                 (:file "validate-llama")))))
