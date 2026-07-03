(defsystem "clos-constructor"
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
                 (:file "validation")
                 (:file "construction")
                 (:file "prompt")
                 (:file "json-parse")
                 (:file "extract")))))

(defsystem "clos-constructor/tests"
  :description "Tests for clos-constructor"
  :depends-on ("clos-constructor" "rove")
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
                 (:file "unsupported-types"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "clos-constructor/examples"
  :description "Example programs for clos-constructor"
  :depends-on ("clos-constructor" "cl-llm-backend/llama")
  :components ((:module "examples"
                :components
                ((:file "extract-llama")
                 (:file "classify-llama")))))
