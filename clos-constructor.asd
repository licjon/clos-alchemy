(defsystem "clos-constructor"
  :version "0.1.0"
  :author "Jonathan Hustad"
  :license "MIT"
  :description "Extract structured data from text via LLMs, constructing typed CLOS instances."
  :depends-on ("closer-mop" "yason")
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
                 (:file "backend-protocol")
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
  :depends-on ("clos-constructor/llama")
  :components ((:module "examples"
                :components
                ((:file "extract-llama")
                 (:file "classify-llama")))))

(defsystem "clos-constructor/llama"
  :description "llama.cpp backend for clos-constructor using grammar-constrained generation"
  :depends-on ("clos-constructor"
               "cl-llama-cpp"
               "cl-llama-cpp-extras/json-schema")
  :components ((:module "src"
                :components
                ((:file "backend-llama")))))

(defsystem "clos-constructor/llama/tests"
  :description "Tests for the llama.cpp backend"
  :depends-on ("clos-constructor/llama" "rove")
  :components ((:module "tests"
                :components
                ((:file "backend-llama"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
