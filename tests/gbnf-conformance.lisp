(defpackage #:clos-alchemy/tests/gbnf-conformance
  (:use #:cl #:rove #:clos-alchemy)
  (:import-from #:cl-llama-cpp-extras/json-schema #:json-schema-to-grammar))

(in-package #:clos-alchemy/tests/gbnf-conformance)

;;;; Gated: requires cl-llama-cpp-extras, which needs llama.cpp built as a
;;;; shared library. Kept out of the core test system for that reason — see
;;;; README.md:68-85. Load via the clos-alchemy/tests/gbnf system.
;;;;
;;;; json-schema-to-grammar is a pure function over the schema: no model is
;;;; loaded and no inference runs, so this is fast and deterministic.
;;;;
;;;; What this catches: an emitted schema that OpenAI accepts but llama.cpp
;;;; cannot compile to a grammar. The two consumers have different rules, and
;;;; satisfying one has already been shown capable of breaking the other.

(defclass optional-slot ()
  ((reason :type string :initarg :reason)
   (nick   :type string :initarg :nick :initform "none")))

(defclass address ()
  ((street :type string :initarg :street)
   (city   :type string :initarg :city)))

(defclass employee ()
  ((name    :type string :initarg :name)
   (address :type address :initarg :address)))

(defclass enum-holder ()
  ((urgency :type (member :low :high) :initarg :urgency)))

(defclass nullable-holder ()
  ((email :type (or null string) :initarg :email)))

(defclass listy ()
  ((tags :initarg :tags :list-of string))
  (:metaclass constructor-class))

(defun compiles-to-grammar-p (class-name)
  "True when the schema emitted for CLASS-NAME converts to a GBNF grammar."
  (let ((grammar (json-schema-to-grammar
                  (schema-to-json-schema (class-to-schema class-name)))))
    (and (stringp grammar) (plusp (length grammar)))))

(deftest gbnf/optional-slot-class
  (ok (compiles-to-grammar-p 'optional-slot)))

(deftest gbnf/nested-class
  (ok (compiles-to-grammar-p 'employee)))

(deftest gbnf/enum-class
  (ok (compiles-to-grammar-p 'enum-holder)))

(deftest gbnf/nullable-class
  (ok (compiles-to-grammar-p 'nullable-holder)))

(deftest gbnf/list-of-class
  (ok (compiles-to-grammar-p 'listy)))

(deftest gbnf/optional-field-keeps-its-place-in-generation-order
  (testing "the grammar's root rule must still emit fields in slot order —
the chain-of-thought ordering documented at README:300"
    (let ((grammar (json-schema-to-grammar
                    (schema-to-json-schema (class-to-schema 'optional-slot)))))
      (ok (< (search "reason-kv" grammar) (search "nick-kv" grammar))))))

;;; Date types (#12)

(defclass dated-event ()
  ((title :type string :initarg :title)
   (event-date :type date :initarg :event-date)))

(deftest gbnf/date-class
  (ok (compiles-to-grammar-p 'dated-event)))

(defclass timestamped-event ()
  ((title :type string :initarg :title)
   (ts :type date-time :initarg :ts)))

(deftest gbnf/date-time-class
  (ok (compiles-to-grammar-p 'timestamped-event)))

;;; Cyclic schemas (#1)

(defclass tree-node ()
  ((label :type string :initarg :label)
   (child :type tree-node :initarg :child :initform nil)))

(deftest gbnf/self-referencing-class
  (ok (compiles-to-grammar-p 'tree-node)))
