(defpackage #:clos-alchemy/tests/security
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/security)

;;; Test classes

(defclass person ()
  ((name :initarg :name :accessor person-name :type string)
   (age :initarg :age :accessor person-age :type integer)
   (score :initarg :score :accessor person-score :type number :initform 0)))

(defclass tagged ()
  ((label :initarg :label :accessor tagged-label :type string)
   (kind :initarg :kind :accessor tagged-kind
         :type (member :alpha :beta :gamma))))

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun number-schema ()
  (make-ir-schema
   :name "person"
   :class-name 'person
   :fields (list
            (make-ir-field :name "name"
                           :type (make-ir-type-primitive :kind :string)
                           :required-p t :slot-name 'name)
            (make-ir-field :name "age"
                           :type (make-ir-type-primitive :kind :integer)
                           :required-p t :slot-name 'age)
            (make-ir-field :name "score"
                           :type (make-ir-type-primitive :kind :number)
                           :required-p t :slot-name 'score))))

;;; ── Regression: read-from-string removal ──────────────────────────

(deftest number-coercion-rejects-read-eval
  (testing "#.(...) reader macro must not execute"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30
                           "score" "#.(+ 1 2)")))
      (ok (handler-case
              (progn (construct-from-data data schema) nil)
            (generation-error () t))))))

(deftest number-coercion-rejects-sharp-quote
  (testing "#'function reader macro must not execute"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30
                           "score" "#'identity")))
      (ok (handler-case
              (progn (construct-from-data data schema) nil)
            (generation-error () t))))))

(deftest number-coercion-rejects-sharp-colon
  (testing "#:uninterned reader macro must not execute"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30
                           "score" "#:foo")))
      (ok (handler-case
              (progn (construct-from-data data schema) nil)
            (generation-error () t))))))

(deftest number-coercion-rejects-package-prefix
  (testing "package-qualified symbols must not be interned"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30
                           "score" "cl-user::*standard-output*")))
      (ok (handler-case
              (progn (construct-from-data data schema) nil)
            (generation-error () t))))))

(deftest number-coercion-rejects-arbitrary-text
  (testing "plain text must not be parsed"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30
                           "score" "delete-everything")))
      (ok (handler-case
              (progn (construct-from-data data schema) nil)
            (generation-error () t))))))

(deftest number-coercion-rejects-empty-string
  (let ((schema (number-schema))
        (data (make-data "name" "Alice" "age" 30 "score" "")))
    (ok (handler-case
            (progn (construct-from-data data schema) nil)
          (generation-error () t)))))

;;; ── Regression: safe number parsing still works ────────────────────

(deftest safe-parse-integer-string
  (testing "string integers still coerce correctly"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30 "score" "99")))
      (let ((instance (construct-from-data data schema)))
        (ok (= 99 (person-score instance)))))))

(deftest safe-parse-negative-integer-string
  (let ((schema (number-schema))
        (data (make-data "name" "Alice" "age" 30 "score" "-7")))
    (let ((instance (construct-from-data data schema)))
      (ok (= -7 (person-score instance))))))

(deftest safe-parse-float-string
  (testing "string decimals coerce to double-float, not ratios"
    (let ((schema (number-schema))
          (data (make-data "name" "Alice" "age" 30 "score" "3.14")))
      (let ((instance (construct-from-data data schema)))
        (ok (floatp (person-score instance)))
        (ok (= 3.14d0 (person-score instance)))))))

(deftest safe-parse-negative-float-string
  (let ((schema (number-schema))
        (data (make-data "name" "Alice" "age" 30 "score" "-2.5")))
    (let ((instance (construct-from-data data schema)))
      (ok (floatp (person-score instance)))
      (ok (= -2.5d0 (person-score instance))))))

;;; ── Enum interning: validation gates construction ──────────────────

(deftest enum-validation-blocks-novel-keywords
  (testing "validation rejects enum values not in the schema before intern runs"
    (let* ((schema (make-ir-schema
                    :name "tagged"
                    :class-name 'tagged
                    :fields (list
                             (make-ir-field :name "label"
                                            :type (make-ir-type-primitive :kind :string)
                                            :required-p t :slot-name 'label)
                             (make-ir-field :name "kind"
                                            :type (make-ir-type-enum
                                                   :values '("alpha" "beta" "gamma"))
                                            :required-p t :slot-name 'kind))))
           (data (make-data "label" "test"
                            "kind" "injected-keyword-that-should-never-be-interned")))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (= 1 (length errors))))
      (ok (null (find-symbol "INJECTED-KEYWORD-THAT-SHOULD-NEVER-BE-INTERNED"
                             :keyword))))))

;;; ── JSON parser: no reader macro execution ─────────────────────────

(deftest json-parser-ignores-reader-macros-in-strings
  (testing "yason parses #.(...) as a literal string, not CL reader input"
    (let ((result (parse-json-response
                   "{\"value\": \"#.(delete-file \\\"/etc/passwd\\\")\"}")))
      (ok (stringp (gethash "value" result)))
      (ok (search "#." (gethash "value" result))))))

(deftest json-parser-ignores-reader-macros-in-keys
  (testing "reader macros in JSON keys are treated as literal strings"
    (let ((result (parse-json-response "{\"#.(evil)\": 42}")))
      (ok (= 42 (gethash "#.(evil)" result))))))

;;; ── make-instance: class name from schema, not data ────────────────

(deftest construction-ignores-type-field-in-data
  (testing "a __type or class field in LLM data cannot override the schema's class"
    (let* ((schema (make-ir-schema
                    :name "person"
                    :class-name 'person
                    :fields (list
                             (make-ir-field :name "name"
                                            :type (make-ir-type-primitive :kind :string)
                                            :required-p t :slot-name 'name)
                             (make-ir-field :name "age"
                                            :type (make-ir-type-primitive :kind :integer)
                                            :required-p t :slot-name 'age))))
           (data (make-data "name" "Alice" "age" 30
                            "__type" "some-dangerous-class"
                            "class" "another-dangerous-class"))
           (instance (construct-from-data data schema)))
      (ok (typep instance 'person))
      (ok (string= "Alice" (person-name instance))))))
