(defpackage #:clos-alchemy/tests/type-mapping
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/type-mapping)

;;; Primitive type mappings

(deftest string-type
  (let ((ir (cl-type-to-ir-type 'string)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :string (ir-type-primitive-kind ir)))))

(deftest integer-type
  (let ((ir (cl-type-to-ir-type 'integer)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :integer (ir-type-primitive-kind ir)))))

(deftest fixnum-type
  (let ((ir (cl-type-to-ir-type 'fixnum)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :integer (ir-type-primitive-kind ir)))))

(deftest float-type
  (let ((ir (cl-type-to-ir-type 'float)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :number (ir-type-primitive-kind ir)))))

(deftest number-type
  (let ((ir (cl-type-to-ir-type 'number)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :number (ir-type-primitive-kind ir)))))

(deftest boolean-type
  (let ((ir (cl-type-to-ir-type 'boolean)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :boolean (ir-type-primitive-kind ir)))))

;;; T and NIL signal schema-error (no silent fallback to string)

(deftest t-signals-schema-error
  (ok (handler-case
          (progn (cl-type-to-ir-type t) nil)
        (schema-error () t))))

(deftest nil-signals-schema-error
  (ok (handler-case
          (progn (cl-type-to-ir-type nil) nil)
        (schema-error () t))))

;;; Enum types

(deftest keyword-enum
  (let ((ir (cl-type-to-ir-type '(member :pending :active :done))))
    (ok (ir-type-enum-p ir))
    (ok (equal '("pending" "active" "done") (ir-type-enum-values ir)))))

(deftest string-enum
  (let ((ir (cl-type-to-ir-type '(member "red" "green" "blue"))))
    (ok (ir-type-enum-p ir))
    (ok (equal '("red" "green" "blue") (ir-type-enum-values ir)))))

;;; Nullable types

(deftest or-null-string
  (let ((ir (cl-type-to-ir-type '(or null string))))
    (ok (ir-type-nullable-p ir))
    (ok (ir-type-primitive-p (ir-type-nullable-inner-type ir)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-nullable-inner-type ir))))))

(deftest or-string-null
  (let ((ir (cl-type-to-ir-type '(or string null))))
    (ok (ir-type-nullable-p ir))
    (ok (ir-type-primitive-p (ir-type-nullable-inner-type ir)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-nullable-inner-type ir))))))

(deftest or-null-integer
  (let ((ir (cl-type-to-ir-type '(or null integer))))
    (ok (ir-type-nullable-p ir))
    (ok (eq :integer (ir-type-primitive-kind (ir-type-nullable-inner-type ir))))))

;;; Nullable boolean — unrepresentable in CL, must signal schema-error

(deftest or-null-boolean-signals-schema-error
  (testing "(or null boolean) signals schema-error because CL NIL conflates false and null"
    (ok (handler-case
            (progn (cl-type-to-ir-type '(or null boolean)) nil)
          (schema-error () t)))))

(deftest or-boolean-null-signals-schema-error
  (testing "(or boolean null) — reversed order — also signals schema-error"
    (ok (handler-case
            (progn (cl-type-to-ir-type '(or boolean null)) nil)
          (schema-error () t)))))

;;; List types

(deftest plain-list
  (let ((ir (cl-type-to-ir-type 'list)))
    (ok (ir-type-list-p ir))
    (ok (ir-type-primitive-p (ir-type-list-element-type ir)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type ir))))))

;;; CLOS class references

(defclass test-address ()
  ((street :initarg :street :type string)
   (city :initarg :city :type string)))

(deftest class-reference
  (let ((ir (cl-type-to-ir-type 'test-address)))
    (ok (ir-type-object-p ir))
    (ok (ir-schema-p (ir-type-object-schema ir)))
    (ok (eq 'test-address (ir-schema-class-name (ir-type-object-schema ir))))))

;;; General union types signal schema-error (issue #6)

(deftest or-multi-member-signals-schema-error
  (testing "(or string integer) — general union, not nullable"
    (ok (handler-case
            (progn (cl-type-to-ir-type '(or string integer)) nil)
          (schema-error () t)))))

(deftest or-multi-member-with-null-signals-schema-error
  (testing "(or null string integer) — nullable but still multi-member"
    (ok (handler-case
            (progn (cl-type-to-ir-type '(or null string integer)) nil)
          (schema-error () t)))))

(deftest or-null-single-member-still-works
  (testing "(or null string) — genuine nullable, not a general union"
    (let ((ir (cl-type-to-ir-type '(or null string))))
      (ok (ir-type-nullable-p ir))
      (ok (eq :string (ir-type-primitive-kind (ir-type-nullable-inner-type ir)))))))

;;; Date types

(deftest date-type-mapping
  (let ((ir (cl-type-to-ir-type 'date)))
    (ok (ir-type-date-p ir))
    (ok (eq :date (ir-type-date-format ir)))))

(deftest date-time-type-mapping
  (let ((ir (cl-type-to-ir-type 'date-time)))
    (ok (ir-type-date-p ir))
    (ok (eq :date-time (ir-type-date-format ir)))))

(deftest or-null-date
  (let ((ir (cl-type-to-ir-type '(or null date))))
    (ok (ir-type-nullable-p ir))
    (ok (ir-type-date-p (ir-type-nullable-inner-type ir)))
    (ok (eq :date (ir-type-date-format (ir-type-nullable-inner-type ir))))))

(deftest or-null-date-time
  (let ((ir (cl-type-to-ir-type '(or null date-time))))
    (ok (ir-type-nullable-p ir))
    (ok (ir-type-date-p (ir-type-nullable-inner-type ir)))
    (ok (eq :date-time (ir-type-date-format (ir-type-nullable-inner-type ir))))))

;;; Unsupported types signal schema-error

(deftest unsupported-type-signals-error
  (ok (handler-case
          (progn (cl-type-to-ir-type 'nonexistent-class-xyz) nil)
        (schema-error () t))))
