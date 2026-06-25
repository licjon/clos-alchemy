(defpackage #:clos-constructor/tests/type-mapping
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/tests/type-mapping)

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

;;; T and absent default to string

(deftest t-defaults-to-string
  (let ((ir (cl-type-to-ir-type t)))
    (ok (ir-type-primitive-p ir))
    (ok (eq :string (ir-type-primitive-kind ir)))))

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

;;; List types

(deftest plain-list
  (let ((ir (cl-type-to-ir-type 'list)))
    (ok (ir-type-list-p ir))
    (ok (ir-type-primitive-p (ir-type-list-element-type ir)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type ir))))))

(deftest list-of-strings
  (let ((ir (cl-type-to-ir-type '(list-of string))))
    (ok (ir-type-list-p ir))
    (ok (ir-type-primitive-p (ir-type-list-element-type ir)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type ir))))))

(deftest list-of-integers
  (let ((ir (cl-type-to-ir-type '(list-of integer))))
    (ok (ir-type-list-p ir))
    (ok (eq :integer (ir-type-primitive-kind (ir-type-list-element-type ir))))))

;;; CLOS class references

(defclass test-address ()
  ((street :initarg :street :type string)
   (city :initarg :city :type string)))

(deftest class-reference
  (let ((ir (cl-type-to-ir-type 'test-address)))
    (ok (ir-type-object-p ir))
    (ok (ir-schema-p (ir-type-object-schema ir)))
    (ok (eq 'test-address (ir-schema-class-name (ir-type-object-schema ir))))))

;;; Unsupported types signal schema-error

(deftest unsupported-type-signals-error
  (ok (handler-case
          (progn (cl-type-to-ir-type 'nonexistent-class-xyz) nil)
        (schema-error () t))))
