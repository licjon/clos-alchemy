(defpackage #:clos-alchemy/tests/unsupported-types
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/unsupported-types)

;;; Type-specifier coverage beyond the basic table.
;;;
;;; Two behaviors, both explicit:
;;;   1. Mappable specifiers ((vector ...), (unsigned-byte N), (integer ...))
;;;      map to a sensible IR type.
;;;   2. Everything else signals schema-error — never a silent fallback
;;;      to string, which produces wrong output instead of an error.

(defun signals-schema-error-p (type-spec)
  (handler-case
      (progn (cl-type-to-ir-type type-spec) nil)
    (schema-error () t)))

;;; ── Named types with no JSON analogue: schema-error ────────────────

(deftest type/hash-table
  (ok (signals-schema-error-p 'hash-table)))

(deftest type/symbol
  (ok (signals-schema-error-p 'symbol)))

(deftest type/keyword
  (ok (signals-schema-error-p 'keyword)))

(deftest type/character
  (ok (signals-schema-error-p 'character)))

(deftest type/pathname
  (ok (signals-schema-error-p 'pathname)))

;;; ── Compound types that map cleanly ─────────────────────────────────

(deftest type/vector
  (testing "(vector <type>) maps to a typed array with vector container"
    (let ((ir (cl-type-to-ir-type '(vector string))))
      (ok (ir-type-list-p ir))
      (ok (eq :vector (ir-type-list-container ir)))
      (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type ir))))))
  (testing "plain vector defaults to string elements"
    (let ((ir (cl-type-to-ir-type 'vector)))
      (ok (ir-type-list-p ir))
      (ok (eq :vector (ir-type-list-container ir)))))
  (testing "(vector *) defaults to string elements"
    (let ((ir (cl-type-to-ir-type '(vector *))))
      (ok (ir-type-list-p ir))
      (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type ir)))))))

(deftest type/array
  (testing "(array <type>) maps like (vector <type>)"
    (let ((ir (cl-type-to-ir-type '(array integer))))
      (ok (ir-type-list-p ir))
      (ok (eq :vector (ir-type-list-container ir)))
      (ok (eq :integer (ir-type-primitive-kind (ir-type-list-element-type ir)))))))

(deftest type/unsigned-byte
  (testing "(unsigned-byte N) maps to integer"
    (let ((ir (cl-type-to-ir-type '(unsigned-byte 8))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir))))))

(deftest type/integer-range
  (testing "(integer low high) maps to integer"
    (let ((ir (cl-type-to-ir-type '(integer 0 120))))
      (ok (eq :integer (ir-type-primitive-kind ir)))))
  (testing "(mod N) maps to integer"
    (let ((ir (cl-type-to-ir-type '(mod 10))))
      (ok (eq :integer (ir-type-primitive-kind ir)))))
  (testing "(signed-byte N) maps to integer"
    (let ((ir (cl-type-to-ir-type '(signed-byte 16))))
      (ok (eq :integer (ir-type-primitive-kind ir))))))

(deftest type/float-range
  (testing "(float low high) and friends map to number"
    (ok (eq :number (ir-type-primitive-kind (cl-type-to-ir-type '(float 0.0)))))
    (ok (eq :number (ir-type-primitive-kind (cl-type-to-ir-type '(double-float 0d0 1d0)))))
    (ok (eq :number (ir-type-primitive-kind (cl-type-to-ir-type '(real 0)))))))

(deftest type/sized-string
  (testing "(string N) maps to string"
    (let ((ir (cl-type-to-ir-type '(string 10))))
      (ok (eq :string (ir-type-primitive-kind ir))))))

;;; ── Compound types with no JSON analogue: schema-error, not silence ─

(deftest type/satisfies
  (ok (signals-schema-error-p '(satisfies evenp))))

(deftest type/complex
  (ok (signals-schema-error-p '(complex float))))

(deftest type/cons-pair
  (ok (signals-schema-error-p '(cons string integer))))

(deftest type/values
  (ok (signals-schema-error-p '(values string integer))))

(deftest type/unknown-compound
  (testing "an unrecognized compound specifier errors instead of becoming string"
    (ok (signals-schema-error-p '(no-such-thing 42)))))
