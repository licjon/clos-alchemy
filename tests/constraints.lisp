(defpackage #:clos-alchemy/tests/constraints
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/constraints)

(defun ht-get (obj key)
  (etypecase obj
    (hash-table (gethash key obj))
    (ordered-map (ordered-map-get obj key))))

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART A — IR layer: constraint fields on type nodes
;;; ══════════════════════════════════════════════════════════════════

(deftest ir/primitive-constraints-default-nil
  (testing "new constraint slots default to nil — backward compatible"
    (let ((p (make-ir-type-primitive :kind :integer)))
      (ok (null (ir-type-primitive-minimum p)))
      (ok (null (ir-type-primitive-maximum p)))
      (ok (null (ir-type-primitive-exclusive-minimum p)))
      (ok (null (ir-type-primitive-exclusive-maximum p)))
      (ok (null (ir-type-primitive-min-length p)))
      (ok (null (ir-type-primitive-max-length p)))
      (ok (null (ir-type-primitive-pattern p))))))

(deftest ir/primitive-constraints-can-be-set
  (testing "constraint slots accept values at construction"
    (let ((p (make-ir-type-primitive :kind :integer
                                     :minimum 0
                                     :maximum 100)))
      (ok (eql 0 (ir-type-primitive-minimum p)))
      (ok (eql 100 (ir-type-primitive-maximum p))))))

(deftest ir/primitive-string-constraints
  (let ((p (make-ir-type-primitive :kind :string
                                   :min-length 1
                                   :max-length 255
                                   :pattern "^[a-z]+$")))
    (ok (eql 1 (ir-type-primitive-min-length p)))
    (ok (eql 255 (ir-type-primitive-max-length p)))
    (ok (string= "^[a-z]+$" (ir-type-primitive-pattern p)))))

(deftest ir/primitive-exclusive-bounds
  (let ((p (make-ir-type-primitive :kind :number
                                   :exclusive-minimum 0
                                   :exclusive-maximum 1)))
    (ok (eql 0 (ir-type-primitive-exclusive-minimum p)))
    (ok (eql 1 (ir-type-primitive-exclusive-maximum p)))))

(deftest ir/list-constraints-default-nil
  (testing "new list constraint slots default to nil"
    (let ((lt (make-ir-type-list
               :element-type (make-ir-type-primitive :kind :string))))
      (ok (null (ir-type-list-min-items lt)))
      (ok (null (ir-type-list-max-items lt))))))

(deftest ir/list-constraints-can-be-set
  (let ((lt (make-ir-type-list
             :element-type (make-ir-type-primitive :kind :string)
             :min-items 1
             :max-items 10)))
    (ok (eql 1 (ir-type-list-min-items lt)))
    (ok (eql 10 (ir-type-list-max-items lt)))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART B — Type mapping: infer constraints from CL type specifiers
;;; ══════════════════════════════════════════════════════════════════

(deftest type-mapping/integer-with-both-bounds
  (testing "(integer 0 100) infers minimum and maximum"
    (let ((ir (cl-type-to-ir-type '(integer 0 100))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (eql 0 (ir-type-primitive-minimum ir)))
      (ok (eql 100 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/integer-lower-bound-only
  (testing "(integer 0 *) infers only minimum"
    (let ((ir (cl-type-to-ir-type '(integer 0 *))))
      (ok (eql 0 (ir-type-primitive-minimum ir)))
      (ok (null (ir-type-primitive-maximum ir))))))

(deftest type-mapping/integer-upper-bound-only
  (testing "(integer * 100) infers only maximum"
    (let ((ir (cl-type-to-ir-type '(integer * 100))))
      (ok (null (ir-type-primitive-minimum ir)))
      (ok (eql 100 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/integer-star-star
  (testing "(integer * *) produces no constraints"
    (let ((ir (cl-type-to-ir-type '(integer * *))))
      (ok (null (ir-type-primitive-minimum ir)))
      (ok (null (ir-type-primitive-maximum ir))))))

(deftest type-mapping/integer-no-args
  (testing "bare (integer) still produces no constraints"
    (let ((ir (cl-type-to-ir-type '(integer))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (null (ir-type-primitive-minimum ir)))
      (ok (null (ir-type-primitive-maximum ir))))))

(deftest type-mapping/float-with-bounds
  (testing "(float 0.0 1.0) infers numeric bounds"
    (let ((ir (cl-type-to-ir-type '(float 0.0 1.0))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :number (ir-type-primitive-kind ir)))
      (ok (eql 0.0 (ir-type-primitive-minimum ir)))
      (ok (eql 1.0 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/single-float-with-bounds
  (testing "(single-float -1.0 1.0) infers bounds"
    (let ((ir (cl-type-to-ir-type '(single-float -1.0 1.0))))
      (ok (eq :number (ir-type-primitive-kind ir)))
      (ok (eql -1.0 (ir-type-primitive-minimum ir)))
      (ok (eql 1.0 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/double-float-with-bounds
  (testing "(double-float 0.0d0 100.0d0) infers bounds"
    (let ((ir (cl-type-to-ir-type '(double-float 0.0d0 100.0d0))))
      (ok (eq :number (ir-type-primitive-kind ir)))
      (ok (eql 0.0d0 (ir-type-primitive-minimum ir)))
      (ok (eql 100.0d0 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/string-with-length
  (testing "(string 5) infers max-length"
    (let ((ir (cl-type-to-ir-type '(string 5))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :string (ir-type-primitive-kind ir)))
      (ok (eql 5 (ir-type-primitive-max-length ir))))))

(deftest type-mapping/string-star
  (testing "(string *) produces no constraint"
    (let ((ir (cl-type-to-ir-type '(string *))))
      (ok (null (ir-type-primitive-max-length ir))))))

(deftest type-mapping/mod-type
  (testing "(mod 256) maps to integer 0..255"
    (let ((ir (cl-type-to-ir-type '(mod 256))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (eql 0 (ir-type-primitive-minimum ir)))
      (ok (eql 255 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/unsigned-byte-with-bits
  (testing "(unsigned-byte 8) maps to integer 0..255"
    (let ((ir (cl-type-to-ir-type '(unsigned-byte 8))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (eql 0 (ir-type-primitive-minimum ir)))
      (ok (eql 255 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/signed-byte-with-bits
  (testing "(signed-byte 8) maps to integer -128..127"
    (let ((ir (cl-type-to-ir-type '(signed-byte 8))))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (eql -128 (ir-type-primitive-minimum ir)))
      (ok (eql 127 (ir-type-primitive-maximum ir))))))

(deftest type-mapping/bare-integer-unchanged
  (testing "bare 'integer still has no constraints"
    (let ((ir (cl-type-to-ir-type 'integer)))
      (ok (ir-type-primitive-p ir))
      (ok (eq :integer (ir-type-primitive-kind ir)))
      (ok (null (ir-type-primitive-minimum ir)))
      (ok (null (ir-type-primitive-maximum ir))))))

(deftest type-mapping/bare-string-unchanged
  (testing "bare 'string still has no constraints"
    (let ((ir (cl-type-to-ir-type 'string)))
      (ok (null (ir-type-primitive-max-length ir))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART C — Metaclass slot options: explicit constraints
;;; ══════════════════════════════════════════════════════════════════

(defclass constrained-item ()
  ((score :initarg :score :type integer
          :minimum 0 :maximum 100)
   (name :initarg :name :type string
         :min-length 1 :max-length 50 :pattern "^[A-Za-z]+$")
   (tags :initarg :tags :type list :list-of string
         :min-items 1 :max-items 5)
   (rating :initarg :rating :type float
           :exclusive-minimum 0.0 :exclusive-maximum 5.0))
  (:metaclass constructor-class))

(deftest metaclass/constraint-slot-accessors
  (closer-mop:ensure-finalized (find-class 'constrained-item))
  (let* ((slots (closer-mop:class-slots (find-class 'constrained-item)))
         (score-slot (find 'score slots :key #'closer-mop:slot-definition-name))
         (name-slot (find 'name slots :key #'closer-mop:slot-definition-name))
         (tags-slot (find 'tags slots :key #'closer-mop:slot-definition-name)))
    (ok (eql 0 (slot-definition-minimum score-slot)))
    (ok (eql 100 (slot-definition-maximum score-slot)))
    (ok (eql 1 (slot-definition-min-length name-slot)))
    (ok (eql 50 (slot-definition-max-length name-slot)))
    (ok (string= "^[A-Za-z]+$" (slot-definition-pattern name-slot)))
    (ok (eql 1 (slot-definition-min-items tags-slot)))
    (ok (eql 5 (slot-definition-max-items tags-slot)))))

(deftest metaclass/slot-option-constraints-flow-to-ir
  (testing "slot options produce constraint fields on IR type nodes"
    (let* ((schema (class-to-schema 'constrained-item))
           (fields (ir-schema-fields schema))
           (score-f (find "score" fields :key #'ir-field-name :test #'string=))
           (name-f (find "name" fields :key #'ir-field-name :test #'string=))
           (tags-f (find "tags" fields :key #'ir-field-name :test #'string=))
           (rating-f (find "rating" fields :key #'ir-field-name :test #'string=)))
      ;; score: integer 0..100
      (ok (eql 0 (ir-type-primitive-minimum (ir-field-type score-f))))
      (ok (eql 100 (ir-type-primitive-maximum (ir-field-type score-f))))
      ;; name: string 1..50 with pattern
      (ok (eql 1 (ir-type-primitive-min-length (ir-field-type name-f))))
      (ok (eql 50 (ir-type-primitive-max-length (ir-field-type name-f))))
      (ok (string= "^[A-Za-z]+$" (ir-type-primitive-pattern (ir-field-type name-f))))
      ;; tags: list 1..5 items
      (ok (eql 1 (ir-type-list-min-items (ir-field-type tags-f))))
      (ok (eql 5 (ir-type-list-max-items (ir-field-type tags-f))))
      ;; rating: exclusive bounds
      (ok (eql 0.0 (ir-type-primitive-exclusive-minimum (ir-field-type rating-f))))
      (ok (eql 5.0 (ir-type-primitive-exclusive-maximum (ir-field-type rating-f)))))))

(deftest metaclass/slot-options-override-inferred-bounds
  (testing "explicit :minimum/:maximum override inferred type bounds"
    (defclass override-bounds ()
      ((val :initarg :val :type (integer 0 1000)
            :minimum 10 :maximum 500))
      (:metaclass constructor-class))
    (let* ((schema (class-to-schema 'override-bounds))
           (fields (ir-schema-fields schema))
           (val-f (find "val" fields :key #'ir-field-name :test #'string=)))
      (ok (eql 10 (ir-type-primitive-minimum (ir-field-type val-f))))
      (ok (eql 500 (ir-type-primitive-maximum (ir-field-type val-f)))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART D — JSON Schema emission: constraint keywords
;;; ══════════════════════════════════════════════════════════════════

(deftest json-schema/integer-bounds-emitted
  (let* ((schema (make-ir-schema
                  :name "bounded"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :integer
                                                                        :minimum 0
                                                                        :maximum 100)
                                          :required-p t
                                          :slot-name 'val))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (val-js (ht-get props "val")))
    (ok (string= "integer" (ht-get val-js "type")))
    (ok (eql 0 (ht-get val-js "minimum")))
    (ok (eql 100 (ht-get val-js "maximum")))))

(deftest json-schema/exclusive-bounds-emitted
  (let* ((schema (make-ir-schema
                  :name "exclusive"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :number
                                                                        :exclusive-minimum 0
                                                                        :exclusive-maximum 1)
                                          :required-p t
                                          :slot-name 'val))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (val-js (ht-get props "val")))
    (ok (eql 0 (ht-get val-js "exclusiveMinimum")))
    (ok (eql 1 (ht-get val-js "exclusiveMaximum")))))

(deftest json-schema/string-constraints-emitted
  (let* ((schema (make-ir-schema
                  :name "constrained_str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string
                                                                        :min-length 1
                                                                        :max-length 50
                                                                        :pattern "^[a-z]+$")
                                          :required-p t
                                          :slot-name 'val))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (val-js (ht-get props "val")))
    (ok (string= "string" (ht-get val-js "type")))
    (ok (eql 1 (ht-get val-js "minLength")))
    (ok (eql 50 (ht-get val-js "maxLength")))
    (ok (string= "^[a-z]+$" (ht-get val-js "pattern")))))

(deftest json-schema/list-constraints-emitted
  (let* ((schema (make-ir-schema
                  :name "bounded_list"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string)
                                                 :min-items 1
                                                 :max-items 10)
                                          :required-p t
                                          :slot-name 'items))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (items-js (ht-get props "items")))
    (ok (string= "array" (ht-get items-js "type")))
    (ok (eql 1 (ht-get items-js "minItems")))
    (ok (eql 10 (ht-get items-js "maxItems")))))

(deftest json-schema/no-constraints-no-extra-keys
  (testing "primitives without constraints must not emit constraint keywords"
    (let* ((schema (make-ir-schema
                    :name "plain"
                    :fields (list
                             (make-ir-field :name "val"
                                            :type (make-ir-type-primitive :kind :integer)
                                            :required-p t
                                            :slot-name 'val))))
           (js (schema-to-json-schema schema))
           (props (ht-get js "properties"))
           (val-js (ht-get props "val")))
      (ok (null (nth-value 1 (gethash "minimum" val-js))))
      (ok (null (nth-value 1 (gethash "maximum" val-js))))
      (ok (null (nth-value 1 (gethash "minLength" val-js))))
      (ok (null (nth-value 1 (gethash "maxLength" val-js))))
      (ok (null (nth-value 1 (gethash "pattern" val-js)))))))

(deftest json-schema/constraint-serialization-roundtrip
  (testing "constrained schemas serialize and parse correctly via yason"
    (let* ((schema (make-ir-schema
                    :name "bounded"
                    :fields (list
                             (make-ir-field :name "score"
                                            :type (make-ir-type-primitive :kind :integer
                                                                          :minimum 0
                                                                          :maximum 100)
                                            :required-p t
                                            :slot-name 'score))))
           (js (schema-to-json-schema schema))
           (json-string (with-output-to-string (s) (yason:encode js s)))
           (parsed (yason:parse json-string))
           (score-js (gethash "score" (gethash "properties" parsed))))
      (ok (eql 0 (gethash "minimum" score-js)))
      (ok (eql 100 (gethash "maximum" score-js))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART E — Validation: enforce constraints
;;; ══════════════════════════════════════════════════════════════════

;;; Numeric bounds

(deftest validation/integer-minimum-pass
  (let* ((schema (make-ir-schema
                  :name "bounded"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :integer :minimum 0)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" 0)))
    (ok (validate-data data schema))))

(deftest validation/integer-minimum-fail
  (let* ((schema (make-ir-schema
                  :name "bounded"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :integer :minimum 0)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" -1)))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest validation/integer-maximum-pass
  (let* ((schema (make-ir-schema
                  :name "bounded"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :integer :maximum 100)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" 100)))
    (ok (validate-data data schema))))

(deftest validation/integer-maximum-fail
  (let* ((schema (make-ir-schema
                  :name "bounded"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :integer :maximum 100)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" 101)))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest validation/exclusive-minimum-boundary
  (testing "value equal to exclusive-minimum fails"
    (let* ((schema (make-ir-schema
                    :name "exc"
                    :fields (list
                             (make-ir-field :name "val"
                                            :type (make-ir-type-primitive :kind :number
                                                                          :exclusive-minimum 0)
                                            :required-p t
                                            :slot-name 'val))))
           (data (make-data "val" 0)))
      (ok (not (validate-data data schema))))))

(deftest validation/exclusive-minimum-pass
  (let* ((schema (make-ir-schema
                  :name "exc"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :number
                                                                        :exclusive-minimum 0)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" 0.001)))
    (ok (validate-data data schema))))

(deftest validation/exclusive-maximum-boundary
  (testing "value equal to exclusive-maximum fails"
    (let* ((schema (make-ir-schema
                    :name "exc"
                    :fields (list
                             (make-ir-field :name "val"
                                            :type (make-ir-type-primitive :kind :number
                                                                          :exclusive-maximum 1)
                                            :required-p t
                                            :slot-name 'val))))
           (data (make-data "val" 1)))
      (ok (not (validate-data data schema))))))

;;; String constraints

(deftest validation/min-length-pass
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string :min-length 3)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "abc")))
    (ok (validate-data data schema))))

(deftest validation/min-length-fail
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string :min-length 3)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "ab")))
    (ok (not (validate-data data schema)))))

(deftest validation/max-length-pass
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string :max-length 5)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "hello")))
    (ok (validate-data data schema))))

(deftest validation/max-length-fail
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string :max-length 5)
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "toolong")))
    (ok (not (validate-data data schema)))))

(deftest validation/pattern-pass
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string
                                                                        :pattern "^[0-9]+$")
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "12345")))
    (ok (validate-data data schema))))

(deftest validation/pattern-fail
  (let* ((schema (make-ir-schema
                  :name "str"
                  :fields (list
                           (make-ir-field :name "val"
                                          :type (make-ir-type-primitive :kind :string
                                                                        :pattern "^[0-9]+$")
                                          :required-p t
                                          :slot-name 'val))))
         (data (make-data "val" "abc")))
    (ok (not (validate-data data schema)))))

;;; List constraints

(deftest validation/min-items-pass
  (let* ((schema (make-ir-schema
                  :name "list"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string)
                                                 :min-items 2)
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" '("a" "b"))))
    (ok (validate-data data schema))))

(deftest validation/min-items-fail
  (let* ((schema (make-ir-schema
                  :name "list"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string)
                                                 :min-items 2)
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" '("a"))))
    (ok (not (validate-data data schema)))))

(deftest validation/max-items-pass
  (let* ((schema (make-ir-schema
                  :name "list"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string)
                                                 :max-items 3)
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" '("a" "b" "c"))))
    (ok (validate-data data schema))))

(deftest validation/max-items-fail
  (let* ((schema (make-ir-schema
                  :name "list"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string)
                                                 :max-items 2)
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" '("a" "b" "c"))))
    (ok (not (validate-data data schema)))))

;;; Constraint validation still runs type check first

(deftest validation/type-error-before-constraint
  (testing "type mismatch is reported even when constraints are present"
    (let* ((schema (make-ir-schema
                    :name "bounded"
                    :fields (list
                             (make-ir-field :name "val"
                                            :type (make-ir-type-primitive :kind :integer
                                                                          :minimum 0
                                                                          :maximum 100)
                                            :required-p t
                                            :slot-name 'val))))
           (data (make-data "val" "hello")))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (= 1 (length errors)))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART F — Full pipeline: class → schema → JSON Schema → validate
;;; ══════════════════════════════════════════════════════════════════

(defclass bounded-score ()
  ((value :initarg :value :type (integer 0 100)))
  (:metaclass constructor-class))

(deftest pipeline/integer-bounds-full-trip
  (testing "CL type → IR → JSON Schema → validation"
    (let* ((schema (class-to-schema 'bounded-score))
           (field (first (ir-schema-fields schema)))
           (ir-type (ir-field-type field)))
      ;; IR captured the bounds
      (ok (eql 0 (ir-type-primitive-minimum ir-type)))
      (ok (eql 100 (ir-type-primitive-maximum ir-type)))
      ;; JSON Schema has the keywords
      (let* ((js (schema-to-json-schema schema))
             (props (ht-get js "properties"))
             (val-js (ht-get props "value")))
        (ok (eql 0 (ht-get val-js "minimum")))
        (ok (eql 100 (ht-get val-js "maximum"))))
      ;; Validation enforces them
      (ok (validate-data (make-data "value" 50) schema))
      (ok (not (validate-data (make-data "value" -1) schema)))
      (ok (not (validate-data (make-data "value" 101) schema))))))

(defclass string-length-holder ()
  ((name :initarg :name :type (string 10)))
  (:metaclass constructor-class))

(deftest pipeline/string-length-full-trip
  (let* ((schema (class-to-schema 'string-length-holder))
         (field (first (ir-schema-fields schema)))
         (ir-type (ir-field-type field)))
    (ok (eql 10 (ir-type-primitive-max-length ir-type)))
    (let* ((js (schema-to-json-schema schema))
           (props (ht-get js "properties"))
           (name-js (ht-get props "name")))
      (ok (eql 10 (ht-get name-js "maxLength"))))
    (ok (validate-data (make-data "name" "short") schema))
    (ok (not (validate-data (make-data "name" "this is way too long") schema)))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART G — Schema conformance: constrained schemas are still valid
;;; ══════════════════════════════════════════════════════════════════

(deftest conformance/constrained-schema-conforms
  (testing "constraint keywords don't violate OpenAI strict mode"
    (let* ((schema (make-ir-schema
                    :name "constrained"
                    :fields (list
                             (make-ir-field :name "score"
                                            :type (make-ir-type-primitive :kind :integer
                                                                          :minimum 0
                                                                          :maximum 100)
                                            :required-p t
                                            :slot-name 'score)
                             (make-ir-field :name "name"
                                            :type (make-ir-type-primitive :kind :string
                                                                          :min-length 1
                                                                          :max-length 50)
                                            :required-p t
                                            :slot-name 'name))))
           (js (schema-to-json-schema schema)))
      (ok (string= "object" (ht-get js "type")))
      (ok (eq 'yason:false (ht-get js "additionalProperties"))))))
