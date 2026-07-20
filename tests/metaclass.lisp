(defpackage #:clos-alchemy/tests/metaclass
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/metaclass)

;;; Test classes using the custom slot option

(defclass tagged-items ()
  ((tags :initarg :tags :type list :list-of string :accessor item-tags)
   (scores :initarg :scores :type list :list-of integer :accessor item-scores)
   (name :initarg :name :type string :accessor item-name))
  (:metaclass constructor-class))

;;; Metaclass basics

(deftest metaclass-is-constructor-class
  (ok (typep (find-class 'tagged-items) 'constructor-class)))

(deftest slot-list-of-accessor
  (closer-mop:ensure-finalized (find-class 'tagged-items))
  (let* ((slots (closer-mop:class-slots (find-class 'tagged-items)))
         (tags-slot (find 'tags slots :key #'closer-mop:slot-definition-name))
         (name-slot (find 'name slots :key #'closer-mop:slot-definition-name)))
    (ok (slot-definition-list-of tags-slot))
    (ok (eq 'string (slot-definition-list-of tags-slot)))
    (ok (null (slot-definition-list-of name-slot)))))

;;; Introspection integration

(deftest list-of-slot-option-produces-ir-type-list
  (let* ((schema (class-to-schema 'tagged-items))
         (fields (ir-schema-fields schema))
         (tags-f (find "tags" fields :key #'ir-field-name :test #'string=))
         (scores-f (find "scores" fields :key #'ir-field-name :test #'string=))
         (name-f (find "name" fields :key #'ir-field-name :test #'string=)))
    (ok (= 3 (length fields)))
    ;; tags: list-of string
    (ok (ir-type-list-p (ir-field-type tags-f)))
    (ok (ir-type-primitive-p (ir-type-list-element-type (ir-field-type tags-f))))
    (ok (eq :string (ir-type-primitive-kind
                     (ir-type-list-element-type (ir-field-type tags-f)))))
    ;; scores: list-of integer
    (ok (ir-type-list-p (ir-field-type scores-f)))
    (ok (eq :integer (ir-type-primitive-kind
                      (ir-type-list-element-type (ir-field-type scores-f)))))
    ;; name: plain string (no list-of)
    (ok (ir-type-primitive-p (ir-field-type name-f)))
    (ok (eq :string (ir-type-primitive-kind (ir-field-type name-f))))))

;;; Slot-types override still takes precedence

(deftest slot-types-override-beats-list-of-option
  (let* ((schema (class-to-schema 'tagged-items
                                  :slot-types '((tags . string))))
         (fields (ir-schema-fields schema))
         (tags-f (find "tags" fields :key #'ir-field-name :test #'string=)))
    (ok (ir-type-primitive-p (ir-field-type tags-f)))
    (ok (eq :string (ir-type-primitive-kind (ir-field-type tags-f))))))

;;; Instance construction works

(deftest constructor-class-instances-work
  (let ((obj (make-instance 'tagged-items
                            :name "test"
                            :tags '("a" "b")
                            :scores '(1 2 3))))
    (ok (typep obj 'tagged-items))
    (ok (equal '("a" "b") (item-tags obj)))
    (ok (equal '(1 2 3) (item-scores obj)))
    (ok (string= "test" (item-name obj)))))
