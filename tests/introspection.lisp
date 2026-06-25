(defpackage #:clos-constructor/tests/introspection
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/tests/introspection)

;;; Test classes

(defclass person ()
  ((name :initarg :name :type string)
   (age :initarg :age :type integer)
   (email :initarg :email :type (or null string) :initform nil)
   (status :initarg :status :type (member :active :inactive) :initform :active)))

(defclass address ()
  ((street :initarg :street :type string)
   (city :initarg :city :type string)))

(defclass employee ()
  ((name :initarg :name :type string)
   (address :initarg :address :type address)))

;;; Name conversion

(deftest lisp-name-to-json-name-conversion
  (ok (string= "first_name" (lisp-name-to-json-name 'first-name)))
  (ok (string= "email" (lisp-name-to-json-name 'email)))
  (ok (string= "my_slot_name" (lisp-name-to-json-name 'my-slot-name))))

;;; Full introspection

(deftest full-mode-introspection
  (let ((schema (class-to-schema 'person)))
    (ok (ir-schema-p schema))
    (ok (string= "person" (ir-schema-name schema)))
    (ok (eq 'person (ir-schema-class-name schema)))
    (ok (= 4 (length (ir-schema-fields schema))))))

(deftest field-types
  (let* ((schema (class-to-schema 'person))
         (fields (ir-schema-fields schema))
         (name-f (find "name" fields :key #'ir-field-name :test #'string=))
         (age-f (find "age" fields :key #'ir-field-name :test #'string=))
         (email-f (find "email" fields :key #'ir-field-name :test #'string=))
         (status-f (find "status" fields :key #'ir-field-name :test #'string=)))
    ;; name: required string
    (ok (ir-type-primitive-p (ir-field-type name-f)))
    (ok (eq :string (ir-type-primitive-kind (ir-field-type name-f))))
    (ok (ir-field-required-p name-f))
    ;; age: required integer
    (ok (ir-type-primitive-p (ir-field-type age-f)))
    (ok (eq :integer (ir-type-primitive-kind (ir-field-type age-f))))
    (ok (ir-field-required-p age-f))
    ;; email: optional nullable string
    (ok (ir-type-nullable-p (ir-field-type email-f)))
    (ok (not (ir-field-required-p email-f)))
    (ok (ir-field-nullable-p email-f))
    ;; status: optional enum
    (ok (ir-type-enum-p (ir-field-type status-f)))
    (ok (not (ir-field-required-p status-f)))))

;;; Required mode

(deftest required-mode
  (let* ((schema (class-to-schema 'person :mode :required))
         (fields (ir-schema-fields schema)))
    (ok (= 2 (length fields)))
    (ok (find "name" fields :key #'ir-field-name :test #'string=))
    (ok (find "age" fields :key #'ir-field-name :test #'string=))))

;;; Custom mode

(deftest custom-mode
  (let* ((schema (class-to-schema 'person :mode :custom
                                          :slot-list '(name email)))
         (fields (ir-schema-fields schema)))
    (ok (= 2 (length fields)))
    (ok (find "name" fields :key #'ir-field-name :test #'string=))
    (ok (find "email" fields :key #'ir-field-name :test #'string=))))

;;; Nested classes

(deftest nested-class-introspection
  (let* ((schema (class-to-schema 'employee))
         (fields (ir-schema-fields schema))
         (addr-f (find "address" fields :key #'ir-field-name :test #'string=)))
    (ok (= 2 (length fields)))
    (ok (ir-type-object-p (ir-field-type addr-f)))
    (let ((inner (ir-type-object-schema (ir-field-type addr-f))))
      (ok (string= "address" (ir-schema-name inner)))
      (ok (= 2 (length (ir-schema-fields inner)))))))

;;; Slot type overrides

(defclass invoice ()
  ((items :initarg :items :type list)
   (total :initarg :total :type number)))

(deftest slot-type-override
  (let* ((schema (class-to-schema 'invoice
                                  :slot-types '((items . (list-of string)))))
         (fields (ir-schema-fields schema))
         (items-f (find "items" fields :key #'ir-field-name :test #'string=)))
    (ok (ir-type-list-p (ir-field-type items-f)))
    (ok (ir-type-primitive-p (ir-type-list-element-type (ir-field-type items-f))))
    (ok (eq :string (ir-type-primitive-kind
                     (ir-type-list-element-type (ir-field-type items-f)))))))

;;; Schema caching prevents duplicate work

(deftest schema-cache-reuse
  (let ((cache (make-hash-table)))
    (class-to-schema 'address :schema-cache cache)
    (ok (gethash 'address cache))
    (let ((s1 (class-to-schema 'address :schema-cache cache))
          (s2 (gethash 'address cache)))
      (ok (eq s1 s2)))))
