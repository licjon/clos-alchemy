(defpackage #:clos-alchemy/tests/construction
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/construction)

;;; Test classes

(defclass person ()
  ((name :initarg :name :accessor person-name :type string)
   (age :initarg :age :accessor person-age :type integer)
   (email :initarg :email :accessor person-email :type (or null string) :initform nil)
   (status :initarg :status :accessor person-status :type keyword :initform :active)))

(defclass address ()
  ((street :initarg :street :accessor address-street :type string)
   (city :initarg :city :accessor address-city :type string)))

(defclass employee ()
  ((name :initarg :name :accessor employee-name :type string)
   (address :initarg :address :accessor employee-address :type address)))

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

;;; Simple construction

(deftest construct-simple-object
  (let* ((schema (make-ir-schema
                  :name "person"
                  :class-name 'person
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name)
                           (make-ir-field :name "age"
                                          :type (make-ir-type-primitive :kind :integer)
                                          :required-p t
                                          :slot-name 'age))))
         (data (make-data "name" "Alice" "age" 30))
         (instance (construct-from-data data schema)))
    (ok (typep instance 'person))
    (ok (string= "Alice" (person-name instance)))
    (ok (= 30 (person-age instance)))))

;;; Enum coercion — string to keyword

(deftest enum-coercion
  (let* ((schema (make-ir-schema
                  :name "person"
                  :class-name 'person
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name)
                           (make-ir-field :name "age"
                                          :type (make-ir-type-primitive :kind :integer)
                                          :required-p t
                                          :slot-name 'age)
                           (make-ir-field :name "status"
                                          :type (make-ir-type-enum :values '("active" "inactive"))
                                          :required-p nil
                                          :slot-name 'status))))
         (data (make-data "name" "Bob" "age" 25 "status" "active"))
         (instance (construct-from-data data schema)))
    (ok (eq :active (person-status instance)))))

;;; Nullable field with null

(deftest nullable-null-value
  (let* ((schema (make-ir-schema
                  :name "person"
                  :class-name 'person
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name)
                           (make-ir-field :name "age"
                                          :type (make-ir-type-primitive :kind :integer)
                                          :required-p t
                                          :slot-name 'age)
                           (make-ir-field :name "email"
                                          :type (make-ir-type-nullable
                                                 :inner-type (make-ir-type-primitive :kind :string))
                                          :required-p nil
                                          :nullable-p t
                                          :slot-name 'email))))
         (data (make-data "name" "Alice" "age" 30 "email" :null))
         (instance (construct-from-data data schema)))
    (ok (null (person-email instance)))))

;;; Nested object construction

(deftest nested-construction
  (let* ((addr-schema (make-ir-schema
                       :name "address"
                       :class-name 'address
                       :fields (list
                                (make-ir-field :name "street"
                                               :type (make-ir-type-primitive :kind :string)
                                               :required-p t
                                               :slot-name 'street)
                                (make-ir-field :name "city"
                                               :type (make-ir-type-primitive :kind :string)
                                               :required-p t
                                               :slot-name 'city))))
         (schema (make-ir-schema
                  :name "employee"
                  :class-name 'employee
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name)
                           (make-ir-field :name "address"
                                          :type (make-ir-type-object :schema addr-schema)
                                          :required-p t
                                          :slot-name 'address))))
         (data (make-data "name" "Carol"
                          "address" (make-data "street" "123 Main St"
                                               "city" "Springfield")))
         (instance (construct-from-data data schema)))
    (ok (typep instance 'employee))
    (ok (string= "Carol" (employee-name instance)))
    (ok (typep (employee-address instance) 'address))
    (ok (string= "123 Main St" (address-street (employee-address instance))))
    (ok (string= "Springfield" (address-city (employee-address instance))))))

;;; Nil class-name (synthetic wrapper schemas like extract-list)

(deftest construct-nil-class-name-returns-hash-table
  (testing "schemas with nil class-name return a hash-table instead of crashing"
    (let* ((inner-schema (make-ir-schema
                          :name "person"
                          :class-name 'person
                          :fields (list
                                   (make-ir-field :name "name"
                                                  :type (make-ir-type-primitive :kind :string)
                                                  :required-p t
                                                  :slot-name 'name)
                                   (make-ir-field :name "age"
                                                  :type (make-ir-type-primitive :kind :integer)
                                                  :required-p t
                                                  :slot-name 'age))))
           (wrapper-schema (make-ir-schema
                            :name "person_list"
                            :class-name nil
                            :fields (list
                                     (make-ir-field
                                      :name "items"
                                      :type (make-ir-type-list
                                             :element-type (make-ir-type-object
                                                            :schema inner-schema))
                                      :required-p t
                                      :slot-name 'items))))
           (data (make-data "items" (list (make-data "name" "Alice" "age" 30)
                                          (make-data "name" "Bob" "age" 25))))
           (result (construct-from-data data wrapper-schema)))
      (ok (hash-table-p result))
      (let ((items (gethash "items" result)))
        (ok (listp items))
        (ok (= 2 (length items)))
        (ok (typep (first items) 'person))
        (ok (string= "Alice" (person-name (first items))))
        (ok (typep (second items) 'person))
        (ok (string= "Bob" (person-name (second items))))))))

;;; Integer coercion from float (JSON numbers)

(deftest integer-coercion-from-float
  (let* ((schema (make-ir-schema
                  :name "person"
                  :class-name 'person
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name)
                           (make-ir-field :name "age"
                                          :type (make-ir-type-primitive :kind :integer)
                                          :required-p t
                                          :slot-name 'age))))
         (data (make-data "name" "Dave" "age" 30.0))
         (instance (construct-from-data data schema)))
    (ok (integerp (person-age instance)))
    (ok (= 30 (person-age instance)))))

;;; Initarg handling — declared initargs and initarg-less slots

(defclass renamed-initarg ()
  ((name :initarg :the-name :accessor renamed-name :type string)))

(deftest construction-uses-declared-initarg
  (testing "a slot whose initarg differs from its name constructs correctly"
    (let* ((schema (class-to-schema 'renamed-initarg))
           (data (make-data "name" "Alice"))
           (instance (construct-from-data data schema)))
      (ok (typep instance 'renamed-initarg))
      (ok (string= "Alice" (renamed-name instance))))))

(defclass initarg-less ()
  ((name :initarg :name :accessor il-name :type string)
   (notes :accessor il-notes :type string)))

(deftest construction-sets-slot-without-initarg
  (testing "a slot with no initarg is set via slot-value after make-instance"
    (let* ((schema (class-to-schema 'initarg-less))
           (data (make-data "name" "Bob" "notes" "from the LLM"))
           (instance (construct-from-data data schema)))
      (ok (string= "Bob" (il-name instance)))
      (ok (string= "from the LLM" (il-notes instance))))))

;;; Vector-typed slots receive CL vectors, not lists

(defclass tagged-thing ()
  ((tags :initarg :tags :accessor tagged-thing-tags :type (vector string))))

(deftest vector-slot-coerces-to-vector
  (let* ((schema (class-to-schema 'tagged-thing))
         (data (make-data "tags" (list "a" "b")))
         (instance (construct-from-data data schema)))
    (ok (vectorp (tagged-thing-tags instance)))
    (ok (equalp #("a" "b") (tagged-thing-tags instance)))))
