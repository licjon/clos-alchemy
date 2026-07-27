(defpackage #:clos-alchemy/tests/ir
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/ir)

;;; Type node construction and predicates

(deftest primitive-types
  (let ((s (make-ir-type-primitive :kind :string))
        (i (make-ir-type-primitive :kind :integer))
        (n (make-ir-type-primitive :kind :number))
        (b (make-ir-type-primitive :kind :boolean)))
    (ok (ir-type-primitive-p s))
    (ok (eq :string (ir-type-primitive-kind s)))
    (ok (eq :integer (ir-type-primitive-kind i)))
    (ok (eq :number (ir-type-primitive-kind n)))
    (ok (eq :boolean (ir-type-primitive-kind b)))
    (ok (not (ir-type-enum-p s)))))

(deftest enum-type
  (let ((e (make-ir-type-enum :values '("red" "green" "blue"))))
    (ok (ir-type-enum-p e))
    (ok (equal '("red" "green" "blue") (ir-type-enum-values e)))
    (ok (not (ir-type-primitive-p e)))))

(deftest list-type
  (let* ((elem (make-ir-type-primitive :kind :string))
         (lt (make-ir-type-list :element-type elem)))
    (ok (ir-type-list-p lt))
    (ok (ir-type-primitive-p (ir-type-list-element-type lt)))
    (ok (eq :string (ir-type-primitive-kind (ir-type-list-element-type lt))))))

(deftest nullable-type
  (let* ((inner (make-ir-type-primitive :kind :string))
         (nt (make-ir-type-nullable :inner-type inner)))
    (ok (ir-type-nullable-p nt))
    (ok (ir-type-primitive-p (ir-type-nullable-inner-type nt)))))

;;; Field construction

(deftest field-construction
  (let ((f (make-ir-field :name "first_name"
                          :type (make-ir-type-primitive :kind :string)
                          :required-p t
                          :nullable-p nil
                          :slot-name 'first-name)))
    (ok (ir-field-p f))
    (ok (string= "first_name" (ir-field-name f)))
    (ok (ir-type-primitive-p (ir-field-type f)))
    (ok (ir-field-required-p f))
    (ok (not (ir-field-nullable-p f)))
    (ok (eq 'first-name (ir-field-slot-name f)))))

;;; Schema construction

(deftest schema-construction
  (let* ((name-field (make-ir-field :name "name"
                                    :type (make-ir-type-primitive :kind :string)
                                    :required-p t
                                    :slot-name 'name))
         (age-field (make-ir-field :name "age"
                                   :type (make-ir-type-primitive :kind :integer)
                                   :required-p t
                                   :slot-name 'age))
         (schema (make-ir-schema :name "person"
                                 :fields (list name-field age-field)
                                 :class-name 'person)))
    (ok (ir-schema-p schema))
    (ok (string= "person" (ir-schema-name schema)))
    (ok (= 2 (length (ir-schema-fields schema))))
    (ok (eq 'person (ir-schema-class-name schema)))
    (ok (null (ir-schema-description schema)))))

;;; Recursive nesting — the critical test

(deftest nested-schemas
  (let* ((street-field (make-ir-field :name "street"
                                      :type (make-ir-type-primitive :kind :string)
                                      :required-p t
                                      :slot-name 'street))
         (city-field (make-ir-field :name "city"
                                    :type (make-ir-type-primitive :kind :string)
                                    :required-p t
                                    :slot-name 'city))
         (address-schema (make-ir-schema :name "address"
                                         :fields (list street-field city-field)
                                         :class-name 'address))
         (name-field (make-ir-field :name "name"
                                    :type (make-ir-type-primitive :kind :string)
                                    :required-p t
                                    :slot-name 'name))
         (address-field (make-ir-field :name "address"
                                       :type (make-ir-type-object :schema address-schema)
                                       :required-p t
                                       :slot-name 'address))
         (person-schema (make-ir-schema :name "person"
                                        :fields (list name-field address-field)
                                        :class-name 'person)))
    ;; Navigate the tree
    (ok (= 2 (length (ir-schema-fields person-schema))))
    (let* ((addr-f (second (ir-schema-fields person-schema)))
           (addr-type (ir-field-type addr-f)))
      (ok (ir-type-object-p addr-type))
      (let ((inner-schema (ir-type-object-schema addr-type)))
        (ok (ir-schema-p inner-schema))
        (ok (string= "address" (ir-schema-name inner-schema)))
        (ok (= 2 (length (ir-schema-fields inner-schema))))))))

;;; Deeply nested — list of objects

(deftest list-of-objects
  (let* ((item-schema (make-ir-schema :name "line_item"
                                      :fields (list (make-ir-field
                                                     :name "description"
                                                     :type (make-ir-type-primitive :kind :string)
                                                     :required-p t
                                                     :slot-name 'description))
                                      :class-name 'line-item))
         (items-type (make-ir-type-list
                      :element-type (make-ir-type-object :schema item-schema)))
         (field (make-ir-field :name "items"
                               :type items-type
                               :required-p t
                               :slot-name 'items)))
    (ok (ir-type-list-p (ir-field-type field)))
    (let ((elem (ir-type-list-element-type (ir-field-type field))))
      (ok (ir-type-object-p elem))
      (ok (string= "line_item" (ir-schema-name (ir-type-object-schema elem)))))))

;;; Compilation artifact and result structs

(deftest extraction-compilation-struct
  (let ((c (make-extraction-compilation
            :schema (make-ir-schema :name "test" :class-name 'test)
            :prompt "Extract data")))
    (ok (extraction-compilation-p c))
    (ok (ir-schema-p (extraction-compilation-schema c)))
    (ok (string= "Extract data" (extraction-compilation-prompt c)))))

(deftest extraction-result-struct
  (let ((r (make-extraction-result
            :instance nil
            :raw-data nil
            :raw-response "{}"
            :retries 2)))
    (ok (extraction-result-p r))
    (ok (= 2 (extraction-result-retries r)))
    (ok (string= "{}" (extraction-result-raw-response r)))))

;;; Conditions

;;; Date type nodes

(deftest date-type
  (let ((d (make-ir-type-date :format :date)))
    (ok (ir-type-date-p d))
    (ok (eq :date (ir-type-date-format d)))
    (ok (not (ir-type-primitive-p d)))))

(deftest date-time-type
  (let ((dt (make-ir-type-date :format :date-time)))
    (ok (ir-type-date-p dt))
    (ok (eq :date-time (ir-type-date-format dt)))))

;;; Conditions

(deftest condition-hierarchy
  (ok (subtypep 'schema-error 'extraction-error))
  (ok (subtypep 'validation-error 'extraction-error))
  (ok (subtypep 'generation-error 'extraction-error))
  (ok (subtypep 'max-retries-error 'extraction-error)))

(deftest schema-error-reporting
  (let ((c (make-condition 'schema-error
                           :class-name 'person
                           :reason "unsupported type")))
    (ok (eq 'person (schema-error-class-name c)))
    (ok (string= "unsupported type" (schema-error-reason c)))
    (ok (search "PERSON" (format nil "~A" c)))))

(deftest validation-error-reporting
  (let ((c (make-condition 'validation-error
                           :field-name "age"
                           :expected "integer"
                           :actual "hello")))
    (ok (string= "age" (validation-error-field-name c)))
    (ok (search "age" (format nil "~A" c)))))
