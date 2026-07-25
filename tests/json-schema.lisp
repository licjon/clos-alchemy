(defpackage #:clos-alchemy/tests/json-schema
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/json-schema)

(defun ht-get (ht key)
  (gethash key ht))

;;; Helper to build IR schemas for testing without introspection

(defun make-test-schema ()
  (make-ir-schema
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
                           :slot-name 'email)
            (make-ir-field :name "status"
                           :type (make-ir-type-enum :values '("active" "inactive"))
                           :required-p nil
                           :slot-name 'status))))

;;; Basic structure

(deftest top-level-is-object
  (let ((js (schema-to-json-schema (make-test-schema))))
    (ok (string= "object" (ht-get js "type")))
    (ok (hash-table-p (ht-get js "properties")))
    ;; Must serialise to JSON false, not null. NIL here encodes as null, which
    ;; OpenAI strict mode rejects outright (issue #5).
    (ok (eq 'yason:false (ht-get js "additionalProperties")))))

(deftest required-array
  ;; OpenAI strict mode requires every key in properties to appear in required,
  ;; at every nesting level (issue #15). Optionality is expressed by permitting
  ;; null, not by omission from this array.
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (required (ht-get js "required")))
    (ok (= 4 (length required)))
    (ok (member "name" required :test #'string=))
    (ok (member "age" required :test #'string=))
    (ok (member "email" required :test #'string=))
    (ok (member "status" required :test #'string=))))

;;; Primitive type mappings

(deftest primitive-string
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (props (ht-get js "properties"))
         (name-js (ht-get props "name")))
    (ok (string= "string" (ht-get name-js "type")))))

(deftest primitive-integer
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (props (ht-get js "properties"))
         (age-js (ht-get props "age")))
    (ok (string= "integer" (ht-get age-js "type")))))

;;; Enum

(deftest enum-schema
  ;; `status` is optional and not declared nullable, so it is emitted as a
  ;; choice between its enum type and null (issue #15). The enum itself lives
  ;; in the first branch.
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (props (ht-get js "properties"))
         (status-js (ht-get props "status"))
         (enum-branch (first (ht-get status-js "anyOf"))))
    (ok (string= "string" (ht-get enum-branch "type")))
    (ok (equal '("active" "inactive") (ht-get enum-branch "enum")))))

;;; Nullable

(deftest nullable-schema
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (props (ht-get js "properties"))
         (email-js (ht-get props "email")))
    (ok (listp (ht-get email-js "anyOf")))
    (ok (= 2 (length (ht-get email-js "anyOf"))))
    (let ((inner (first (ht-get email-js "anyOf")))
          (null-type (second (ht-get email-js "anyOf"))))
      (ok (string= "string" (ht-get inner "type")))
      (ok (string= "null" (ht-get null-type "type"))))))

;;; Nested object

(deftest nested-object-schema
  (let* ((address-schema (make-ir-schema
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
                                          :type (make-ir-type-object :schema address-schema)
                                          :required-p t
                                          :slot-name 'address))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (addr-js (ht-get props "address")))
    (ok (string= "object" (ht-get addr-js "type")))
    (ok (hash-table-p (ht-get addr-js "properties")))
    (let ((inner-props (ht-get addr-js "properties")))
      (ok (string= "string" (ht-get (ht-get inner-props "street") "type")))
      (ok (string= "string" (ht-get (ht-get inner-props "city") "type"))))))

;;; List type

(deftest array-schema
  (let* ((schema (make-ir-schema
                  :name "tags"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'items))))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (items-js (ht-get props "items")))
    (ok (string= "array" (ht-get items-js "type")))
    (ok (string= "string" (ht-get (ht-get items-js "items") "type")))))

;;; Round-trip serialization via yason

(deftest json-serialization-roundtrip
  (let* ((js (schema-to-json-schema (make-test-schema)))
         (json-string (with-output-to-string (s)
                        (yason:encode js s)))
         (parsed (yason:parse json-string)))
    (ok (string= "object" (ht-get parsed "type")))
    (ok (hash-table-p (ht-get parsed "properties")))))

;;; Field descriptions flow into the JSON Schema

(deftest field-descriptions-emitted
  (let* ((schema (make-ir-schema
                  :name "person"
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name
                                          :description "Full legal name")
                           (make-ir-field :name "age"
                                          :type (make-ir-type-primitive :kind :integer)
                                          :required-p t
                                          :slot-name 'age))))
         (js (schema-to-json-schema schema))
         (props (gethash "properties" js)))
    (ok (string= "Full legal name" (gethash "description" (gethash "name" props))))
    (ok (null (nth-value 1 (gethash "description" (gethash "age" props)))))))
