(defpackage #:clos-constructor/tests/prompt
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/tests/prompt)

(defun person-schema ()
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
            (make-ir-field :name "status"
                           :type (make-ir-type-enum :values '("active" "inactive"))
                           :required-p nil
                           :slot-name 'status))))

(deftest prompt-contains-field-names
  (let ((prompt (generate-system-prompt (person-schema))))
    (ok (search "name" prompt))
    (ok (search "age" prompt))
    (ok (search "status" prompt))))

(deftest prompt-contains-type-info
  (let ((prompt (generate-system-prompt (person-schema))))
    (ok (search "string" prompt))
    (ok (search "integer" prompt))
    (ok (search "enum" prompt))))

(deftest prompt-contains-required-markers
  (let ((prompt (generate-system-prompt (person-schema))))
    (ok (search "required" prompt))
    (ok (search "optional" prompt))))

(deftest prompt-contains-enum-values
  (let ((prompt (generate-system-prompt (person-schema))))
    (ok (search "active" prompt))
    (ok (search "inactive" prompt))))

(deftest prompt-contains-json-instruction
  (let ((prompt (generate-system-prompt (person-schema))))
    (ok (search "JSON" prompt))))

(deftest user-prompt-appended
  (let ((prompt (generate-system-prompt (person-schema)
                                        :user-prompt "This is a medical record.")))
    (ok (search "medical record" prompt))))

;;; Field descriptions flow into the system prompt

(deftest prompt-includes-field-descriptions
  (let* ((schema (make-ir-schema
                  :name "person"
                  :fields (list
                           (make-ir-field :name "name"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'name
                                          :description "Full legal name"))))
         (prompt (generate-system-prompt schema)))
    (ok (search "Full legal name" prompt))))

(deftest prompt-keeps-enum-values-alongside-description
  (let* ((schema (make-ir-schema
                  :name "ticket"
                  :fields (list
                           (make-ir-field :name "urgency"
                                          :type (make-ir-type-enum :values '("low" "high"))
                                          :required-p t
                                          :slot-name 'urgency
                                          :description "How urgent the ticket is"))))
         (prompt (generate-system-prompt schema)))
    (ok (search "How urgent the ticket is" prompt))
    (ok (search "\"low\"" prompt))
    (ok (search "\"high\"" prompt))))
