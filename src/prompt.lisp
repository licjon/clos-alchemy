(in-package #:clos-constructor)

(defun generate-system-prompt (schema &key user-prompt)
  "Generate a system prompt for structured extraction from an ir-schema.
USER-PROMPT appends domain-specific context if provided."
  (with-output-to-string (s)
    (format s "Extract the following structured information from the provided text.~%~%")
    (format s "Fields:~%")
    (dolist (field (ir-schema-fields schema))
      (%format-field-description s field))
    (format s "~%Respond with ONLY a valid JSON object containing these fields. ")
    (format s "Use null for any field you cannot determine from the text.")
    (when user-prompt
      (format s "~%~%~A" user-prompt))))

(defun %format-field-description (stream field)
  (format stream "- ~A (~A~A): ~A~%"
          (ir-field-name field)
          (%type-label (ir-field-type field))
          (if (ir-field-required-p field) ", required" ", optional")
          (%field-hint field)))

(defun %field-hint (field)
  "The slot's :documentation when present, else a generic type hint.
Enum fields always keep their allowed values visible."
  (let ((desc (ir-field-description field))
        (type (ir-field-type field)))
    (cond
      ((and desc (ir-type-enum-p type))
       (format nil "~A — ~A" desc (%type-hint type)))
      (desc desc)
      (t (%type-hint type)))))

(defun %type-label (ir-type)
  (etypecase ir-type
    (ir-type-primitive (string-downcase (symbol-name (ir-type-primitive-kind ir-type))))
    (ir-type-enum "enum")
    (ir-type-list (format nil "array of ~A" (%type-label (ir-type-list-element-type ir-type))))
    (ir-type-object "object")
    (ir-type-nullable (format nil "~A or null" (%type-label (ir-type-nullable-inner-type ir-type))))))

(defun %type-hint (ir-type)
  (etypecase ir-type
    (ir-type-primitive
     (ecase (ir-type-primitive-kind ir-type)
       (:string "text value")
       (:integer "whole number")
       (:number "numeric value")
       (:boolean "true or false")))
    (ir-type-enum
     (format nil "one of: ~{\"~A\"~^, ~}" (ir-type-enum-values ir-type)))
    (ir-type-list
     (format nil "list of ~A values" (%type-label (ir-type-list-element-type ir-type))))
    (ir-type-object
     (format nil "nested object with fields: ~{~A~^, ~}"
             (mapcar #'ir-field-name (ir-schema-fields (ir-type-object-schema ir-type)))))
    (ir-type-nullable
     (format nil "~A, or null if unknown" (%type-hint (ir-type-nullable-inner-type ir-type))))))
