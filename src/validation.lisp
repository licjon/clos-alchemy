(in-package #:clos-alchemy)

(defun validate-data (data schema)
  "Validate parsed data (hash table) against an ir-schema.
Returns (values valid-p error-list). Errors are collected, not signaled."
  (let ((errors '()))
    (dolist (field (ir-schema-fields schema))
      (let ((field-errors (%validate-field data field)))
        (setf errors (nconc errors field-errors))))
    (values (null errors) errors)))

(defun %validate-field (data field)
  (let* ((key (ir-field-name field))
         (present-p (nth-value 1 (gethash key data)))
         (value (gethash key data)))
    (cond
      ;; Missing required field
      ((and (ir-field-required-p field) (not present-p))
       (list (make-condition 'validation-error
                             :field-name key
                             :expected "present"
                             :actual "missing")))
      ;; Missing optional field — ok
      ((not present-p) nil)
      ;; Null value — accepted for declared-nullable and wire-nullable fields
      ((eq value :null)
       (if (or (ir-field-nullable-p field)
               (wire-nullable-p field))
           nil
           (list (make-condition 'validation-error
                                 :field-name key
                                 :expected (format nil "~A (non-null)" (%type-description (ir-field-type field)))
                                 :actual "null"))))
      ;; Type check, then custom validator
      (t (let ((type-errors (%validate-type value (ir-field-type field) key)))
           (if type-errors
               type-errors
               (%run-slot-validator value (ir-field-type field) key
                                    (ir-field-validate field))))))))

(defun %validate-type (value ir-type field-name)
  (etypecase ir-type
    (ir-type-primitive (%validate-primitive value ir-type field-name))
    (ir-type-enum (%validate-enum value ir-type field-name))
    (ir-type-list (%validate-list value ir-type field-name))
    (ir-type-object (%validate-object value ir-type field-name))
    (ir-type-nullable (%validate-nullable value ir-type field-name))))

(defun %validate-primitive (value ir-type field-name)
  (let ((kind (ir-type-primitive-kind ir-type)))
    (if (ecase kind
          (:string (stringp value))
          (:integer (integerp value))
          (:number (numberp value))
          (:boolean (or (eq value t) (eq value nil)
                        (equal value :true) (equal value :false))))
        nil
        (list (make-condition 'validation-error
                              :field-name field-name
                              :expected (string-downcase (symbol-name kind))
                              :actual (format nil "~S" value))))))

(defun %validate-enum (value ir-type field-name)
  (if (and (stringp value) (member value (ir-type-enum-values ir-type) :test #'string=))
      nil
      (list (make-condition 'validation-error
                            :field-name field-name
                            :expected (format nil "one of ~{~S~^, ~}" (ir-type-enum-values ir-type))
                            :actual (format nil "~S" value)))))

(defun %validate-list (value ir-type field-name)
  (if (not (listp value))
      (list (make-condition 'validation-error
                            :field-name field-name
                            :expected "array"
                            :actual (format nil "~S" value)))
      (loop for elem in value
            for i from 0
            nconc (%validate-type elem (ir-type-list-element-type ir-type)
                                  (format nil "~A[~D]" field-name i)))))

(defun %validate-object (value ir-type field-name)
  (if (not (hash-table-p value))
      (list (make-condition 'validation-error
                            :field-name field-name
                            :expected "object"
                            :actual (format nil "~S" value)))
      (multiple-value-bind (valid-p errors)
          (validate-data value (ir-type-object-schema ir-type))
        (declare (ignore valid-p))
        errors)))

(defun %validate-nullable (value ir-type field-name)
  (if (eq value :null)
      nil
      (%validate-type value (ir-type-nullable-inner-type ir-type) field-name)))

(defun %run-slot-validator (value ir-type field-name validator)
  "Run a per-slot validator predicate on the coerced value.
Returns nil on success, or a list of validation-error conditions."
  (when validator
    (let* ((coerced (%coerce-value value ir-type))
           (result (funcall validator coerced)))
      (when (stringp result)
        (list (make-condition 'validation-error
                              :field-name field-name
                              :expected result
                              :actual (format nil "~S" coerced)))))))

(defun %type-description (ir-type)
  (etypecase ir-type
    (ir-type-primitive (string-downcase (symbol-name (ir-type-primitive-kind ir-type))))
    (ir-type-enum (format nil "enum(~{~A~^,~})" (ir-type-enum-values ir-type)))
    (ir-type-list "array")
    (ir-type-object "object")
    (ir-type-nullable (format nil "~A or null" (%type-description (ir-type-nullable-inner-type ir-type))))))

;; Must return a list of strings (not conditions) — the extract loop wraps them.
(defgeneric validate-instance (instance)
  (:documentation "Validate a constructed instance for semantic constraints.
Return an empty list for success, or a list of error-description strings.
Specialize on your class to add cross-field or domain validations.")
  (:method ((instance t)) '()))
