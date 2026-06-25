(in-package #:clos-constructor)

(defun construct-from-data (data schema)
  "Construct a CLOS instance from validated data using the IR schema.
DATA is a hash table (from JSON parsing). Builds nested objects bottom-up."
  (%construct-object data schema))

(defun %construct-object (data schema)
  (if (ir-schema-class-name schema)
      (%construct-clos-instance data schema)
      (%construct-hash-table data schema)))

(defun %construct-clos-instance (data schema)
  (let ((initargs '()))
    (dolist (field (ir-schema-fields schema))
      (multiple-value-bind (value present-p)
          (gethash (ir-field-name field) data)
        (when present-p
          (let* ((slot-name (ir-field-slot-name field))
                 (initarg (intern (symbol-name slot-name) :keyword))
                 (coerced (%coerce-value value (ir-field-type field))))
            (push coerced initargs)
            (push initarg initargs)))))
    (apply #'make-instance (ir-schema-class-name schema) initargs)))

(defun %construct-hash-table (data schema)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (field (ir-schema-fields schema))
      (multiple-value-bind (value present-p)
          (gethash (ir-field-name field) data)
        (when present-p
          (setf (gethash (ir-field-name field) ht)
                (%coerce-value value (ir-field-type field))))))
    ht))

(defun %coerce-value (value ir-type)
  (if (null value)
      nil
      (etypecase ir-type
        (ir-type-primitive (%coerce-primitive value ir-type))
        (ir-type-enum (%coerce-enum value))
        (ir-type-list (%coerce-list value ir-type))
        (ir-type-object (%coerce-object value ir-type))
        (ir-type-nullable (%coerce-nullable value ir-type)))))

(defun %coerce-primitive (value ir-type)
  (ecase (ir-type-primitive-kind ir-type)
    (:string (if (stringp value) value (princ-to-string value)))
    (:integer (etypecase value
                (integer value)
                (number (truncate value))
                (string (parse-integer value))))
    (:number (etypecase value
               (number value)
               (string (read-from-string value))))
    (:boolean (cond
                ((member value '(t :true) :test #'eq) t)
                ((member value '(nil :false) :test #'eq) nil)
                ((stringp value) (string-equal value "true"))
                (t (not (null value)))))))

(defun %coerce-enum (value)
  "Convert JSON enum string to a keyword."
  (intern (string-upcase value) :keyword))

(defun %coerce-list (value ir-type)
  (mapcar (lambda (elem)
            (%coerce-value elem (ir-type-list-element-type ir-type)))
          value))

(defun %coerce-object (value ir-type)
  (%construct-object value (ir-type-object-schema ir-type)))

(defun %coerce-nullable (value ir-type)
  (if (null value)
      nil
      (%coerce-value value (ir-type-nullable-inner-type ir-type))))
