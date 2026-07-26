(in-package #:clos-alchemy)

(defun construct-from-data (data schema)
  "Construct a CLOS instance from validated data using the IR schema.
DATA is a hash table (from JSON parsing). Builds nested objects bottom-up."
  (%construct-object data schema))

(defun %construct-object (data schema)
  (if (ir-schema-class-name schema)
      (%construct-clos-instance data schema)
      (%construct-hash-table data schema)))

(defun %construct-clos-instance (data schema)
  (let ((initargs '())
        (slot-values '()))
    (dolist (field (ir-schema-fields schema))
      (multiple-value-bind (value present-p)
          (gethash (ir-field-name field) data)
        (when (and present-p
                   (not (and (eq value :null) (wire-nullable-p field))))
          (let ((coerced (%coerce-value value (ir-field-type field)))
                (initarg (ir-field-initarg field)))
            (if initarg
                (progn (push coerced initargs)
                       (push initarg initargs))
                (push (cons (ir-field-slot-name field) coerced)
                      slot-values))))))
    (let ((instance (apply #'make-instance (ir-schema-class-name schema)
                           initargs)))
      (dolist (entry slot-values instance)
        (setf (slot-value instance (car entry)) (cdr entry))))))

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
  (if (eq value :null)
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
    (:number (let ((n (etypecase value
                       (number value)
                       (string (%safe-parse-number value))))
                   (target (ir-type-primitive-numeric-type ir-type)))
               (if target (coerce n target) n)))
    (:boolean (cond
                ((member value '(t :true) :test #'eq) t)
                ((member value '(nil :false) :test #'eq) nil)
                ((stringp value) (string-equal value "true"))
                (t (not (null value)))))))

(defun %safe-parse-number (string)
  "Parse a numeric value from STRING without invoking the full CL reader."
  (let ((end (length string)))
    (when (zerop end)
      (error 'generation-error :backend :coerce
             :reason "empty string for number field"))
    (multiple-value-bind (integer pos)
        (parse-integer string :junk-allowed t)
      (cond
        ((and integer (= pos end)) integer)
        ((and integer (< pos end) (char= (char string pos) #\.))
         (multiple-value-bind (frac frac-end)
             (parse-integer string :start (1+ pos) :junk-allowed t)
           (if (and frac (= frac-end end))
               (let ((divisor (expt 10 (- frac-end (1+ pos)))))
                 (coerce (if (char= (char string 0) #\-)
                             (- integer (/ frac divisor))
                             (+ integer (/ frac divisor)))
                         'double-float))
               (error 'generation-error :backend :coerce
                      :reason (format nil "invalid number: ~S" string)))))
        (t (error 'generation-error :backend :coerce
                  :reason (format nil "invalid number: ~S" string)))))))

(defun %coerce-enum (value)
  "Convert JSON enum string to a keyword."
  (intern (string-upcase value) :keyword))

(defun %coerce-list (value ir-type)
  (let ((items (mapcar (lambda (elem)
                         (%coerce-value elem (ir-type-list-element-type ir-type)))
                       value)))
    (if (eq :vector (ir-type-list-container ir-type))
        (coerce items 'vector)
        items)))

(defun %coerce-object (value ir-type)
  (%construct-object value (ir-type-object-schema ir-type)))

(defun %coerce-nullable (value ir-type)
  (if (eq value :null)
      nil
      (%coerce-value value (ir-type-nullable-inner-type ir-type))))
