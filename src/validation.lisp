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
    (ir-type-nullable (%validate-nullable value ir-type field-name))
    (ir-type-date (%validate-date value ir-type field-name))))

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

(defun %validate-date (value ir-type field-name)
  (if (not (stringp value))
      (list (make-condition 'validation-error
                            :field-name field-name
                            :expected (ecase (ir-type-date-format ir-type)
                                        (:date "date string")
                                        (:date-time "date-time string"))
                            :actual (format nil "~S" value)))
      (ecase (ir-type-date-format ir-type)
        (:date (%validate-date-string value field-name))
        (:date-time (%validate-date-time-string value field-name)))))

(defun %validate-date-string (value field-name)
  (multiple-value-bind (year month day)
      (%parse-date-components value 0 (length value))
    (if (and year month day
             (<= 1 month 12)
             (<= 1 day (%days-in-month month year)))
        nil
        (list (make-condition 'validation-error
                              :field-name field-name
                              :expected "ISO 8601 date (YYYY-MM-DD)"
                              :actual (format nil "~S" value))))))

(defun %validate-date-time-string (value field-name)
  (let ((len (length value)))
    (if (or (< len 20)
            (char/= (char value 10) #\T))
        (list (make-condition 'validation-error
                              :field-name field-name
                              :expected "ISO 8601 date-time (YYYY-MM-DDThh:mm:ssZ or YYYY-MM-DDThh:mm:ss±hh:mm)"
                              :actual (format nil "~S" value)))
        (multiple-value-bind (year month day)
            (%parse-date-components value 0 10)
          (multiple-value-bind (hour minute second tz-valid-p)
              (%parse-time-components value 11 len)
            (if (and year month day hour minute second tz-valid-p
                     (<= 1 month 12)
                     (<= 1 day (%days-in-month month year))
                     (<= 0 hour 23)
                     (<= 0 minute 59)
                     (<= 0 second 59))
                nil
                (list (make-condition 'validation-error
                                      :field-name field-name
                                      :expected "ISO 8601 date-time (YYYY-MM-DDThh:mm:ssZ or YYYY-MM-DDThh:mm:ss±hh:mm)"
                                      :actual (format nil "~S" value)))))))))

(defun %parse-date-components (string start end)
  "Parse YYYY-MM-DD from STRING between START and END.
Returns (values year month day), all nil on failure."
  (when (and (= (- end start) 10)
             (char= (char string (+ start 4)) #\-)
             (char= (char string (+ start 7)) #\-))
    (let ((year (%parse-digits string start (+ start 4)))
          (month (%parse-digits string (+ start 5) (+ start 7)))
          (day (%parse-digits string (+ start 8) (+ start 10))))
      (values year month day))))

(defun %parse-time-components (string start end)
  "Parse hh:mm:ss[.frac](Z|±hh:mm) from STRING between START and END.
Returns (values hour minute second tz-valid-p), all nil on failure."
  (when (and (>= (- end start) 9)
             (char= (char string (+ start 2)) #\:)
             (char= (char string (+ start 5)) #\:))
    (let ((hour (%parse-digits string start (+ start 2)))
          (minute (%parse-digits string (+ start 3) (+ start 5)))
          (second (%parse-digits string (+ start 6) (+ start 8))))
      (when (and hour minute second)
        (let ((rest-start (+ start 8)))
          ;; skip fractional seconds
          (when (and (< rest-start end)
                     (char= (char string rest-start) #\.))
            (incf rest-start)
            (loop while (and (< rest-start end)
                             (digit-char-p (char string rest-start)))
                  do (incf rest-start)))
          (let ((tz-valid (%validate-tz-suffix string rest-start end)))
            (values hour minute second tz-valid)))))))

(defun %validate-tz-suffix (string start end)
  "Return T if the substring from START to END is a valid timezone suffix."
  (let ((remaining (- end start)))
    (cond
      ((and (= remaining 1) (char= (char string start) #\Z)) t)
      ((and (= remaining 6)
            (member (char string start) '(#\+ #\-))
            (char= (char string (+ start 3)) #\:)
            (%parse-digits string (+ start 1) (+ start 3))
            (%parse-digits string (+ start 4) (+ start 6)))
       t)
      (t nil))))

(defun %parse-digits (string start end)
  "Parse an unsigned integer from STRING between START and END.
Returns the integer, or nil if any character is not a digit."
  (loop for i from start below end
        unless (digit-char-p (char string i))
          do (return nil)
        finally (return (parse-integer string :start start :end end))))

(defun %days-in-month (month year)
  "Return the number of days in MONTH for the given YEAR."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (%leap-year-p year) 29 28))
    (t 0)))

(defun %leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

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
    (ir-type-nullable (format nil "~A or null" (%type-description (ir-type-nullable-inner-type ir-type))))
    (ir-type-date (ecase (ir-type-date-format ir-type)
                    (:date "date")
                    (:date-time "date-time")))))

;; Must return a list of strings (not conditions) — the extract loop wraps them.
(defgeneric validate-instance (instance)
  (:documentation "Validate a constructed instance for semantic constraints.
Return an empty list for success, or a list of error-description strings.
Specialize on your class to add cross-field or domain validations.")
  (:method ((instance t)) '()))
