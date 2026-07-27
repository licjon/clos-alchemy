(defpackage #:clos-alchemy/tests/validation
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/validation)

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

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

;;; Valid data

(deftest valid-data-passes
  (let ((data (make-data "name" "Alice" "age" 30 "email" :null "status" "active")))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok valid-p)
      (ok (null errors)))))

(deftest valid-with-only-required
  (let ((data (make-data "name" "Bob" "age" 25)))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok valid-p)
      (ok (null errors)))))

;;; Missing required field

(deftest missing-required-field
  (let ((data (make-data "name" "Alice")))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok (not valid-p))
      (ok (= 1 (length errors)))
      (ok (string= "age" (validation-error-field-name (first errors)))))))

;;; Wrong type

(deftest wrong-type-string-for-integer
  (let ((data (make-data "name" "Alice" "age" "twenty")))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

;;; Invalid enum

(deftest invalid-enum-value
  (let ((data (make-data "name" "Alice" "age" 30 "status" "unknown")))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

;;; Null for non-nullable

(deftest null-for-required-field
  (let ((data (make-data "name" :null "age" 30)))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok (not valid-p)))))

;;; Null for nullable — ok

(deftest null-for-nullable-field
  (let ((data (make-data "name" "Alice" "age" 30 "email" :null)))
    (multiple-value-bind (valid-p errors)
        (validate-data data (person-schema))
      (ok valid-p)
      (ok (null errors)))))

;;; Array field validates lists (not just vectors)

(deftest array-field-validates-list
  (let* ((schema (make-ir-schema
                  :name "tags"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list
                                                 :element-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" '("a" "b" "c"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

;;; Date validation

(deftest valid-date-passes
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "date"
                                          :type (make-ir-type-date :format :date)
                                          :required-p t
                                          :slot-name 'event-date))))
         (data (make-data "date" "2026-07-25")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest valid-date-leap-year
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "date"
                                          :type (make-ir-type-date :format :date)
                                          :required-p t
                                          :slot-name 'event-date))))
         (data (make-data "date" "2024-02-29")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest invalid-date-bad-month
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "date"
                                          :type (make-ir-type-date :format :date)
                                          :required-p t
                                          :slot-name 'event-date))))
         (data (make-data "date" "2026-13-01")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest invalid-date-bad-day
  (testing "2026-02-29 is invalid — 2026 is not a leap year"
    (let* ((schema (make-ir-schema
                    :name "event"
                    :fields (list
                             (make-ir-field :name "date"
                                            :type (make-ir-type-date :format :date)
                                            :required-p t
                                            :slot-name 'event-date))))
           (data (make-data "date" "2026-02-29")))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (= 1 (length errors)))))))

(deftest invalid-date-malformed
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "date"
                                          :type (make-ir-type-date :format :date)
                                          :required-p t
                                          :slot-name 'event-date))))
         (data (make-data "date" "not-a-date")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest invalid-date-non-string
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "date"
                                          :type (make-ir-type-date :format :date)
                                          :required-p t
                                          :slot-name 'event-date))))
         (data (make-data "date" 42)))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

;;; Date-time validation

(deftest valid-date-time-with-z
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "ts"
                                          :type (make-ir-type-date :format :date-time)
                                          :required-p t
                                          :slot-name 'ts))))
         (data (make-data "ts" "2026-07-25T10:30:00Z")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest valid-date-time-with-offset
  (let* ((schema (make-ir-schema
                  :name "event"
                  :fields (list
                           (make-ir-field :name "ts"
                                          :type (make-ir-type-date :format :date-time)
                                          :required-p t
                                          :slot-name 'ts))))
         (data (make-data "ts" "2026-07-25T10:30:00+05:30")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest invalid-date-time-missing-timezone
  (testing "RFC 3339 date-time requires a timezone designator"
    (let* ((schema (make-ir-schema
                    :name "event"
                    :fields (list
                             (make-ir-field :name "ts"
                                            :type (make-ir-type-date :format :date-time)
                                            :required-p t
                                            :slot-name 'ts))))
           (data (make-data "ts" "2026-07-25T10:30:00")))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (= 1 (length errors)))))))

;;; Nested validation

(deftest nested-validation-errors
  (let* ((address-schema (make-ir-schema
                          :name "address"
                          :fields (list
                                   (make-ir-field :name "street"
                                                  :type (make-ir-type-primitive :kind :string)
                                                  :required-p t
                                                  :slot-name 'street))))
         (schema (make-ir-schema
                  :name "employee"
                  :fields (list
                           (make-ir-field :name "address"
                                          :type (make-ir-type-object :schema address-schema)
                                          :required-p t
                                          :slot-name 'address))))
         (data (make-data "address" (make-data))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

;;; Map validation

(deftest map-field-validates-hash-table
  (let* ((schema (make-ir-schema
                  :name "config"
                  :fields (list
                           (make-ir-field :name "settings"
                                          :type (make-ir-type-map
                                                 :value-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'settings))))
         (data (make-data "settings" (make-data "key1" "val1" "key2" "val2"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest map-field-rejects-non-hash-table
  (let* ((schema (make-ir-schema
                  :name "config"
                  :fields (list
                           (make-ir-field :name "settings"
                                          :type (make-ir-type-map
                                                 :value-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'settings))))
         (data (make-data "settings" "not-a-map")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest map-field-rejects-wrong-value-type
  (let* ((schema (make-ir-schema
                  :name "config"
                  :fields (list
                           (make-ir-field :name "settings"
                                          :type (make-ir-type-map
                                                 :value-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'settings))))
         (data (make-data "settings" (make-data "key1" "valid" "key2" 42))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (= 1 (length errors))))))

(deftest map-field-accepts-empty-map
  (let* ((schema (make-ir-schema
                  :name "config"
                  :fields (list
                           (make-ir-field :name "settings"
                                          :type (make-ir-type-map
                                                 :value-type (make-ir-type-primitive :kind :string))
                                          :required-p t
                                          :slot-name 'settings))))
         (data (make-data "settings" (make-data))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))
