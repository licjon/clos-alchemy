(defpackage #:clos-alchemy/tests/custom-validation
  (:use #:cl #:rove #:clos-alchemy)
  (:import-from #:cl-llm-backend
                #:make-mock-backend #:mock-backend-calls))

(in-package #:clos-alchemy/tests/custom-validation)

;;; ── Helpers ──────────────────────────────────────────────────────────

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

;;; ══════════════════════════════════════════════════════════════════════
;;; PART 1: Metaclass — :validate slot option
;;; ══════════════════════════════════════════════════════════════════════

(defclass validated-item ()
  ((amount :initarg :amount :type integer
           :validate (lambda (v) (if (plusp v) t "amount must be positive")))
   (name   :initarg :name :type string))
  (:metaclass constructor-class))

(deftest metaclass-accepts-validate-option
  (testing ":validate is stored on the direct slot definition"
    (let* ((class (find-class 'validated-item))
           (slots (closer-mop:class-direct-slots class))
           (amount-slot (find 'amount slots
                              :key #'closer-mop:slot-definition-name)))
      (ok amount-slot "amount slot exists")
      (ok (slot-definition-validate amount-slot)
          "validate accessor returns the predicate"))))

(deftest metaclass-propagates-validate-to-effective-slot
  (testing ":validate propagates to effective slot definition"
    (let* ((class (find-class 'validated-item))
           (_ (closer-mop:ensure-finalized class))
           (slots (closer-mop:class-slots class))
           (amount-slot (find 'amount slots
                              :key #'closer-mop:slot-definition-name)))
      (declare (ignore _))
      (ok amount-slot "effective amount slot exists")
      (ok (slot-definition-validate amount-slot)
          "validate propagates to effective slot"))))

;;; ══════════════════════════════════════════════════════════════════════
;;; PART 2: IR — validator propagation
;;; ══════════════════════════════════════════════════════════════════════

(deftest introspection-captures-validator
  (testing "class-to-schema stores validator in ir-field"
    (let* ((schema (class-to-schema 'validated-item))
           (fields (ir-schema-fields schema))
           (amount-field (find "amount" fields :key #'ir-field-name :test #'string=))
           (name-field (find "name" fields :key #'ir-field-name :test #'string=)))
      (ok (ir-field-validate amount-field) "amount field has a validator")
      (ok (null (ir-field-validate name-field)) "name field has no validator"))))

;;; ══════════════════════════════════════════════════════════════════════
;;; PART 3: Per-slot validation — validate-data with predicates
;;; ══════════════════════════════════════════════════════════════════════

(deftest per-slot-validator-passes-when-predicate-returns-t
  (testing "a predicate returning T means the field is valid"
    (let* ((schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "value"
                              :type (make-ir-type-primitive :kind :integer)
                              :required-p t
                              :validate (lambda (v) (if (plusp v) t "must be positive"))))))
           (data (make-data "value" 5)))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok valid-p)
        (ok (null errors))))))

(deftest per-slot-validator-fails-when-predicate-returns-string
  (testing "a predicate returning a string means validation failure"
    (let* ((schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "value"
                              :type (make-ir-type-primitive :kind :integer)
                              :required-p t
                              :validate (lambda (v)
                                          (declare (ignore v))
                                          "value is invalid")))))
           (data (make-data "value" 5)))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p) "validation fails")
        (ok (= 1 (length errors)) "one error")
        (ok (string= "value" (validation-error-field-name (first errors))))))))

(deftest per-slot-validator-receives-coerced-value
  (testing "validator receives the Lisp-coerced value, not raw JSON"
    (let* ((received nil)
           (schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "status"
                              :type (make-ir-type-enum :values '("active" "inactive"))
                              :required-p t
                              :validate (lambda (v)
                                          (setf received v)
                                          t)))))
           (data (make-data "status" "active")))
      (validate-data data schema)
      (ok (eq :active received)
          "validator received keyword :ACTIVE, not string \"active\""))))

(deftest per-slot-validator-not-called-for-missing-optional
  (testing "validator does not fire when an optional field is absent"
    (let* ((called nil)
           (schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "opt"
                              :type (make-ir-type-primitive :kind :string)
                              :required-p nil
                              :validate (lambda (v)
                                          (declare (ignore v))
                                          (setf called t)
                                          t)))))
           (data (make-data)))
      (validate-data data schema)
      (ok (not called) "validator was not called"))))

(deftest per-slot-validator-not-called-for-null-on-nullable
  (testing "validator does not fire when a nullable field is null"
    (let* ((called nil)
           (schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "opt"
                              :type (make-ir-type-nullable
                                     :inner-type (make-ir-type-primitive :kind :string))
                              :required-p nil
                              :nullable-p t
                              :validate (lambda (v)
                                          (declare (ignore v))
                                          (setf called t)
                                          t)))))
           (data (make-data "opt" :null)))
      (validate-data data schema)
      (ok (not called) "validator was not called for :null"))))

(deftest per-slot-validator-not-called-when-type-check-fails
  (testing "if the type check already fails, the predicate is not invoked"
    (let* ((called nil)
           (schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "n"
                              :type (make-ir-type-primitive :kind :integer)
                              :required-p t
                              :validate (lambda (v)
                                          (declare (ignore v))
                                          (setf called t)
                                          t)))))
           (data (make-data "n" "not-a-number")))
      (validate-data data schema)
      (ok (not called) "validator not called after type mismatch"))))

(deftest multiple-slot-validators-independent
  (testing "validators on different slots are independent"
    (let* ((schema (make-ir-schema
                    :name "test"
                    :fields (list
                             (make-ir-field
                              :name "a"
                              :type (make-ir-type-primitive :kind :integer)
                              :required-p t
                              :validate (lambda (v) (if (plusp v) t "a must be positive")))
                             (make-ir-field
                              :name "b"
                              :type (make-ir-type-primitive :kind :integer)
                              :required-p t
                              :validate (lambda (v) (if (evenp v) t "b must be even"))))))
           (data (make-data "a" -1 "b" 3)))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (= 2 (length errors)) "both validators fail independently")))))

;;; ══════════════════════════════════════════════════════════════════════
;;; PART 4: Whole-object validation — validate-instance
;;; ══════════════════════════════════════════════════════════════════════

(defclass date-range ()
  ((start-date :initarg :start-date :type string)
   (end-date   :initarg :end-date   :type string))
  (:metaclass constructor-class))

(defmethod validate-instance ((obj date-range))
  (if (string< (slot-value obj 'end-date) (slot-value obj 'start-date))
      (list "end_date must not be before start_date")
      '()))

(deftest validate-instance-default-returns-empty-list
  (testing "the base method returns no errors"
    (let ((obj (make-instance 'validated-item :amount 5 :name "x")))
      (ok (null (validate-instance obj))))))

(deftest validate-instance-specialized-returns-errors
  (testing "a specialized method returns error strings for cross-field issues"
    (let ((obj (make-instance 'date-range :start-date "2024-06-01"
                                          :end-date "2024-01-01")))
      (let ((errors (validate-instance obj)))
        (ok (= 1 (length errors)))
        (ok (stringp (first errors)))))))

(deftest validate-instance-passes-when-constraints-met
  (testing "no errors when cross-field constraint is satisfied"
    (let ((obj (make-instance 'date-range :start-date "2024-01-01"
                                          :end-date "2024-06-01")))
      (ok (null (validate-instance obj))))))

;;; ══════════════════════════════════════════════════════════════════════
;;; PART 5: Integration — retry on custom validation failure
;;; ══════════════════════════════════════════════════════════════════════

(defclass positive-item ()
  ((name   :initarg :name   :type string)
   (amount :initarg :amount :type integer
           :validate (lambda (v) (if (plusp v) t "amount must be positive"))))
  (:metaclass constructor-class))

(deftest extract-retries-on-slot-validator-failure
  (testing "per-slot predicate failure triggers a retry"
    (let* ((bad  "{\"name\":\"widget\",\"amount\":-5}")
           (good "{\"name\":\"widget\",\"amount\":10}")
           (backend (make-mock-backend :responses (list bad good)))
           (result (extract backend 'positive-item "text")))
      (ok (typep (extraction-result-instance result) 'positive-item))
      (ok (= 1 (extraction-result-retries result)))
      (ok (= 2 (mock-backend-calls backend))))))

(deftest extract-retries-on-instance-validator-failure
  (testing "validate-instance failure triggers a retry"
    (let* ((bad  "{\"start_date\":\"2024-06-01\",\"end_date\":\"2024-01-01\"}")
           (good "{\"start_date\":\"2024-01-01\",\"end_date\":\"2024-06-01\"}")
           (backend (make-mock-backend :responses (list bad good)))
           (result (extract backend 'date-range "text")))
      (ok (typep (extraction-result-instance result) 'date-range))
      (ok (= 1 (extraction-result-retries result)))
      (ok (= 2 (mock-backend-calls backend))))))

(deftest extract-validator-errors-in-max-retries-error
  (testing "custom validation errors appear in max-retries-error"
    (let* ((bad "{\"name\":\"widget\",\"amount\":-5}")
           (backend (make-mock-backend :responses (list bad bad))))
      (handler-case
          (progn (extract backend 'positive-item "text" :max-retries 1) nil)
        (max-retries-error (e)
          (ok (some (lambda (err)
                      (search "amount must be positive" (princ-to-string err)))
                    (max-retries-error-errors e))
              "custom error message is in the error list"))))))

(deftest extract-no-retry-when-validators-pass
  (testing "when all validators pass, no retry happens"
    (let* ((good "{\"name\":\"widget\",\"amount\":10}")
           (backend (make-mock-backend :responses (list good)))
           (result (extract backend 'positive-item "text")))
      (ok (= 0 (extraction-result-retries result)))
      (ok (= 1 (mock-backend-calls backend))))))
