(defpackage #:clos-alchemy/tests/extract-value
  (:use #:cl #:rove #:clos-alchemy)
  (:import-from #:cl-llm-backend
                #:make-mock-backend #:mock-backend-calls))

(in-package #:clos-alchemy/tests/extract-value)

;;; Tests for extract-value — top-level primitive / unwrapped extraction

(deftest extract-value/integer
  (testing "extracts an integer and unwraps it from the wrapper"
    (let* ((backend (make-mock-backend :responses (list "{\"value\":42}")))
           (result (extract-value backend 'integer "the answer is 42")))
      (ok (= 42 (extraction-result-instance result)))
      (ok (= 0 (extraction-result-retries result)))
      (ok (= 1 (mock-backend-calls backend))))))

(deftest extract-value/string
  (testing "extracts a string value"
    (let* ((backend (make-mock-backend :responses (list "{\"value\":\"hello\"}")))
           (result (extract-value backend 'string "say hello")))
      (ok (string= "hello" (extraction-result-instance result))))))

(deftest extract-value/boolean-true
  (testing "extracts boolean true"
    (let* ((backend (make-mock-backend :responses (list "{\"value\":true}")))
           (result (extract-value backend 'boolean "is it sarcastic?")))
      (ok (eq t (extraction-result-instance result))))))

(deftest extract-value/boolean-false
  (testing "extracts boolean false — instance is nil but extraction succeeded"
    (let* ((backend (make-mock-backend :responses (list "{\"value\":false}")))
           (result (extract-value backend 'boolean "this is sincere")))
      (ok (extraction-result-p result))
      (ok (eq nil (extraction-result-instance result)))
      (ok (= 0 (extraction-result-retries result))))))

(deftest extract-value/enum
  (testing "extracts an enum from (member ...) and returns a keyword"
    (let* ((backend (make-mock-backend
                     :responses (list "{\"value\":\"positive\"}")))
           (result (extract-value backend '(member :positive :negative :neutral)
                                  "I love this product!")))
      (ok (eq :positive (extraction-result-instance result))))))

(deftest extract-value/vector-of-strings
  (testing "extracts a vector of strings via (vector string)"
    (let* ((backend (make-mock-backend
                     :responses (list "{\"value\":[\"red\",\"green\",\"blue\"]}")))
           (result (extract-value backend '(vector string)
                                  "colors: red, green, blue")))
      (let ((instance (extraction-result-instance result)))
        (ok (typep instance 'vector))
        (ok (= 3 (length instance)))
        (ok (string= "red" (aref instance 0)))
        (ok (string= "blue" (aref instance 2)))))))

(deftest extract-value/retries-on-invalid-data
  (testing "retries when validation fails then succeeds"
    (let* ((backend (make-mock-backend
                     :responses (list "{\"value\":\"not-a-number\"}"
                                      "{\"value\":42}")))
           (result (extract-value backend 'integer "the answer")))
      (ok (= 42 (extraction-result-instance result)))
      (ok (= 1 (extraction-result-retries result)))
      (ok (= 2 (mock-backend-calls backend))))))

(deftest extract-value/max-retries-error
  (testing "signals max-retries-error when all attempts fail"
    (let ((backend (make-mock-backend
                    :responses (list "{\"value\":\"bad\"}"
                                     "{\"value\":\"bad\"}"))))
      (ok (handler-case
              (progn (extract-value backend 'integer "text" :max-retries 1) nil)
            (max-retries-error (e)
              (= 1 (max-retries-error-retries e))))))))

(deftest extract-value/compound-type-constraints
  (testing "(integer 0 255) rejects out-of-range values and retries"
    (let* ((backend (make-mock-backend
                     :responses (list "{\"value\":256}"
                                      "{\"value\":200}")))
           (result (extract-value backend '(integer 0 255) "pick a byte")))
      (ok (= 200 (extraction-result-instance result)))
      (ok (= 1 (extraction-result-retries result))))))

(deftest extract-value/user-prompt-forwarded
  (testing "user-prompt keyword is passed through to the extraction"
    (let* ((backend (make-mock-backend :responses (list "{\"value\":true}")))
           (result (extract-value backend 'boolean "text"
                                  :user-prompt "Is this sarcastic?")))
      (ok (eq t (extraction-result-instance result)))
      (ok (= 0 (extraction-result-retries result))))))

(deftest extract-value/schema-error-on-type-t
  (testing "type T signals schema-error immediately"
    (ok (handler-case
            (progn (extract-value (make-mock-backend :responses (list "{}")) t "text") nil)
          (schema-error () t)))))

(deftest extract-value/schema-error-on-type-nil
  (testing "type NIL signals schema-error immediately"
    (ok (handler-case
            (progn (extract-value (make-mock-backend :responses (list "{}")) nil "text") nil)
          (schema-error () t)))))
