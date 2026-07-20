(defpackage #:clos-alchemy/tests/extract
  (:use #:cl #:rove #:clos-alchemy)
  (:import-from #:cl-llm-backend
                #:make-mock-backend #:mock-backend-calls))

(in-package #:clos-alchemy/tests/extract)

;;; The protocol mock (cl-llm-backend) returns canned response strings;
;;; %extract-with-retry parses them itself, so scripted responses are
;;; plain JSON strings: bad data N times, then good data.

;;; ── Test class ────────────────────────────────────────────────────

(defclass item ()
  ((name  :initarg :name  :accessor item-name  :type string)
   (count :initarg :count :accessor item-count :type integer)))

(defun good-data ()
  "{\"name\":\"widget\",\"count\":5}")

(defun bad-data-wrong-type ()
  "{\"name\":\"widget\",\"count\":\"five\"}")

(defun bad-data-missing-field ()
  "{\"name\":\"widget\"}")

;;; ── Tests ─────────────────────────────────────────────────────────

(defclass flagged ()
  ((name   :initarg :name   :accessor flagged-name   :type string)
   (active :initarg :active :accessor flagged-active :type boolean)))

(deftest boolean-false-survives-extraction
  (testing "a required boolean field whose value is false validates and constructs"
    (let* ((raw "{\"name\":\"widget\",\"active\":false}")
           (backend (make-mock-backend
                     :responses (list raw)))
           (result (extract backend 'flagged "text")))
      (ok (typep (extraction-result-instance result) 'flagged))
      (ok (eq nil (flagged-active (extraction-result-instance result))))
      (ok (= 0 (extraction-result-retries result)))))
  (testing "true coerces to t"
    (let* ((raw "{\"name\":\"widget\",\"active\":true}")
           (backend (make-mock-backend
                     :responses (list raw)))
           (result (extract backend 'flagged "text")))
      (ok (eq t (flagged-active (extraction-result-instance result)))))))

(deftest succeeds-on-first-attempt
  (let* ((backend (make-mock-backend :responses (list (good-data))))
         (result (extract backend 'item "text")))
    (ok (typep (extraction-result-instance result) 'item))
    (ok (string= "widget" (item-name (extraction-result-instance result))))
    (ok (= 5 (item-count (extraction-result-instance result))))
    (ok (= 0 (extraction-result-retries result)))
    (ok (= 1 (mock-backend-calls backend)))))

(deftest retries-on-validation-failure-then-succeeds
  (let* ((backend (make-mock-backend
                   :responses (list (bad-data-wrong-type)
                                    (good-data))))
         (result (extract backend 'item "text")))
    (ok (typep (extraction-result-instance result) 'item))
    (ok (= 1 (extraction-result-retries result)))
    (ok (= 2 (mock-backend-calls backend)))))

(deftest retries-multiple-times-then-succeeds
  (let* ((backend (make-mock-backend
                   :responses (list (bad-data-wrong-type)
                                    (bad-data-missing-field)
                                    (bad-data-wrong-type)
                                    (good-data))))
         (result (extract backend 'item "text" :max-retries 3)))
    (ok (typep (extraction-result-instance result) 'item))
    (ok (= 3 (extraction-result-retries result)))
    (ok (= 4 (mock-backend-calls backend)))))

(deftest signals-max-retries-error-when-exhausted
  (let ((backend (make-mock-backend
                  :responses (list (bad-data-wrong-type)
                                   (bad-data-wrong-type)
                                   (bad-data-wrong-type)
                                   (bad-data-wrong-type)))))
    (ok (handler-case
            (progn (extract backend 'item "text" :max-retries 3) nil)
          (max-retries-error (e)
            (and (= 3 (max-retries-error-retries e))
                 (= 4 (length (max-retries-error-errors e)))))))))

(deftest max-retries-error-accumulates-errors-across-attempts
  (let ((backend (make-mock-backend
                  :responses (list (bad-data-wrong-type)
                                   (bad-data-missing-field)))))
    (ok (handler-case
            (progn (extract backend 'item "text" :max-retries 1) nil)
          (max-retries-error (e)
            (= 2 (length (max-retries-error-errors e))))))))

(deftest max-retries-zero-means-one-attempt
  (testing "max-retries 0 means no retries — just the initial attempt"
    (let ((backend (make-mock-backend
                    :responses (list (bad-data-wrong-type)))))
      (ok (handler-case
              (progn (extract backend 'item "text" :max-retries 0) nil)
            (max-retries-error (e)
              (and (= 0 (max-retries-error-retries e))
                   (= 1 (mock-backend-calls backend)))))))))

(deftest retries-record-correct-attempt-number
  (testing "extraction-result-retries reflects the attempt that succeeded"
    (let* ((backend (make-mock-backend
                     :responses (list (bad-data-wrong-type)
                                      (bad-data-missing-field)
                                      (good-data))))
           (result (extract backend 'item "text" :max-retries 5)))
      (ok (= 2 (extraction-result-retries result))))))

(deftest auto-compiles-from-class-name
  (testing "passing a symbol instead of a compilation works"
    (let* ((backend (make-mock-backend :responses (list (good-data))))
           (result (extract backend 'item "text")))
      (ok (typep (extraction-result-instance result) 'item)))))

(deftest extract-list-returns-list-of-instances
  (testing "the instance slot is the list itself, not the items wrapper"
    (let* ((raw "{\"items\":[{\"name\":\"widget\",\"count\":5},{\"name\":\"gadget\",\"count\":2}]}")
           (backend (make-mock-backend
                     :responses (list raw)))
           (result (extract-list backend 'item "text"))
           (instances (extraction-result-instance result)))
      (ok (listp instances))
      (ok (= 2 (length instances)))
      (ok (every (lambda (i) (typep i 'item)) instances))
      (ok (string= "widget" (item-name (first instances))))
      (ok (= 2 (item-count (second instances)))))))

(deftest extract-list-with-no-matches-returns-empty-list
  (testing "an empty items array is valid and yields nil, not a validation failure"
    (let* ((raw "{\"items\":[]}")
           (backend (make-mock-backend
                     :responses (list raw)))
           (result (extract-list backend 'item "text")))
      (ok (null (extraction-result-instance result)))
      (ok (= 0 (extraction-result-retries result))))))

(deftest pre-compiled-extractor-works
  (testing "compile-extractor result can be reused across calls"
    (let* ((compilation (compile-extractor 'item))
           (backend (make-mock-backend
                     :responses (list (good-data) (good-data))))
           (r1 (extract backend compilation "text1"))
           (r2 (extract backend compilation "text2")))
      (ok (typep (extraction-result-instance r1) 'item))
      (ok (typep (extraction-result-instance r2) 'item))
      (ok (= 2 (mock-backend-calls backend))))))
