(defpackage #:clos-alchemy/tests/telemetry
  (:use #:cl #:rove #:clos-alchemy #:clos-alchemy/telemetry)
  (:import-from #:cl-llm-backend
                #:make-mock-backend))

(in-package #:clos-alchemy/tests/telemetry)

;;; ── Test class ────────────────────────────────────────────────────

(defclass logged-item ()
  ((name  :initarg :name  :accessor logged-item-name  :type string)
   (count :initarg :count :accessor logged-item-count :type integer)))

(defun good-data ()
  "{\"name\":\"widget\",\"count\":5}")

(defun bad-data-wrong-type ()
  "{\"name\":\"widget\",\"count\":\"five\"}")

;;; ── Tests ─────────────────────────────────────────────────────────

(deftest with-extraction-logging-returns-the-body-value
  (testing "the macro is transparent to its body's return value"
    (let ((backend (make-mock-backend :responses (list (good-data)))))
      (let ((result (with-extraction-logging (*standard-output*)
                      (extract backend 'logged-item "text"))))
        (ok (typep (extraction-result-instance result) 'logged-item))))))

(deftest with-extraction-logging-writes-one-line-per-signaled-event
  (testing "one failing attempt then success logs attempt+retry+attempt"
    (let ((backend (make-mock-backend
                    :responses (list (bad-data-wrong-type) (good-data))))
          (log (make-string-output-stream)))
      (with-extraction-logging (log)
        (extract backend 'logged-item "text" :max-retries 3))
      (ok (= 3 (count #\Newline (get-output-stream-string log)))))))

(deftest with-extraction-logging-logs-exhaustion-on-terminal-failure
  (testing "a single doomed attempt logs the attempt and the exhaustion, then still raises"
    (let ((backend (make-mock-backend :responses (list (bad-data-wrong-type))))
          (log (make-string-output-stream)))
      (handler-case
          (with-extraction-logging (log)
            (extract backend 'logged-item "text" :max-retries 0))
        (max-retries-error ()
          (ok (= 2 (count #\Newline (get-output-stream-string log)))))))))
