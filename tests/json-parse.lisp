(defpackage #:clos-constructor/tests/json-parse
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/tests/json-parse)

(deftest parse-clean-json
  (let ((result (parse-json-response "{\"name\": \"Alice\", \"age\": 30}")))
    (ok (hash-table-p result))
    (ok (string= "Alice" (gethash "name" result)))
    (ok (= 30 (gethash "age" result)))))

(deftest parse-with-markdown-fences
  (let ((result (parse-json-response "```json
{\"name\": \"Bob\"}
```")))
    (ok (hash-table-p result))
    (ok (string= "Bob" (gethash "name" result)))))

(deftest parse-with-leading-text
  (let ((result (parse-json-response "Here is the result: {\"name\": \"Carol\"}")))
    (ok (hash-table-p result))
    (ok (string= "Carol" (gethash "name" result)))))

(deftest parse-with-whitespace
  (let ((result (parse-json-response "
  {\"name\": \"Dave\"}
  ")))
    (ok (hash-table-p result))
    (ok (string= "Dave" (gethash "name" result)))))

(deftest parse-array
  (let ((result (parse-json-response "[{\"name\": \"Eve\"}, {\"name\": \"Frank\"}]")))
    (ok (listp result))
    (ok (= 2 (length result)))))

(deftest parse-arrays-as-lists-not-vectors
  (testing "JSON arrays are parsed as lists so validation and construction work"
    (let* ((result (parse-json-response "{\"tags\": [\"a\", \"b\", \"c\"]}"))
           (tags (gethash "tags" result)))
      (ok (listp tags))
      (ok (not (vectorp tags)))
      (ok (equal '("a" "b" "c") tags))))
  (testing "nested arrays are also lists"
    (let* ((result (parse-json-response "{\"matrix\": [[1, 2], [3, 4]]}"))
           (matrix (gethash "matrix" result)))
      (ok (listp matrix))
      (ok (listp (first matrix)))
      (ok (equal '((1 2) (3 4)) matrix)))))

(deftest parse-invalid-json-signals-error
  (ok (handler-case
          (progn (parse-json-response "not json at all {broken") nil)
        (generation-error () t))))
