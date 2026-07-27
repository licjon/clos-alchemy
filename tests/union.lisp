(defpackage #:clos-alchemy/tests/union
  (:use #:cl #:rove #:clos-alchemy))

(in-package #:clos-alchemy/tests/union)

;;; ── Test classes for discriminated unions ──

(defclass query-result ()
  ((kind :initarg :kind :type (member :query))
   (q :initarg :q :type string)))

(defclass refusal-result ()
  ((kind :initarg :kind :type (member :refusal))
   (reason :initarg :reason :type string)))

(defclass bad-discriminator ()
  ((kind :initarg :kind :type string)
   (data :initarg :data :type string)))

(defclass multi-value-discriminator ()
  ((kind :initarg :kind :type (member :a :b))
   (data :initarg :data :type string)))

(defclass wrapper ()
  ((label :initarg :label :type string)
   (result :initarg :result :initform nil)))

;;; ── Helpers ──

(defun make-data (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun ht-get (obj key)
  (etypecase obj
    (hash-table (gethash key obj))
    (ordered-map (ordered-map-get obj key))))

(defun make-two-branch-union ()
  (let ((query-schema (class-to-schema 'query-result))
        (refusal-schema (class-to-schema 'refusal-result)))
    (make-ir-type-union
     :discriminator "kind"
     :branches (list (cons "query" query-schema)
                     (cons "refusal" refusal-schema)))))

(defun make-wrapper-schema (union-type)
  (make-ir-schema
   :name "response"
   :fields (list
            (make-ir-field :name "result"
                           :type union-type
                           :required-p t
                           :slot-name 'result))))

;;; ══════════════════════════════════════════════════════════════
;;; IR struct
;;; ══════════════════════════════════════════════════════════════

(deftest ir-type-union/struct-exists
  (let ((u (make-two-branch-union)))
    (ok (ir-type-union-p u))
    (ok (string= "kind" (ir-type-union-discriminator u)))
    (ok (= 2 (length (ir-type-union-branches u))))))

(deftest ir-type-union/branch-contents
  (let* ((u (make-two-branch-union))
         (first-branch (first (ir-type-union-branches u)))
         (second-branch (second (ir-type-union-branches u))))
    (ok (string= "query" (car first-branch)))
    (ok (ir-schema-p (cdr first-branch)))
    (ok (string= "refusal" (car second-branch)))
    (ok (ir-schema-p (cdr second-branch)))))

;;; ══════════════════════════════════════════════════════════════
;;; JSON Schema emission
;;; ══════════════════════════════════════════════════════════════

(deftest union-schema/emits-anyof
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (result-js (ht-get props "result")))
    (ok (listp (ht-get result-js "anyOf")))
    (ok (= 2 (length (ht-get result-js "anyOf"))))))

(deftest union-schema/branches-are-objects-with-strict-mode
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (js (schema-to-json-schema schema))
         (props (ht-get js "properties"))
         (result-js (ht-get props "result"))
         (branches (ht-get result-js "anyOf")))
    (dolist (branch branches)
      (ok (string= "object" (ht-get branch "type")))
      (ok (eq 'yason:false (ht-get branch "additionalProperties"))))))

(deftest union-schema/discriminator-is-single-value-enum
  (testing "each branch constrains the discriminator to a single-value enum, not const"
    (let* ((u (make-two-branch-union))
           (schema (make-wrapper-schema u))
           (js (schema-to-json-schema schema))
           (props (ht-get js "properties"))
           (result-js (ht-get props "result"))
           (branches (ht-get result-js "anyOf"))
           (query-branch (first branches))
           (refusal-branch (second branches)))
      (let ((kind-js (ht-get (ht-get query-branch "properties") "kind")))
        (ok (string= "string" (ht-get kind-js "type")))
        (ok (equal '("query") (ht-get kind-js "enum"))))
      (let ((kind-js (ht-get (ht-get refusal-branch "properties") "kind")))
        (ok (string= "string" (ht-get kind-js "type")))
        (ok (equal '("refusal") (ht-get kind-js "enum")))))))

(deftest union-schema/all-fields-in-required
  (testing "each branch lists all properties in required (OpenAI strict mode)"
    (let* ((u (make-two-branch-union))
           (schema (make-wrapper-schema u))
           (js (schema-to-json-schema schema))
           (props (ht-get js "properties"))
           (result-js (ht-get props "result"))
           (query-branch (first (ht-get result-js "anyOf")))
           (required (ht-get query-branch "required")))
      (ok (member "kind" required :test #'string=))
      (ok (member "q" required :test #'string=)))))

(deftest union-schema/properties-are-ordered-maps
  (testing "each branch uses ordered-map for properties"
    (let* ((u (make-two-branch-union))
           (schema (make-wrapper-schema u))
           (js (schema-to-json-schema schema))
           (props (ht-get js "properties"))
           (result-js (ht-get props "result"))
           (branches (ht-get result-js "anyOf")))
      (dolist (branch branches)
        (ok (ordered-map-p (ht-get branch "properties")))))))

(deftest union-schema/serialization-roundtrip
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (js (schema-to-json-schema schema))
         (json-string (with-output-to-string (s) (yason:encode js s)))
         (parsed (yason:parse json-string))
         (result-schema (gethash "result" (gethash "properties" parsed))))
    (ok (listp (gethash "anyOf" result-schema)))
    (ok (= 2 (length (gethash "anyOf" result-schema))))))

;;; ══════════════════════════════════════════════════════════════
;;; Validation
;;; ══════════════════════════════════════════════════════════════

(deftest union-validation/valid-first-branch
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" (make-data "kind" "query" "q" "what is lisp?"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest union-validation/valid-second-branch
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" (make-data "kind" "refusal"
                                              "reason" "not enough context"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest union-validation/missing-discriminator
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" (make-data "q" "what is lisp?"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (>= (length errors) 1)))))

(deftest union-validation/unknown-discriminator-value
  (testing "error message names the allowed discriminator values"
    (let* ((u (make-two-branch-union))
           (schema (make-wrapper-schema u))
           (data (make-data "result" (make-data "kind" "unknown" "q" "hi"))))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (>= (length errors) 1))
        (let ((msg (format nil "~A" (first errors))))
          (ok (search "query" msg))
          (ok (search "refusal" msg)))))))

(deftest union-validation/wrong-field-type-in-matched-branch
  (testing "validates against matched branch and reports per-field errors"
    (let* ((u (make-two-branch-union))
           (schema (make-wrapper-schema u))
           (data (make-data "result" (make-data "kind" "query" "q" 42))))
      (multiple-value-bind (valid-p errors)
          (validate-data data schema)
        (ok (not valid-p))
        (ok (>= (length errors) 1))))))

(deftest union-validation/non-hash-table
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" "not-an-object")))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok (not valid-p))
      (ok (>= (length errors) 1)))))

;;; ══════════════════════════════════════════════════════════════
;;; Construction
;;; ══════════════════════════════════════════════════════════════

(deftest union-construction/first-branch
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" (make-data "kind" "query" "q" "what is lisp?")))
         (result (construct-from-data data schema))
         (instance (gethash "result" result)))
    (ok (typep instance 'query-result))
    (ok (eq :query (slot-value instance 'kind)))
    (ok (string= "what is lisp?" (slot-value instance 'q)))))

(deftest union-construction/second-branch
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (data (make-data "result" (make-data "kind" "refusal"
                                              "reason" "not enough context")))
         (result (construct-from-data data schema))
         (instance (gethash "result" result)))
    (ok (typep instance 'refusal-result))
    (ok (eq :refusal (slot-value instance 'kind)))
    (ok (string= "not enough context" (slot-value instance 'reason)))))

;;; ══════════════════════════════════════════════════════════════
;;; Prompt generation
;;; ══════════════════════════════════════════════════════════════

(deftest union-prompt/type-label-mentions-alternatives
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (prompt (generate-system-prompt schema)))
    (ok (search "query_result" prompt))
    (ok (search "refusal_result" prompt))))

(deftest union-prompt/mentions-discriminator
  (let* ((u (make-two-branch-union))
         (schema (make-wrapper-schema u))
         (prompt (generate-system-prompt schema)))
    (ok (search "kind" prompt))))

;;; ══════════════════════════════════════════════════════════════
;;; Top-level API: compile-union-extractor
;;; ══════════════════════════════════════════════════════════════

(deftest compile-union/produces-compilation
  (let ((comp (compile-union-extractor '(query-result refusal-result)
                                       :discriminator 'kind)))
    (ok (extraction-compilation-p comp))
    (ok (ir-schema-p (extraction-compilation-schema comp)))
    (ok (stringp (extraction-compilation-prompt comp)))))

(deftest compile-union/schema-has-union-field
  (let* ((comp (compile-union-extractor '(query-result refusal-result)
                                        :discriminator 'kind))
         (schema (extraction-compilation-schema comp))
         (fields (ir-schema-fields schema)))
    (ok (= 1 (length fields)))
    (ok (string= "result" (ir-field-name (first fields))))
    (ok (ir-type-union-p (ir-field-type (first fields))))))

(deftest compile-union/errors-on-duplicate-discriminator
  (testing "two classes with the same discriminator value signal schema-error"
    (ok (handler-case
            (progn
              (compile-union-extractor '(query-result query-result)
                                       :discriminator 'kind)
              nil)
          (schema-error () t)))))

(deftest compile-union/errors-on-missing-discriminator-slot
  (testing "class without the named discriminator slot signals schema-error"
    (ok (handler-case
            (progn
              (compile-union-extractor '(query-result refusal-result)
                                       :discriminator 'nonexistent)
              nil)
          (schema-error () t)))))

(deftest compile-union/errors-on-non-enum-discriminator
  (testing "discriminator slot with type string (not member) signals schema-error"
    (ok (handler-case
            (progn
              (compile-union-extractor '(query-result bad-discriminator)
                                       :discriminator 'kind)
              nil)
          (schema-error () t)))))

(deftest compile-union/errors-on-multi-value-discriminator
  (testing "discriminator slot with multiple enum values signals schema-error"
    (ok (handler-case
            (progn
              (compile-union-extractor '(query-result multi-value-discriminator)
                                       :discriminator 'kind)
              nil)
          (schema-error () t)))))

;;; ══════════════════════════════════════════════════════════════
;;; Composition: union as nested field, in lists, nullable
;;; ══════════════════════════════════════════════════════════════

(deftest union-as-nested-field/validation
  (let* ((u (make-two-branch-union))
         (schema (make-ir-schema
                  :name "wrapper"
                  :class-name 'wrapper
                  :fields (list
                           (make-ir-field :name "label"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'label
                                          :initarg :label)
                           (make-ir-field :name "result"
                                          :type u
                                          :required-p t
                                          :slot-name 'result
                                          :initarg :result))))
         (data (make-data "label" "test"
                          "result" (make-data "kind" "query" "q" "hello"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest union-as-nested-field/construction
  (let* ((u (make-two-branch-union))
         (schema (make-ir-schema
                  :name "wrapper"
                  :class-name 'wrapper
                  :fields (list
                           (make-ir-field :name "label"
                                          :type (make-ir-type-primitive :kind :string)
                                          :required-p t
                                          :slot-name 'label
                                          :initarg :label)
                           (make-ir-field :name "result"
                                          :type u
                                          :required-p t
                                          :slot-name 'result
                                          :initarg :result))))
         (data (make-data "label" "test"
                          "result" (make-data "kind" "refusal" "reason" "no data")))
         (instance (construct-from-data data schema)))
    (ok (typep instance 'wrapper))
    (ok (string= "test" (slot-value instance 'label)))
    (let ((result (slot-value instance 'result)))
      (ok (typep result 'refusal-result))
      (ok (string= "no data" (slot-value result 'reason))))))

(deftest union-in-list/validates-and-constructs
  (let* ((u (make-two-branch-union))
         (schema (make-ir-schema
                  :name "responses"
                  :fields (list
                           (make-ir-field :name "items"
                                          :type (make-ir-type-list :element-type u)
                                          :required-p t
                                          :slot-name 'items))))
         (data (make-data "items" (list
                                   (make-data "kind" "query" "q" "q1")
                                   (make-data "kind" "refusal" "reason" "r1")))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))
    (let* ((result (construct-from-data data schema))
           (items (gethash "items" result)))
      (ok (= 2 (length items)))
      (ok (typep (first items) 'query-result))
      (ok (typep (second items) 'refusal-result)))))

(deftest union-nullable/accepts-null
  (let* ((u (make-two-branch-union))
         (schema (make-ir-schema
                  :name "maybe_response"
                  :fields (list
                           (make-ir-field :name "result"
                                          :type (make-ir-type-nullable :inner-type u)
                                          :required-p nil
                                          :nullable-p t
                                          :slot-name 'result))))
         (data (make-data "result" :null)))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))

(deftest union-nullable/accepts-valid-branch
  (let* ((u (make-two-branch-union))
         (schema (make-ir-schema
                  :name "maybe_response"
                  :fields (list
                           (make-ir-field :name "result"
                                          :type (make-ir-type-nullable :inner-type u)
                                          :required-p nil
                                          :nullable-p t
                                          :slot-name 'result))))
         (data (make-data "result" (make-data "kind" "query" "q" "hello"))))
    (multiple-value-bind (valid-p errors)
        (validate-data data schema)
      (ok valid-p)
      (ok (null errors)))))
