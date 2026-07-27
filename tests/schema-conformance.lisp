(defpackage #:clos-alchemy/tests/schema-conformance
  (:use #:cl #:rove #:clos-alchemy)
  (:import-from #:cl-llm-backend
                #:make-mock-backend #:mock-backend-requests))

(in-package #:clos-alchemy/tests/schema-conformance)

;;;; Conformance of emitted JSON Schemas against the constraints of the
;;;; backends that consume them.
;;;;
;;;; Every rule encoded below was observed directly against the live OpenAI
;;;; strict `json_schema` API. The quoted 400s are the actual responses:
;;;;
;;;;   additionalProperties: null       => None is not of type 'object', 'boolean'
;;;;   additionalProperties omitted     => required to be supplied and to be false
;;;;   required missing a key           => must include every key in properties
;;;;   required missing, nested         => In context=('properties','person')
;;;;   oneOf                            => 'oneOf' is not permitted
;;;;   map without properties/required  => object schema missing properties
;;;;
;;;; yason encodes NIL as JSON null and the symbol YASON:FALSE as JSON false,
;;;; so the two are distinguishable here and must be kept distinct.

;;; ── Test utilities ─────────────────────────────────────────────────

(defun prop-get (props key)
  "Look up KEY in a properties container (hash-table or ordered-map)."
  (etypecase props
    (hash-table (gethash key props))
    (ordered-map (ordered-map-get props key))))

(defun ht (&rest key-values)
  "Build an EQUAL hash table from a flat key/value list."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on key-values by #'cddr do (setf (gethash k h) v))
    h))

(defun object-schema-p (schema)
  (and (hash-table-p schema)
       (equal "object" (gethash "type" schema))))

(defun %props-keys (props)
  "Collect property keys from a hash-table or ordered-map."
  (etypecase props
    (hash-table
     (let ((keys '()))
       (maphash (lambda (k v) (declare (ignore v)) (push k keys)) props)
       keys))
    (ordered-map
     (mapcar #'car (ordered-map-entries props)))))

(defun %props-each (props fn)
  "Call FN with (key value) for each entry in a hash-table or ordered-map."
  (etypecase props
    (hash-table (maphash fn props))
    (ordered-map
     (dolist (entry (ordered-map-entries props))
       (funcall fn (car entry) (cdr entry))))))

(defun %props-p (obj)
  "True when OBJ is a valid properties container (hash-table or ordered-map)."
  (or (hash-table-p obj) (ordered-map-p obj)))

(defun openai-strict-violations (schema &optional (path "(root)"))
  "Return a list of human-readable violations of OpenAI strict-mode structural
rules in SCHEMA. An empty list means conformant. Pure — no I/O."
  (let ((found '()))
    (labels ((report (fmt &rest args)
               (push (format nil "~A: ~?" path fmt args) found))
             (recurse (sub sub-path)
               (setf found (nconc (openai-strict-violations sub sub-path) found))))
      (when (hash-table-p schema)

        ;; oneOf is rejected anywhere it appears.
        (when (nth-value 1 (gethash "oneOf" schema))
          (report "'oneOf' is not permitted"))

        (when (object-schema-p schema)
          ;; additionalProperties must be supplied, and must be false or a schema.
          (multiple-value-bind (ap present) (gethash "additionalProperties" schema)
            (cond ((not present)
                   (report "'additionalProperties' must be supplied"))
                  ((eq ap 'yason:false))          ; closed object — ok
                  ((hash-table-p ap))             ; map — ok
                  (t (report "'additionalProperties' must be false or a schema, got ~S" ap))))

          ;; properties must be supplied, even when empty.
          (multiple-value-bind (props props-present) (gethash "properties" schema)
            (if (not props-present)
                (report "object schema missing 'properties'")
                ;; required must be supplied and cover every property key.
                (multiple-value-bind (required req-present) (gethash "required" schema)
                  (if (not req-present)
                      (report "'required' must be supplied")
                      (let ((keys (%props-keys props)))
                        (dolist (k (sort keys #'string<))
                          (unless (member k required :test #'equal)
                            (report "'required' is missing key ~S" k))))))))

          ;; Recurse through the object's children.
          (let ((props (gethash "properties" schema)))
            (when (%props-p props)
              (%props-each props (lambda (k sub) (recurse sub (format nil "~A.~A" path k))))))
          (let ((ap (gethash "additionalProperties" schema)))
            (when (hash-table-p ap)
              (recurse ap (format nil "~A.<additionalProperties>" path)))))

        ;; These appear on non-object schemas too.
        (let ((items (gethash "items" schema)))
          (when (hash-table-p items)
            (recurse items (format nil "~A[]" path))))
        (let ((branches (gethash "anyOf" schema)))
          (when (listp branches)
            (loop for branch in branches
                  for i from 0
                  do (recurse branch (format nil "~A|anyOf[~D]" path i)))))
        (let ((defs (gethash "$defs" schema)))
          (when (hash-table-p defs)
            (maphash (lambda (k sub) (recurse sub (format nil "$defs.~A" k))) defs)))))
    found))

(defun violates-about-p (schema substring)
  "True when SCHEMA has at least one violation mentioning SUBSTRING."
  (some (lambda (v) (search substring v)) (openai-strict-violations schema)))

(defun conformant-p (schema)
  (null (openai-strict-violations schema)))

(defun encoded (schema)
  (with-output-to-string (s) (yason:encode schema s)))

;;; A schema that the live API accepted, used as the positive control.
(defun accepted-schema ()
  (ht "type" "object"
      "properties" (ht "name" (ht "type" "string")
                       "nick" (ht "anyOf" (list (ht "type" "string")
                                                (ht "type" "null"))))
      "required" (list "name" "nick")
      "additionalProperties" 'yason:false))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART A — the checker itself
;;;
;;;  Without these, every Part B test could pass vacuously against a
;;;  checker that reports nothing.
;;; ══════════════════════════════════════════════════════════════════

(deftest checker/accepts-a-schema-the-api-accepted
  (testing "the positive control reports no violations"
    (ok (conformant-p (accepted-schema)))))

(deftest checker/rejects-additional-properties-null
  (testing "NIL encodes to JSON null, which the API rejects"
    (let ((s (accepted-schema)))
      (setf (gethash "additionalProperties" s) nil)
      (ok (violates-about-p s "additionalProperties")))))

(deftest checker/rejects-additional-properties-absent
  (let ((s (accepted-schema)))
    (remhash "additionalProperties" s)
    (ok (violates-about-p s "additionalProperties"))))

(deftest checker/rejects-additional-properties-true
  (testing "true is not false — strict mode requires a closed object"
    (let ((s (accepted-schema)))
      (setf (gethash "additionalProperties" s) t)
      (ok (violates-about-p s "additionalProperties")))))

(deftest checker/accepts-additional-properties-as-a-schema
  (testing "a map object is legal when properties and required accompany it"
    (ok (conformant-p
         (ht "type" "object"
             "properties" (ht)
             "required" '()
             "additionalProperties" (ht "type" "string"))))))

(deftest checker/rejects-required-missing-a-key
  (let ((s (accepted-schema)))
    (setf (gethash "required" s) (list "name"))
    (ok (violates-about-p s "nick"))))

(deftest checker/rejects-required-absent
  (let ((s (accepted-schema)))
    (remhash "required" s)
    (ok (violates-about-p s "required"))))

(deftest checker/rejects-properties-absent
  (let ((s (accepted-schema)))
    (remhash "properties" s)
    (ok (violates-about-p s "properties"))))

(deftest checker/rejects-one-of
  (ok (violates-about-p
       (ht "type" "object"
           "properties" (ht "r" (ht "oneOf" (list (ht "type" "string"))))
           "required" (list "r")
           "additionalProperties" 'yason:false)
       "oneOf")))

(deftest checker/detects-violation-in-a-nested-object
  (testing "the API enforces the rules at every level, so the checker must recurse"
    (ok (violates-about-p
         (ht "type" "object"
             "properties"
             (ht "person" (ht "type" "object"
                              "properties" (ht "name" (ht "type" "string")
                                               "nick" (ht "type" "string"))
                              "required" (list "name")     ; missing "nick"
                              "additionalProperties" 'yason:false))
             "required" (list "person")
             "additionalProperties" 'yason:false)
         "nick"))))

(deftest checker/detects-violation-inside-an-any-of-branch
  (ok (violates-about-p
       (ht "type" "object"
           "properties"
           (ht "r" (ht "anyOf" (list (ht "type" "object"
                                         "properties" (ht "a" (ht "type" "string"))
                                         "required" '())   ; missing additionalProperties + key
                                     (ht "type" "null"))))
           "required" (list "r")
           "additionalProperties" 'yason:false)
       "additionalProperties")))

(deftest checker/detects-violation-inside-array-items
  (ok (violates-about-p
       (ht "type" "object"
           "properties"
           (ht "rows" (ht "type" "array"
                          "items" (ht "type" "object"
                                      "properties" (ht "a" (ht "type" "string"))
                                      "required" (list "a"))))  ; missing additionalProperties
           "required" (list "rows")
           "additionalProperties" 'yason:false)
       "additionalProperties")))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART B — real emitted schemas must conform
;;;
;;;  Class matrix from the issue. These are the assertions that fail
;;;  against the current emitter.
;;; ══════════════════════════════════════════════════════════════════

(defclass optional-slot ()
  ((reason :type string :initarg :reason)
   (nick   :type string :initarg :nick :initform "none")))

(defclass address ()
  ((street :type string :initarg :street)
   (city   :type string :initarg :city)))

(defclass employee ()
  ((name    :type string :initarg :name)
   (address :type address :initarg :address)))

(defclass enum-holder ()
  ((urgency :type (member :low :high) :initarg :urgency)))

(defclass nullable-holder ()
  ((email :type (or null string) :initarg :email)))

(defclass listy ()
  ((tags :initarg :tags :list-of string))
  (:metaclass constructor-class))

(defclass dated-event ()
  ((title :type string :initarg :title)
   (event-date :type date :initarg :event-date)))

(defclass timestamped-event ()
  ((title :type string :initarg :title)
   (ts :type date-time :initarg :ts)))

(defclass no-slots () ())

(deftest emitted/optional-slot-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'optional-slot)))))

(deftest emitted/nested-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'employee)))))

(deftest emitted/enum-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'enum-holder)))))

(deftest emitted/nullable-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'nullable-holder)))))

(deftest emitted/list-of-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'listy)))))

(deftest emitted/date-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'dated-event)))))

(deftest emitted/date-time-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'timestamped-event)))))

(deftest emitted/slotless-class-conforms
  (testing "required must still be supplied, and must encode as [] not null"
    (let ((js (schema-to-json-schema (class-to-schema 'no-slots))))
      (ok (conformant-p js))
      (ok (search "\"required\":[]" (encoded js))))))

(deftest emitted/extract-list-synthetic-schema-conforms
  (testing "the hand-built wrapper schema in extract-list reaches the backend conformant"
    (let* ((backend (make-mock-backend
                     :responses (lambda (m) (declare (ignore m))
                                  "{\"items\":[{\"reason\":\"r\",\"nick\":\"n\"}]}")))
           (ignored (extract-list backend 'optional-slot "text"))
           (request (first (mock-backend-requests backend)))
           (schema (getf (getf request :keys) :output-schema)))
      (declare (ignore ignored))
      (ok (hash-table-p schema))
      (ok (conformant-p schema)))))

(deftest emitted/additional-properties-encodes-as-false
  (testing "the value must serialise to false, not null"
    (ok (search "\"additionalProperties\":false"
                (encoded (schema-to-json-schema (class-to-schema 'optional-slot)))))))

(deftest emitted/optional-field-permits-null
  (testing "an optional slot must be expressible as null, or the model cannot
say \"not determinable\" once every key is required"
    (let* ((js (schema-to-json-schema (class-to-schema 'optional-slot)))
           (nick (prop-get (gethash "properties" js) "nick"))
           (branches (gethash "anyOf" nick)))
      (ok (listp branches))
      (ok (member "null" branches
                  :test #'equal
                  :key (lambda (b) (and (hash-table-p b) (gethash "type" b))))))))

(deftest emitted/required-slot-is-not-made-nullable
  (testing "only optional slots gain the null branch"
    (let* ((js (schema-to-json-schema (class-to-schema 'optional-slot)))
           (reason (prop-get (gethash "properties" js) "reason")))
      (ok (equal "string" (gethash "type" reason)))
      (ok (null (gethash "anyOf" reason))))))

(deftest emitted/properties-order-follows-slot-order
  (testing "regression guard for the chain-of-thought ordering in README:300;
issue #8 owns the portability question"
    (let ((json (encoded (schema-to-json-schema (class-to-schema 'optional-slot)))))
      (ok (< (search "\"reason\"" json) (search "\"nick\"" json))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART C — validation must accept a null for an optional field
;;;
;;;  Proven necessary by live llama.cpp generation: under the new schema
;;;  the model emits {"nick": null} and the old validator rejects it,
;;;  exhausting retries. See issue #15.
;;; ══════════════════════════════════════════════════════════════════

(deftest validation/null-for-optional-non-nullable-field-is-accepted
  (let ((schema (class-to-schema 'optional-slot)))
    (ok (validate-data (parse-json-response "{\"reason\":\"r\",\"nick\":null}")
                       schema))))

(deftest validation/null-for-required-field-is-still-an-error
  (testing "the fix must not become blanket permissiveness"
    (let ((schema (class-to-schema 'optional-slot)))
      (ok (not (validate-data (parse-json-response "{\"reason\":null,\"nick\":null}")
                              schema))))))

(deftest validation/null-for-declared-nullable-field-is-still-accepted
  (let ((schema (class-to-schema 'nullable-holder)))
    (ok (validate-data (parse-json-response "{\"email\":null}") schema))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART D — construction must not let a wire null override an initform
;;; ══════════════════════════════════════════════════════════════════

(deftest construction/null-for-optional-field-leaves-the-initform
  (testing "the null means \"absent\", so the slot keeps its default"
    (let* ((schema (class-to-schema 'optional-slot))
           (data (parse-json-response "{\"reason\":\"r\",\"nick\":null}"))
           (instance (construct-from-data data schema)))
      (ok (equal "none" (slot-value instance 'nick))))))

(deftest construction/null-for-declared-nullable-field-stores-nil
  (testing "the developer declared null legal, so it must not be skipped"
    (let* ((schema (class-to-schema 'nullable-holder))
           (data (parse-json-response "{\"email\":null}"))
           (instance (construct-from-data data schema)))
      (ok (null (slot-value instance 'email))))))

(defclass optional-list ()
  ((tags :initarg :tags :initform (list "default") :list-of string))
  (:metaclass constructor-class))

(deftest construction/empty-array-for-optional-list-is-not-treated-as-absent
  (testing "JSON [] and JSON null both parse to NIL; an explicitly empty array
must not be replaced by the initform"
    (let* ((schema (class-to-schema 'optional-list))
           (data (parse-json-response "{\"tags\":[]}"))
           (instance (construct-from-data data schema)))
      (ok (null (slot-value instance 'tags))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART E — cyclic schemas must emit valid $defs/$ref (#1)
;;; ══════════════════════════════════════════════════════════════════

(defclass tree-node ()
  ((label :type string :initarg :label)
   (child :type tree-node :initarg :child :initform nil)))

(defclass node-a ()
  ((child :type node-b :initarg :child)))

(defclass node-b ()
  ((parent :type node-a :initarg :parent)))

(deftest emitted/self-referencing-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'tree-node)))))

(deftest emitted/mutual-recursion-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'node-a)))))

(deftest emitted/cyclic-schema-serializes-without-error
  (testing "yason must not infinite-loop on the emitted structure"
    (let* ((js (schema-to-json-schema (class-to-schema 'tree-node)))
           (json (encoded js))
           (parsed (yason:parse json)))
      (ok (hash-table-p (gethash "$defs" parsed)))
      (ok (stringp (gethash "$ref" parsed))))))

(deftest emitted/cyclic-optional-field-permits-null
  (testing "the optional recursive slot must be anyOf [$ref, null]"
    (let* ((js (schema-to-json-schema (class-to-schema 'tree-node)))
           (def (gethash "tree_node" (gethash "$defs" js)))
           (child (prop-get (gethash "properties" def) "child"))
           (branches (gethash "anyOf" child)))
      (ok (= 2 (length branches)))
      (ok (stringp (gethash "$ref" (first branches))))
      (ok (string= "null" (gethash "type" (second branches)))))))

;;; ══════════════════════════════════════════════════════════════════
;;;  PART F — map type conformance (#13)
;;; ══════════════════════════════════════════════════════════════════

(defclass map-holder ()
  ((meta :type hash-table :initarg :meta :map-of string))
  (:metaclass constructor-class))

(defclass map-of-int ()
  ((scores :type hash-table :initarg :scores :map-of integer))
  (:metaclass constructor-class))

(deftest emitted/map-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'map-holder)))))

(deftest emitted/map-of-integer-class-conforms
  (ok (conformant-p (schema-to-json-schema (class-to-schema 'map-of-int)))))

(deftest emitted/map-additional-properties-is-schema
  (testing "map slots emit additionalProperties as a type schema, not false"
    (let* ((js (schema-to-json-schema (class-to-schema 'map-holder)))
           (props (gethash "properties" js))
           (meta-js (prop-get props "meta"))
           (ap (gethash "additionalProperties" meta-js)))
      (ok (hash-table-p ap))
      (ok (string= "string" (gethash "type" ap))))))

(deftest emitted/closed-object-still-has-false-additional-properties
  (testing "non-map objects must still have additionalProperties: false"
    (let ((js (schema-to-json-schema (class-to-schema 'optional-slot))))
      (ok (eq 'yason:false (gethash "additionalProperties" js))))))

(deftest emitted/map-has-empty-properties-and-required
  (testing "map schemas must include empty properties and required for strict mode"
    (let* ((js (schema-to-json-schema (class-to-schema 'map-holder)))
           (meta-js (prop-get (gethash "properties" js) "meta"))
           (json (with-output-to-string (s) (yason:encode meta-js s))))
      (ok (search "\"properties\":{}" json))
      (ok (search "\"required\":[]" json)))))
