(defpackage #:clos-constructor/llama/tests
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/llama/tests)

;;; ── Struct ──────────────────────────────────────────────────────────

(deftest make-llama-backend-struct
  (let ((b (clos-constructor/llama:make-llama-backend)))
    (ok (clos-constructor/llama:llama-backend-p b))
    (ok (null (clos-constructor/llama:llama-backend-model b)))
    (ok (null (clos-constructor/llama:llama-backend-context b)))))

;;; ── backend-output-schema produces valid GBNF ──────────────────────

(defun make-person-schema ()
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
                           :slot-name 'age))))

(deftest output-schema-returns-gbnf-string
  (testing "backend-output-schema returns a non-empty GBNF string with a root rule"
    (let* ((backend (clos-constructor/llama:make-llama-backend))
           (gbnf (backend-output-schema backend (make-person-schema))))
      (ok (stringp gbnf))
      (ok (plusp (length gbnf)))
      (ok (search "root" gbnf)))))

;;; ── Schema → GBNF round-trip for each IR type ─────────────────────

(defun gbnf-for-fields (&rest fields)
  (let ((schema (make-ir-schema :name "test" :fields fields)))
    (backend-output-schema (clos-constructor/llama:make-llama-backend) schema)))

(deftest gbnf-primitive-types
  (testing "string field"
    (ok (stringp (gbnf-for-fields
                  (make-ir-field :name "s" :type (make-ir-type-primitive :kind :string)
                                 :required-p t :slot-name 's)))))
  (testing "integer field"
    (ok (stringp (gbnf-for-fields
                  (make-ir-field :name "i" :type (make-ir-type-primitive :kind :integer)
                                 :required-p t :slot-name 'i)))))
  (testing "number field"
    (ok (stringp (gbnf-for-fields
                  (make-ir-field :name "n" :type (make-ir-type-primitive :kind :number)
                                 :required-p t :slot-name 'n)))))
  (testing "boolean field"
    (ok (stringp (gbnf-for-fields
                  (make-ir-field :name "b" :type (make-ir-type-primitive :kind :boolean)
                                 :required-p t :slot-name 'b))))))

(deftest gbnf-enum-type
  (let ((gbnf (gbnf-for-fields
               (make-ir-field :name "color"
                              :type (make-ir-type-enum :values '("red" "green" "blue"))
                              :required-p t :slot-name 'color))))
    (ok (stringp gbnf))
    (ok (plusp (length gbnf)))))

(deftest gbnf-array-type
  (let ((gbnf (gbnf-for-fields
               (make-ir-field :name "tags"
                              :type (make-ir-type-list
                                     :element-type (make-ir-type-primitive :kind :string))
                              :required-p t :slot-name 'tags))))
    (ok (stringp gbnf))
    (ok (plusp (length gbnf)))))

(deftest gbnf-nullable-type
  (let ((gbnf (gbnf-for-fields
               (make-ir-field :name "bio"
                              :type (make-ir-type-nullable
                                     :inner-type (make-ir-type-primitive :kind :string))
                              :required-p nil :nullable-p t :slot-name 'bio))))
    (ok (stringp gbnf))
    (ok (plusp (length gbnf)))))

(deftest gbnf-nested-object
  (let* ((inner (make-ir-schema
                 :name "address"
                 :class-name 'address
                 :fields (list
                          (make-ir-field :name "city"
                                         :type (make-ir-type-primitive :kind :string)
                                         :required-p t :slot-name 'city))))
         (gbnf (gbnf-for-fields
                (make-ir-field :name "addr"
                               :type (make-ir-type-object :schema inner)
                               :required-p t :slot-name 'addr))))
    (ok (stringp gbnf))
    (ok (plusp (length gbnf)))))

;;; ── Complex schema round-trip ──────────────────────────────────────

(deftest gbnf-complex-schema
  (testing "schema with all type variants converts successfully"
    (let* ((inner (make-ir-schema
                   :name "tag"
                   :class-name 'tag
                   :fields (list
                            (make-ir-field :name "label"
                                           :type (make-ir-type-primitive :kind :string)
                                           :required-p t :slot-name 'label))))
           (schema (make-ir-schema
                    :name "document"
                    :class-name 'document
                    :fields (list
                             (make-ir-field :name "title"
                                            :type (make-ir-type-primitive :kind :string)
                                            :required-p t :slot-name 'title)
                             (make-ir-field :name "page_count"
                                            :type (make-ir-type-primitive :kind :integer)
                                            :required-p t :slot-name 'page-count)
                             (make-ir-field :name "rating"
                                            :type (make-ir-type-primitive :kind :number)
                                            :required-p nil :slot-name 'rating)
                             (make-ir-field :name "published"
                                            :type (make-ir-type-primitive :kind :boolean)
                                            :required-p nil :slot-name 'published)
                             (make-ir-field :name "category"
                                            :type (make-ir-type-enum
                                                   :values '("fiction" "non-fiction" "reference"))
                                            :required-p t :slot-name 'category)
                             (make-ir-field :name "authors"
                                            :type (make-ir-type-list
                                                   :element-type (make-ir-type-primitive :kind :string))
                                            :required-p t :slot-name 'authors)
                             (make-ir-field :name "primary_tag"
                                            :type (make-ir-type-object :schema inner)
                                            :required-p nil :slot-name 'primary-tag)
                             (make-ir-field :name "subtitle"
                                            :type (make-ir-type-nullable
                                                   :inner-type (make-ir-type-primitive :kind :string))
                                            :required-p nil :nullable-p t :slot-name 'subtitle))))
           (gbnf (backend-output-schema
                  (clos-constructor/llama:make-llama-backend) schema)))
      (ok (stringp gbnf))
      (ok (plusp (length gbnf)))
      (ok (search "root" gbnf)))))
