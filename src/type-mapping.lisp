(in-package #:clos-alchemy)

(defun cl-type-to-ir-type (type-spec &key schema-cache slot-types)
  "Translate a CL type specifier to an IR type node.
SCHEMA-CACHE prevents infinite recursion for mutually-referencing classes.
SLOT-TYPES is unused here (consumed by class-to-schema before calling this)."
  (declare (ignore slot-types))
  (let ((cache (or schema-cache (make-hash-table))))
    (%map-type type-spec cache)))

(defun %map-type (spec cache)
  (cond
    ;; NIL type (empty type) — treat as string
    ((null spec)
     (make-ir-type-primitive :kind :string))

    ;; T or absent — default to string
    ((eq spec t)
     (make-ir-type-primitive :kind :string))

    ;; Compound type specifiers
    ((consp spec)
     (%map-compound-type spec cache))

    ;; Named types
    ((symbolp spec)
     (%map-named-type spec cache))

    (t
     (make-ir-type-primitive :kind :string))))

(defun %map-compound-type (spec cache)
  (let ((head (first spec))
        (args (rest spec)))
    (case head
      (member
       (%map-member-type args))
      (or
       (%map-or-type args cache))
      ((vector array simple-vector simple-array)
       (make-ir-type-list
        :element-type (if (and (first args) (not (eq (first args) '*)))
                          (%map-type (first args) cache)
                          (make-ir-type-primitive :kind :string))
        :container :vector))
      ((integer unsigned-byte signed-byte mod)
       (make-ir-type-primitive :kind :integer))
      ((float single-float double-float short-float long-float real rational)
       (make-ir-type-primitive :kind :number))
      ((string base-string simple-string simple-base-string)
       (make-ir-type-primitive :kind :string))
      (t
       (error 'schema-error
              :class-name spec
              :reason (format nil "unsupported compound type specifier: ~S" spec))))))

(defun %map-member-type (members)
  "Map (member :a :b :c) or (member \"x\" \"y\") to ir-type-enum."
  (make-ir-type-enum
   :values (mapcar (lambda (m)
                     (etypecase m
                       (keyword (string-downcase (symbol-name m)))
                       (symbol (string-downcase (symbol-name m)))
                       (string m)))
                   members)))

(defun %map-or-type (members cache)
  "Map (or null X) / (or X null) to ir-type-nullable."
  (let ((non-null (remove 'null members)))
    (cond
      ;; (or null X) — nullable
      ((and (= 1 (length non-null))
            (member 'null members))
       (let ((inner (%map-type (first non-null) cache)))
         (when (and (ir-type-primitive-p inner)
                    (eq :boolean (ir-type-primitive-kind inner)))
           (error 'schema-error
                  :class-name (first non-null)
                  :reason "(or null boolean) is unrepresentable: CL's NIL conflates JSON false and JSON null"))
         (make-ir-type-nullable :inner-type inner)))
      ;; (or X Y ...) without null — take first type
      (non-null
       (%map-type (first non-null) cache))
      ;; (or null) — just null, treat as nullable string
      (t
       (make-ir-type-nullable
        :inner-type (make-ir-type-primitive :kind :string))))))

(defun %map-named-type (spec cache)
  (cond
    ;; String types
    ((member spec '(string base-string simple-string simple-base-string))
     (make-ir-type-primitive :kind :string))

    ;; Integer types
    ((member spec '(integer fixnum bignum bit unsigned-byte signed-byte))
     (make-ir-type-primitive :kind :integer))

    ;; Float/number types
    ((member spec '(float single-float double-float short-float long-float
                    number real rational))
     (make-ir-type-primitive :kind :number))

    ;; Boolean
    ((eq spec 'boolean)
     (make-ir-type-primitive :kind :boolean))

    ;; Plain list without element type
    ((member spec '(list cons sequence))
     (make-ir-type-list
      :element-type (make-ir-type-primitive :kind :string)))

    ;; Plain vector without element type
    ((member spec '(vector simple-vector))
     (make-ir-type-list
      :element-type (make-ir-type-primitive :kind :string)
      :container :vector))

    ;; Try as a CLOS class reference
    (t
     (%try-class-type spec cache))))

(defun %try-class-type (spec cache)
  "If SPEC names a CLOS class, produce ir-type-object. Otherwise signal schema-error."
  (let ((class (find-class spec nil)))
    (if (and class (subtypep spec 'standard-object))
        (make-ir-type-object :schema (class-to-schema spec :schema-cache cache))
        (error 'schema-error
               :class-name spec
               :reason (format nil "unsupported type specifier: ~S" spec)))))
