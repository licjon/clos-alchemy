(in-package #:clos-constructor)

(deftype list-of (element-type)
  (declare (ignore element-type))
  'list)

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
  (let ((head (first spec)))
    (case head
      (member
       (%map-member-type (rest spec)))
      (or
       (%map-or-type (rest spec) cache))
      (list-of
       (%map-list-of-type (second spec) cache))
      (t
       (make-ir-type-primitive :kind :string)))))

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
       (make-ir-type-nullable :inner-type (%map-type (first non-null) cache)))
      ;; (or X Y ...) without null — take first type
      (non-null
       (%map-type (first non-null) cache))
      ;; (or null) — just null, treat as nullable string
      (t
       (make-ir-type-nullable
        :inner-type (make-ir-type-primitive :kind :string))))))

(defun %map-list-of-type (element-spec cache)
  "Map (list-of X) to ir-type-list."
  (make-ir-type-list :element-type (%map-type element-spec cache)))

(defun %map-named-type (spec cache)
  (cond
    ;; String types
    ((member spec '(string base-string simple-string simple-base-string))
     (make-ir-type-primitive :kind :string))

    ;; Integer types
    ((member spec '(integer fixnum bignum bit
                    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)))
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
