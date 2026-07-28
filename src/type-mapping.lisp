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
    ;; NIL — the empty type; no value can inhabit it
    ((null spec)
     (error 'schema-error
            :class-name spec
            :reason "type NIL (the empty type) cannot be mapped to JSON — use an explicit type or :slot-types"))

    ;; T — "any object"; not representable in JSON Schema
    ((eq spec t)
     (error 'schema-error
            :class-name spec
            :reason "type T cannot be mapped to JSON — use an explicit type or :slot-types"))

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
       (%map-integer-compound-type head args))
      ((float single-float double-float short-float long-float)
       (%map-float-compound-type head args))
      ((real rational)
       (make-ir-type-primitive :kind :number))
      ((string base-string simple-string simple-base-string)
       (%map-string-compound-type args))
      (t
       (error 'schema-error
              :class-name spec
              :reason (format nil "unsupported compound type specifier: ~S" spec))))))

(defun %bound-or-nil (value)
  "Return VALUE as a numeric bound, or nil if it is * or absent."
  (if (or (null value) (eq value '*))
      nil
      value))

(defun %map-integer-compound-type (head args)
  "Map (integer low high), (unsigned-byte n), (signed-byte n), (mod n)."
  (case head
    (integer
     (let ((lo (%bound-or-nil (first args)))
           (hi (%bound-or-nil (second args))))
       (make-ir-type-primitive :kind :integer :minimum lo :maximum hi)))
    (unsigned-byte
     (let ((bits (first args)))
       (if (and bits (not (eq bits '*)))
           (make-ir-type-primitive :kind :integer
                                   :minimum 0
                                   :maximum (1- (expt 2 bits)))
           (make-ir-type-primitive :kind :integer))))
    (signed-byte
     (let ((bits (first args)))
       (if (and bits (not (eq bits '*)))
           (make-ir-type-primitive :kind :integer
                                   :minimum (- (expt 2 (1- bits)))
                                   :maximum (1- (expt 2 (1- bits))))
           (make-ir-type-primitive :kind :integer))))
    (mod
     (let ((n (first args)))
       (make-ir-type-primitive :kind :integer
                               :minimum 0
                               :maximum (1- n))))))

(defun %map-float-compound-type (head args)
  "Map (float low high) and friends, extracting numeric bounds."
  (let ((lo (%bound-or-nil (first args)))
        (hi (%bound-or-nil (second args))))
    (make-ir-type-primitive :kind :number
                            :numeric-type (%float-numeric-type head)
                            :minimum lo
                            :maximum hi)))

(defun %map-string-compound-type (args)
  "Map (string size), extracting max-length from the size argument."
  (let ((size (%bound-or-nil (first args))))
    (make-ir-type-primitive :kind :string
                            :max-length size)))

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
      ;; (or X Y ...) without null — cannot faithfully represent a general union
      (non-null
       (error 'schema-error
              :class-name (cons 'or members)
              :reason (format nil "general union types are not supported — members ~{~S~^, ~} would be silently narrowed; use :slot-types to override"
                              non-null)))
      ;; (or null) — just null, treat as nullable string
      (t
       (make-ir-type-nullable
        :inner-type (make-ir-type-primitive :kind :string))))))

(defun %float-numeric-type (spec)
  (case spec
    ((single-float short-float) 'single-float)
    ((double-float long-float) 'double-float)
    (t 'single-float)))

(defun %map-named-type (spec cache)
  (cond
    ;; String types
    ((member spec '(string base-string simple-string simple-base-string))
     (make-ir-type-primitive :kind :string))

    ;; Integer types
    ((member spec '(integer fixnum bignum bit unsigned-byte signed-byte))
     (make-ir-type-primitive :kind :integer))

    ;; Float types — carry numeric-type for coercion
    ((member spec '(float single-float double-float short-float long-float))
     (make-ir-type-primitive :kind :number :numeric-type (%float-numeric-type spec)))

    ;; Generic number types — no coercion target
    ((member spec '(number real rational))
     (make-ir-type-primitive :kind :number))

    ;; Boolean
    ((eq spec 'boolean)
     (make-ir-type-primitive :kind :boolean))

    ;; Date types
    ((eq spec 'date)
     (make-ir-type-date :format :date))
    ((eq spec 'date-time)
     (make-ir-type-date :format :date-time))

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
