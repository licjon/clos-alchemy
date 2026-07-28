(in-package #:clos-alchemy)

(defun lisp-name-to-json-name (symbol)
  "Convert a Lisp symbol name to a JSON key name.
MY-SLOT-NAME -> \"my_slot_name\""
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defun class-to-schema (class-designator &key schema-cache
                                              (mode :full)
                                              slot-list
                                              slot-types)
  "Introspect a CLOS class and produce an ir-schema.
CLASS-DESIGNATOR: symbol or class object.
MODE: :full (all slots), :required (only required), :custom (only SLOT-LIST).
SLOT-TYPES: alist of (slot-name . type-spec) overrides for MOP-reported types."
  (let* ((class (etypecase class-designator
                  (symbol (or (find-class class-designator nil)
                              (error 'schema-error
                                     :class-name class-designator
                                     :reason "class not found")))
                  (class class-designator)))
         (class-name (class-name class))
         (cache (or schema-cache (make-hash-table)))
         (existing (gethash class-name cache)))
    (when existing
      (return-from class-to-schema existing))
    ;; Insert placeholder to break cycles
    (let ((schema (make-ir-schema :name (lisp-name-to-json-name class-name)
                                  :class-name class-name)))
      (setf (gethash class-name cache) schema)
      (closer-mop:ensure-finalized class)
      (let ((fields (mapcan (lambda (slot)
                              (let ((field (%slot-to-field slot class-name
                                                          cache slot-types)))
                                (when (%include-slot-p slot field mode slot-list)
                                  (list field))))
                            (closer-mop:class-slots class))))
        (setf (ir-schema-fields schema) fields)
        schema))))

(defun %slot-to-field (slot class-name cache slot-types)
  "Convert a MOP slot definition to an ir-field."
  (let* ((slot-name (closer-mop:slot-definition-name slot))
         (type-spec (%effective-type slot slot-name slot-types))
         (list-of-element (%slot-list-of-element slot slot-name slot-types))
         (map-of-value (%slot-map-of-value slot slot-name slot-types)))
    (unless (or list-of-element map-of-value)
      (when (null type-spec)
        (error 'schema-error
               :class-name class-name
               :reason (format nil "slot ~A has type NIL (the empty type); use an explicit type or :slot-types to override"
                               slot-name)))
      (when (eq type-spec t)
        (warn "~A slot ~A has no explicit type; defaulting to string. ~
               Declare a :type or use :slot-types to silence this warning."
              class-name slot-name)
        (setf type-spec 'string)))
    (let* ((initargs (closer-mop:slot-definition-initargs slot))
           (has-initform (not (null (closer-mop:slot-definition-initfunction slot))))
           (required-p (and (not (null initargs))
                            (not has-initform)))
           (ir-type (cond
                      (map-of-value
                       (make-ir-type-map
                        :value-type (cl-type-to-ir-type map-of-value
                                                        :schema-cache cache)))
                      (list-of-element
                       (make-ir-type-list
                        :element-type (cl-type-to-ir-type list-of-element
                                                          :schema-cache cache)))
                      (t
                       (cl-type-to-ir-type type-spec :schema-cache cache))))
           (nullable-p (ir-type-nullable-p ir-type)))
      (%apply-slot-constraints slot ir-type)
      (make-ir-field :name (lisp-name-to-json-name slot-name)
                     :type ir-type
                     :required-p required-p
                     :nullable-p nullable-p
                     :slot-name slot-name
                     :initarg (first initargs)
                     :description (documentation slot t)
                     :validate (%slot-validate slot)))))

(defun %effective-type (slot slot-name slot-types)
  "Get the effective type for a slot, checking overrides first."
  (let ((override (assoc slot-name slot-types)))
    (if override
        (cdr override)
        (closer-mop:slot-definition-type slot))))

(defun %slot-list-of-element (slot slot-name slot-types)
  "Return the :list-of element type if present, nil otherwise.
Slot-types overrides take precedence (no :list-of in that path)."
  (when (and (not (assoc slot-name slot-types))
             (typep slot 'constructor-effective-slot-definition))
    (slot-definition-list-of slot)))

(defun %slot-map-of-value (slot slot-name slot-types)
  "Return the :map-of value type if present, nil otherwise.
Slot-types overrides take precedence (no :map-of in that path)."
  (when (and (not (assoc slot-name slot-types))
             (typep slot 'constructor-effective-slot-definition))
    (slot-definition-map-of slot)))

(defun %slot-validate (slot)
  "Return the :validate function from SLOT if it is a constructor slot, nil otherwise."
  (when (typep slot 'constructor-effective-slot-definition)
    (let ((v (slot-definition-validate slot)))
      (when v
        (if (functionp v)
            v
            (coerce v 'function))))))

(defun %apply-slot-constraints (slot ir-type)
  "Apply metaclass constraint slot options onto the IR type node.
Explicit slot options override any constraints inferred from the CL type specifier."
  (when (typep slot 'constructor-effective-slot-definition)
    (macrolet ((override (accessor struct-slot)
                 `(let ((v (,accessor slot)))
                    (when v
                      (setf (,struct-slot ir-type) v)))))
      (when (ir-type-primitive-p ir-type)
        (override slot-definition-minimum ir-type-primitive-minimum)
        (override slot-definition-maximum ir-type-primitive-maximum)
        (override slot-definition-exclusive-minimum ir-type-primitive-exclusive-minimum)
        (override slot-definition-exclusive-maximum ir-type-primitive-exclusive-maximum)
        (override slot-definition-min-length ir-type-primitive-min-length)
        (override slot-definition-max-length ir-type-primitive-max-length)
        (override slot-definition-pattern ir-type-primitive-pattern))
      (when (ir-type-list-p ir-type)
        (override slot-definition-min-items ir-type-list-min-items)
        (override slot-definition-max-items ir-type-list-max-items)))))

(defun %include-slot-p (slot field mode slot-list)
  "Determine whether a slot should be included based on extraction mode."
  (declare (ignore slot))
  (ecase mode
    (:full t)
    (:required (ir-field-required-p field))
    (:custom (member (ir-field-slot-name field) slot-list))))
