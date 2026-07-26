(in-package #:clos-alchemy)

(defclass constructor-class (standard-class)
  ()
  (:documentation "Metaclass supporting :list-of as a custom slot option."))

(defmethod closer-mop:validate-superclass ((class constructor-class)
                                           (superclass standard-class))
  t)

(defclass constructor-direct-slot-definition
    (closer-mop:standard-direct-slot-definition)
  ((list-of  :initarg :list-of  :initform nil :reader slot-definition-list-of)
   (validate :initarg :validate :initform nil :reader slot-definition-validate)))

(defclass constructor-effective-slot-definition
    (closer-mop:standard-effective-slot-definition)
  ((list-of  :initarg :list-of  :initform nil :reader slot-definition-list-of)
   (validate :initarg :validate :initform nil :reader slot-definition-validate)))

(defmethod closer-mop:direct-slot-definition-class ((class constructor-class)
                                                     &rest initargs)
  (declare (ignore initargs))
  (find-class 'constructor-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class constructor-class)
                                                       &rest initargs)
  (declare (ignore initargs))
  (find-class 'constructor-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition
    ((class constructor-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((effective-slot (call-next-method))
        (list-of-value (loop for dsd in direct-slot-definitions
                             when (and (typep dsd 'constructor-direct-slot-definition)
                                       (slot-definition-list-of dsd))
                               return (slot-definition-list-of dsd)))
        (validate-value (loop for dsd in direct-slot-definitions
                              when (and (typep dsd 'constructor-direct-slot-definition)
                                        (slot-definition-validate dsd))
                                return (slot-definition-validate dsd))))
    (when list-of-value
      (setf (slot-value effective-slot 'list-of) list-of-value))
    (when validate-value
      (setf (slot-value effective-slot 'validate) validate-value))
    effective-slot))
