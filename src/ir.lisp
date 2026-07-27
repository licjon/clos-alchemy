(in-package #:clos-alchemy)

;;; Type nodes — tagged union pattern inspired by C2FFI's AST

(defstruct (ir-type-primitive (:copier nil))
  (kind :string :type (member :string :integer :number :boolean))
  (numeric-type nil :type (or symbol null)))

(defstruct (ir-type-enum (:copier nil))
  (values nil :type list))

(defstruct (ir-type-list (:copier nil))
  (element-type nil)
  ;; :list or :vector — which CL sequence construction should produce
  (container :list :type (member :list :vector)))

(defstruct (ir-type-object (:copier nil))
  (schema nil))

(defstruct (ir-type-nullable (:copier nil))
  (inner-type nil))

(defstruct (ir-type-date (:copier nil))
  (format :date :type (member :date :date-time)))

;;; Field — one slot in the extraction target

(defstruct (ir-field (:copier nil))
  (name "" :type string)
  (type nil)
  (required-p t :type boolean)
  (nullable-p nil :type boolean)
  (slot-name nil :type (or symbol null))
  (initarg nil :type (or symbol null))
  (description nil :type (or string null))
  (validate nil :type (or function null)))

(defun wire-nullable-p (field)
  "True when FIELD is optional but not declared nullable — the wire format
must permit null so the model can express absence."
  (and (not (ir-field-required-p field))
       (not (ir-field-nullable-p field))))

;;; Schema — top-level extraction target

(defstruct (ir-schema (:copier nil))
  (name "" :type string)
  (description nil :type (or string null))
  (fields nil :type list)
  (class-name nil :type (or symbol null)))

;;; Compilation artifact — caches IR + prompt, not the output schema

(defstruct (extraction-compilation (:copier nil))
  (schema nil :type (or ir-schema null))
  (prompt nil :type (or string null)))

;;; Extraction result

(defstruct (extraction-result (:copier nil))
  (instance nil)
  (raw-data nil)
  (raw-response nil :type (or string null))
  (retries 0 :type (integer 0))
  (usage nil))
