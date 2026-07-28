(defpackage #:clos-alchemy
  (:use #:cl)
  (:export
   ;; IR type nodes
   #:ir-type-primitive #:ir-type-primitive-p #:ir-type-primitive-kind
   #:ir-type-primitive-numeric-type
   #:ir-type-primitive-minimum #:ir-type-primitive-maximum
   #:ir-type-primitive-exclusive-minimum #:ir-type-primitive-exclusive-maximum
   #:ir-type-primitive-min-length #:ir-type-primitive-max-length
   #:ir-type-primitive-pattern
   #:make-ir-type-primitive
   #:ir-type-enum #:ir-type-enum-p #:ir-type-enum-values
   #:make-ir-type-enum
   #:ir-type-list #:ir-type-list-p #:ir-type-list-element-type
   #:ir-type-list-container
   #:ir-type-list-min-items #:ir-type-list-max-items
   #:make-ir-type-list
   #:ir-type-object #:ir-type-object-p #:ir-type-object-schema
   #:make-ir-type-object
   #:ir-type-nullable #:ir-type-nullable-p #:ir-type-nullable-inner-type
   #:make-ir-type-nullable
   #:ir-type-date #:ir-type-date-p #:ir-type-date-format
   #:make-ir-type-date
   #:ir-type-map #:ir-type-map-p #:ir-type-map-value-type
   #:make-ir-type-map
   #:ir-type-union #:ir-type-union-p #:ir-type-union-discriminator
   #:ir-type-union-branches
   #:make-ir-type-union

   ;; Date type specifiers
   #:date #:date-time

   ;; IR field and schema
   #:ir-field #:ir-field-p
   #:ir-field-name #:ir-field-type #:ir-field-required-p
   #:ir-field-nullable-p #:ir-field-slot-name #:ir-field-initarg
   #:ir-field-description #:ir-field-validate
   #:make-ir-field
   #:ir-schema #:ir-schema-p
   #:ir-schema-name #:ir-schema-description #:ir-schema-fields
   #:ir-schema-class-name
   #:make-ir-schema

   ;; Compilation artifact
   #:extraction-compilation #:extraction-compilation-p
   #:extraction-compilation-schema
   #:extraction-compilation-prompt
   #:make-extraction-compilation

   ;; Extraction result
   #:extraction-result #:extraction-result-p
   #:extraction-result-instance
   #:extraction-result-raw-data
   #:extraction-result-raw-response
   #:extraction-result-retries
   #:extraction-result-usage
   #:make-extraction-result

   ;; Conditions
   #:extraction-error
   #:schema-error #:schema-error-class-name #:schema-error-reason
   #:validation-error #:validation-error-field-name
   #:validation-error-expected #:validation-error-actual
   #:generation-error #:generation-error-backend #:generation-error-reason
   #:max-retries-error #:max-retries-error-retries #:max-retries-error-errors
   #:max-retries-error-raw-response #:max-retries-error-raw-data
   #:max-retries-error-usage #:max-retries-error-attempts

   ;; Metaclass
   #:constructor-class
   #:slot-definition-list-of
   #:slot-definition-map-of
   #:slot-definition-validate
   #:slot-definition-minimum #:slot-definition-maximum
   #:slot-definition-exclusive-minimum #:slot-definition-exclusive-maximum
   #:slot-definition-min-length #:slot-definition-max-length
   #:slot-definition-pattern
   #:slot-definition-min-items #:slot-definition-max-items

   ;; Type mapping
   #:cl-type-to-ir-type

   ;; Introspection
   #:class-to-schema
   #:lisp-name-to-json-name

   ;; Ordered map
   #:ordered-map #:ordered-map-p #:ordered-map-entries
   #:make-ordered-map #:ordered-map-get #:ordered-map-put

   ;; JSON Schema
   #:schema-to-json-schema

   ;; Validation
   #:validate-data
   #:validate-instance

   ;; Construction
   #:construct-from-data

   ;; Prompt
   #:generate-system-prompt

   ;; JSON parsing
   #:parse-json-response

   ;; Top-level API
   #:compile-extractor
   #:extract
   #:extract-list
   #:extract-value
   #:compile-union-extractor
   #:extract-union))
