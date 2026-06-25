(defpackage #:clos-constructor
  (:use #:cl)
  (:export
   ;; IR type nodes
   #:ir-type-primitive #:ir-type-primitive-p #:ir-type-primitive-kind
   #:make-ir-type-primitive
   #:ir-type-enum #:ir-type-enum-p #:ir-type-enum-values
   #:make-ir-type-enum
   #:ir-type-list #:ir-type-list-p #:ir-type-list-element-type
   #:make-ir-type-list
   #:ir-type-object #:ir-type-object-p #:ir-type-object-schema
   #:make-ir-type-object
   #:ir-type-nullable #:ir-type-nullable-p #:ir-type-nullable-inner-type
   #:make-ir-type-nullable

   ;; IR field and schema
   #:ir-field #:ir-field-p
   #:ir-field-name #:ir-field-type #:ir-field-required-p
   #:ir-field-nullable-p #:ir-field-slot-name
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

   ;; Type mapping
   #:cl-type-to-ir-type
   #:list-of

   ;; Introspection
   #:class-to-schema
   #:lisp-name-to-json-name

   ;; JSON Schema
   #:schema-to-json-schema

   ;; Validation
   #:validate-data

   ;; Construction
   #:construct-from-data

   ;; Prompt
   #:generate-system-prompt

   ;; JSON parsing
   #:parse-json-response

   ;; Backend protocol
   #:backend-output-schema
   #:backend-generate

   ;; Top-level API
   #:compile-extractor
   #:extract
   #:extract-list))
