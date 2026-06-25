(in-package #:clos-constructor)

(defgeneric backend-output-schema (backend schema)
  (:documentation
   "Generate the structured output specification from an IR schema.
Returns a JSON Schema representation (hash table)."))

(defgeneric backend-generate (backend schema document &key model temperature
                                                          max-tokens
                                                          system-prompt
                                                          user-prompt)
  (:documentation
   "Invoke an LLM and return an extraction-result.
SCHEMA is an ir-schema. DOCUMENT is the text to extract from."))
