(defpackage #:clos-alchemy/telemetry
  (:use #:cl)
  (:import-from #:clos-alchemy
                #:extraction-attempt #:extraction-retry #:extraction-exhausted
                #:extraction-event-attempt-number #:extraction-event-raw-response
                #:extraction-event-raw-data #:extraction-event-parse-error
                #:extraction-event-validation-errors #:extraction-event-usage)
  (:export
   #:with-extraction-logging))
