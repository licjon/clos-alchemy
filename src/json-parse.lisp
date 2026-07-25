(in-package #:clos-alchemy)

(defun parse-json-response (text)
  "Parse a JSON string from LLM output into nested hash tables.
Handles markdown code fences and leading/trailing text.
Booleans parse to :true/:false so they stay distinguishable from null (nil)."
  (handler-case
      (let ((cleaned (%strip-llm-wrapper text)))
        (%normalize-parse-tree
         (yason:parse cleaned
                      :json-arrays-as-vectors nil
                      :json-booleans-as-symbols t
                      :json-nulls-as-keyword t)))
    (error (e)
      (error 'generation-error
             :backend :json-parser
             :reason (format nil "Failed to parse JSON: ~A~%Raw: ~A"
                             e (subseq text 0 (min 200 (length text))))))))

(defun %normalize-parse-tree (value)
  "Normalize yason symbols throughout a parse tree:
yason:true/yason:false → :true/:false, yason:null → :null."
  (cond
    ((eq value 'yason:true) :true)
    ((eq value 'yason:false) :false)
    ((eq value 'yason:null) :null)
    ((hash-table-p value)
     (maphash (lambda (k v)
                (setf (gethash k value) (%normalize-parse-tree v)))
              value)
     value)
    ((consp value) (mapcar #'%normalize-parse-tree value))
    (t value)))

(defun %strip-llm-wrapper (text)
  "Strip markdown code fences and find the JSON object in LLM output."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    ;; Strip ```json ... ``` fences
    (when (and (>= (length s) 7)
               (string= "```" s :end2 3))
      (let ((end-fence (search "```" s :start2 3)))
        (when end-fence
          (let ((start (or (position #\Newline s :start 3) 3)))
            (setf s (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (subseq s (1+ start) end-fence)))))))
    ;; If empty after stripping, return as-is and let the parser signal
    (when (zerop (length s))
      (return-from %strip-llm-wrapper s))
    ;; If not starting with { or [, find the first one
    (unless (or (char= (char s 0) #\{)
                (char= (char s 0) #\[))
      (let ((brace (position #\{ s))
            (bracket (position #\[ s)))
        (cond
          ((and brace bracket) (setf s (subseq s (min brace bracket))))
          (brace (setf s (subseq s brace)))
          (bracket (setf s (subseq s bracket))))))
    s))
