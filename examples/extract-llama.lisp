;;;; extract-llama.lisp
;;;;
;;;; Demonstrates clos-constructor with a local llama.cpp model. Define
;;;; ordinary CLOS classes, then extract typed instances from unstructured
;;;; text — the JSON Schema is derived from the class, converted to a GBNF
;;;; grammar, and the model is constrained to produce valid output.
;;;;
;;;; Three demos:
;;;;   1. Single extraction — extract a typed CLOS instance from a paragraph
;;;;   2. List extraction — extract multiple instances from a document
;;;;   3. Nested classes — extract objects that reference other CLOS objects
;;;;
;;;; Setup:
;;;;   (ql:quickload :clos-constructor/examples)
;;;;   (setf clos-constructor/examples/extract-llama::*model-path*
;;;;         "/path/to/model.gguf")
;;;;   (clos-constructor/examples/extract-llama:run)
;;;;
;;;; Or via environment variable:
;;;;   export LLAMA_MODEL=/path/to/model.gguf

(defpackage #:clos-constructor/examples/extract-llama
  (:use #:cl #:clos-constructor)
  (:export #:run))

(in-package #:clos-constructor/examples/extract-llama)

(defvar *model-path* (uiop:getenv "LLAMA_MODEL"))

;;; ── Domain classes ─────────────────────────────────────────────────

(defclass person ()
  ((name    :initarg :name    :accessor person-name    :type string
            :documentation "The person's full legal name")
   (age     :initarg :age     :accessor person-age     :type integer)
   (email   :initarg :email   :accessor person-email   :type (or null string))
   (hobbies :initarg :hobbies :accessor person-hobbies :type list))
  (:documentation "A person with contact info and hobbies."))

(defclass address ()
  ((street :initarg :street :accessor address-street :type string)
   (city   :initarg :city   :accessor address-city   :type string)
   (state  :initarg :state  :accessor address-state  :type string))
  (:documentation "A street address."))

(defclass employee ()
  ((name       :initarg :name       :accessor employee-name       :type string)
   (title      :initarg :title      :accessor employee-title      :type string)
   (department :initarg :department :accessor employee-department
               :type (member :engineering :marketing :sales :hr))
   (address    :initarg :address    :accessor employee-address    :type address))
  (:documentation "An employee with a nested address."))

;;; ── Helpers ────────────────────────────────────────────────────────

(defun banner (title)
  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  ~A~%" title)
  (format t "~A~2%" (make-string 64 :initial-element #\═)))

;;; ── Demo 1: Single extraction ──────────────────────────────────────

(defun demo-single-extraction (backend)
  (banner "DEMO 1: Single Extraction")

  (let ((text "My name is Alice Chen and I'm 32 years old. You can reach me at
alice@example.com. In my free time I enjoy rock climbing, painting, and
playing the cello."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting a PERSON instance...~2%")
    (let* ((result (extract backend 'person text))
           (p (extraction-result-instance result)))

      (format t "Got a ~A:~%" (type-of p))
      (format t "  (person-name p)    => ~S~%" (person-name p))
      (format t "  (person-age p)     => ~D  (typep integer: ~A)~%"
              (person-age p) (typep (person-age p) 'integer))
      (format t "  (person-email p)   => ~S~%" (person-email p))
      (format t "  (person-hobbies p) => ~S~%" (person-hobbies p))
      (format t "~%  (typep p 'person)  => ~A~%" (typep p 'person))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Demo 2: List extraction ────────────────────────────────────────

(defun demo-list-extraction (backend)
  (banner "DEMO 2: List Extraction")

  (let ((text "The hiking club has three members. Bob Martinez, age 45, likes
trail running and photography — his email is bob@hiking.org. Carol White
is 28 and enjoys birdwatching and sketching. Finally there's Dave Park,
age 51, who is into geocaching and woodworking; reach him at
dave.park@email.com."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting a list of PERSON instances...~2%")
    (let* ((result (extract-list backend 'person text :max-tokens 2048))
           (people (extraction-result-instance result)))
      (format t "Got ~D PERSON objects:~2%" (length people))
      (dolist (p people)
        (format t "  #<~A ~S, age ~D, email ~S>~%"
                (type-of p)
                (person-name p)
                (person-age p)
                (or (person-email p) :nil))
        (format t "    hobbies: ~{~S~^, ~}~%" (person-hobbies p)))
      (format t "~%  (every #'(lambda (p) (typep p 'person)) people) => ~A~%"
              (every (lambda (p) (typep p 'person)) people)))))

;;; ── Demo 3: Nested classes ────────────────────────────────────────

(defun demo-nested-extraction (backend)
  (banner "DEMO 3: Nested Class Extraction")

  (let ((text "Jane Rodriguez is a Senior Software Engineer in the engineering
department. She works out of the Portland office at 742 Evergreen Terrace,
Portland, Oregon."))
    (format t "Input text:~%  ~A~2%" text)
    (format t "Extracting an EMPLOYEE with nested ADDRESS...~2%")
    (let* ((result (extract backend 'employee text))
           (e (extraction-result-instance result))
           (a (employee-address e)))
      (format t "Got a ~A:~%" (type-of e))
      (format t "  (employee-name e)       => ~S~%" (employee-name e))
      (format t "  (employee-title e)      => ~S~%" (employee-title e))
      (format t "  (employee-department e) => ~S  (keywordp: ~A)~%"
              (employee-department e) (keywordp (employee-department e)))
      (format t "~%  (employee-address e) is a ~A:~%" (type-of a))
      (format t "    (address-street a) => ~S~%" (address-street a))
      (format t "    (address-city a)   => ~S~%" (address-city a))
      (format t "    (address-state a)  => ~S~%" (address-state a))
      (format t "~%  (typep e 'employee) => ~A~%" (typep e 'employee))
      (format t "  (typep a 'address)  => ~A~%" (typep a 'address))
      (format t "  Retries: ~D~%" (extraction-result-retries result)))))

;;; ── Entry point ────────────────────────────────────────────────────

(defun run (&key (model-path *model-path*)
                 (n-gpu-layers 99)
                 (n-ctx 4096))
  "Run all clos-constructor + llama.cpp demos."
  (unless model-path
    (format t "Set *model-path* or export LLAMA_MODEL before calling run.~%")
    (return-from run (values)))

  (format t "Loading model: ~A~%" model-path)
  (cl-llama-cpp:with-backend ()
    (cl-llama-cpp:set-log-callback
     (lambda (level text)
       (when (>= level 3)
         (format *error-output* "~a" text))))
    (cl-llama-cpp:with-model (model model-path :n-gpu-layers n-gpu-layers)
      (cl-llama-cpp:with-context (ctx model :n-ctx n-ctx)
        (let ((backend (clos-constructor/llama:make-llama-backend
                        :model model :context ctx)))
          (demo-single-extraction backend)
          (demo-list-extraction backend)
          (demo-nested-extraction backend)))))

  (format t "~&~%~A~%" (make-string 64 :initial-element #\═))
  (format t "  All demos complete.~%")
  (format t "~A~%" (make-string 64 :initial-element #\═))
  (values))
