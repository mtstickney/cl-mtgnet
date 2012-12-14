(in-package #:cl-rpc)


(defmacro encode-fields ((obj) &rest fields)
  "JSON-encode the FIELDS slots of OBJ. Expects to be run inside a JSON:WITH-OBJECT block."
  `(progn
     ,@(loop for f in fields collect
            `(json:encode-object-member (quote ,f)
                                        (slot-value ,obj (quote ,f))))))

(defmacro fiend-fields ((alist) &rest fields)
  "Search for all the FIELDS in the JSON-decoded object
ALIST. Expanded code returns T if all fields were found, NIL
otherwise."
  `(and
    ,@(loop for f in fields
         collect `(assoc (intern (json:lisp-to-camel-case (symbol-name (quote ,f)))
                                 json:*json-symbols-package*)
                         ,alist))))

(defmacro set-fields ((obj alist) &rest fields)
  "Set the FIELDS slots of OBJ from the JSON-decoded object ALIST."
  `(progn
     ,@(loop for f in fields
          collect `(setf (slot-value ,obj (quote ,f))
                         (car (assoc (intern (json:lisp-to-camel-case
                                              (symbol-name (quote ,f)))
                                             json:*json-symbols-package*)
                                     ,alist))))))

;;; Call object, one per method call
(defstruct call
  "A single method invocation."
  (service)
  (method)
  (args)
  (id))

(defun valid-call-p (obj)
  "Returns T if the object is a valid CALL object."
  (call-p obj))

(defun marshall-call (obj)
  "Marshall the call object OBJ to JSON."
  (check-type obj call)
  (json:with-object ()
    (encode-fields (obj)
                   service
                   method
                   args
                   id)))

(defun json-is-call (json-obj)
  "Return T if JSON-OBJ could be unmarshalled to a CALL object, NIL otherwise."
  (and (listp json-obj)
       (every #'listp json-obj)
       (find-fields (json-obj)
                    'service
                    'method
                    'args
                    'id)))

(defun unmarshall-call (json-obj)
  "Unmarshall a CALL object from the decoded JSON-OBJ."
  (when (not (json-is-call json-obj))
    (error 'invalid-json-obj :type 'call :json json-obj))
  (let ((call (make-call)))
    (set-fields (call json-obj)
                service
                method
                args id)))

;;; Result object, one per (non-notification) method call
(defstruct result
  "A single result from a method invocation."
  (data)
  (error)
  (warnings)
  (id))

(defun valid-result-p (obj)
  "Return T if OBJ is a valid RESULT object, NIL otherwise."
  (check-type obj result)
  (flet ((slot-has-value (obj slot)
           (and (slot-boundp obj slot)
                (slot-value obj slot))))
    (and (result-p obj)
         ;; Can't (shouldn't) have both data and error slots set.
         (and (or (slot-has-value obj 'result)
                  (slot-has-value obj 'error))
              (not (and (slot-has-value obj 'result)
                        (slot-has-value obj 'error)))))))

(defun marshall-result (obj)
  "Marshall the RESULT object OBJ to JSON."
  (when (and (slot-value obj 'error)
             (slot-value obj 'data))
    (error 'invalid-result-obj :obj obj))
  (json:with-object ()
    (encode-fields (obj)
                   data
                   error
                   warnings
                   id)))

(defun json-is-result (json-obj)
  "Return T if JSON-OBJ could be unmarshalled to a RESULT object, NIL otherwise."
  (and (listp json-obj)
       (every #'listp json-obj)
       (find-fields (json-obj)
                    id)
       (or (find-fields (json-obj)
                        data)
           (find-fields (json-obj)
                        error))
       (not (find-fields (json-obj)
                         data
                         error))))

(defun unmarshall-result (json-obj)
  "Unmarshall a RESULT object from the decoded JSON object JSON-OBJ."
  (when (not (json-is-result json-obj))
    (error 'invalid-json-obj :type 'result :json json-obj))
  (macrolet ((maybe-set-field (obj json-obj &rest fields)
               `(progn
                  ,@(loop for f in fields collect
                         `(when (find-fields (,json-obj) ,f)
                            (set-fields (,obj ,json-obj)
                                        ,f))))))
    (let ((result (make-result)))
      (set-fields (result json-obj)
                  id)
      (maybe-set-fields (result json-obj)
                        data
                        error
                        warnings))))

;;; Request, a list of CALLs
(defun request-p (obj)
  "Return T if OBJ is a REQUEST object, NIL otherwise."
  (and (listp obj)
       (every #'call-p obj)))

(deftype request () (and list (satisfies #'request-p)))

(defun marshall-request (obj)
  (check-type obj request)
  (when (not (every #'valid-call-p obj))
    (error 'invalid-call-obj :obj (find-if-not #'valid-call-p obj)))
  (json:with-array ()
    (loop for call in obj do
         (json:as-array-member ()
           (marshall-call obj)))))

(defun json-is-request (json-obj)
  "Return T if a REQUEST object could be unmarshalled from JSON-OBJ."
  (and (listp json-obj)
       (every #'json-is-call json-obj)))

(defun unmarshall-request (json-obj)
  "Unmarshall a REQUEST object from the decoded JSON object JSON-OBJ."
  (when (not (json-is-request json-obj))
    (error 'invalid-json-obj :type 'request :json json-obj))
  (mapcar #'unmarshall-call json-obj))

;;; Response, a list of RESULTs
(defun response-p (obj)
  "Return T if OBJ is a RESPONSE object, NIL otherwise."
  (and (listp obj)
       (every #'result-p obj)))

(deftype response () (and list (satisfies #'response-p)))

(defun marshall-response (obj)
  "Marshall the RESPONSE object OBJ to JSON."
  (check-type obj response)
  (when (not (every #'valid-result-p obj))
    (error 'invalid-result-obj :obj (find-if-not #'valid-result-p obj)))
  (json:with-array ()
    (loop for result in obj do
         (json:as-array-member ()
           (marshall-result result)))))

(defun json-is-response (json-obj)
  "Return T if a RESPONSE object could be unmarshalled from the decoded JSON object JSON-OBJ."
  (and (listp json-obj)
       (every #'json-is-result json-obj)))

(defun unmarshall-response (json-obj)
  "Unmarshall a RESPONSE object from the decoded JSON object JSON-OBJ."
  (when (not (json-is-response json-obj))
    (error 'invalid-json-obj :type 'response :json json-obj))
  (macar #'unmarshall-result json-obj))
