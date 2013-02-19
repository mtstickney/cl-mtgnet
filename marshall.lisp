(in-package #:cl-mtgnet)

;;; TODO: have marshall/unmarshall only deal with string<->struct
;;; conversions (don't expose or accept intermediate json-obj)
(defmacro encode-fields ((obj) &rest fields)
  "JSON-encode the FIELDS slots of OBJ. Expects to be run inside a JSON:WITH-OBJECT block."
  `(progn
     ,@(loop for f in fields collect
            (if (listp f)
                `(json:as-object-member ((quote ,(first f)))
                   (funcall ,(second f) (slot-value ,obj (quote ,(first f)))))
                `(json:encode-object-member (quote ,f)
                                            (slot-value ,obj (quote ,f)))))))

(defun json-key (symb)
  "Return the symbol produced by JSON encoding and decoding SYMB."
  (funcall json::*identifier-name-to-key*
           (json:decode-json-from-string
            (funcall json::*json-identifier-name-to-lisp*
                     (json:encode-json-to-string symb)))))

(defmacro find-fields ((alist) &rest fields)
  "Search for all the FIELDS in the JSON-decoded object
ALIST. Expanded code returns T if all fields were found, NIL
otherwise."
  `(and
    ,@(loop for f in fields
         collect `(assoc (json-key (quote ,f))
                         ,alist))))

(defmacro copy-slots-from-alist ((alist obj) &rest fields)
  "Set the FIELDS slots of OBJ from the JSON-decoded object ALIST."
  `(progn
     ,@(loop for f in fields
          collect `(setf (slot-value ,obj (quote ,f))
                         (cdr (assoc (json-key (quote ,f))
                                     ,alist))))))

(defmacro maybe-copy-slots-to-alist ((obj alist) &rest fields)
  "Add entries to ALIST from the slots of OBJ."
  `(progn
     ,@(loop for f in fields
          collect `(push (cons (json-key (quote ,f))
                               (slot-value ,obj (quote ,f)))
                         ,alist))))

;;; Call object, one per method call
(defstruct rpc-call
  "A single method invocation."
  (service)
  (method)
  (args)
  (id))

(defun valid-call-p (obj)
  "Returns T if the object is a valid CALL object."
  (rpc-call-p obj))

(defun marshall-call (obj)
  "Marshall the call object OBJ to JSON."
  (check-type obj rpc-call)
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
                    service
                    method
                    args
                    id)))

(defun intermediate-to-call (obj)
  "Convert an intermediate-format object to a call object."
  (let ((call (make-rpc-call)))
    (copy-slots-from-alist
     (obj call)
     service
     method
     args
     id)
    call))

(defun unmarshall-call (json-string)
  "Unmarshall a CALL object from the decoded JSON-STRING."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (when (not (json-is-call json-obj))
      (format *debug-io* "Invalid rpc-call json object ~S~%" json-obj)
      (error 'invalid-json-obj :type 'rpc-call :json json-obj))
    (intermediate-to-call json-obj)))

;;; Result object, one per (non-notification) method call
(defstruct rpc-result
  "A single result from a method invocation."
  (data)
  (error)
  (warnings)
  (id))

(defun valid-result-p (obj)
  "Return T if OBJ is a valid RESULT object, NIL otherwise."
  (check-type obj rpc-result)
  (flet ((slot-has-value (obj slot)
           (and (slot-boundp obj slot)
                (slot-value obj slot))))
    (and (rpc-result-p obj)
         ;; Can't (shouldn't) have both data and error slots set.
         (and (or (slot-has-value obj 'result)
                  (slot-has-value obj 'error))
              (not (and (slot-has-value obj 'result)
                        (slot-has-value obj 'error)))))))

(define-condition invalid-result-obj ()
  ((obj :initarg :obj)))

(defun result-to-intermediate (result)
  "Convert a result object into an intermediate-format object."
  (check-type result rpc-result "an RPC Result Object")
  (let ((obj '()))
    (maybe-copy-slots-to-alist
     (result obj)
     data
     error
     warnings
     id)
    obj))

(defun marshall-result (obj &optional (data-encoder-func #'json:encode-json))
  "Marshall the RESULT object OBJ to JSON."
  (when (and (slot-value obj 'error)
             (slot-value obj 'data))
    (error 'invalid-result-obj :obj obj))
  (json:with-object ()
    (encode-fields (obj)
                   warnings
                   id)
    (if (slot-value obj 'error)
        (encode-fields (obj)
                       error)
        (encode-fields (obj)
                       (data data-encoder-func)))))

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

(define-condition invalid-json-obj ()
  ((type :initarg :type)
   (json :initarg :json)))

(defun intermediate-to-result (obj)
  "Convert an intermediate-format object to a result object."
  (macrolet ((maybe-set-fields ((obj json-obj) &rest fields)
               `(progn
                  ,@(loop for f in fields collect
                         `(when (find-fields (,json-obj) ,f)
                            (copy-slots-from-alist (,json-obj ,obj)
                                        ,f))))))
    (let ((result (make-rpc-result)))
      (copy-slots-from-alist
       (obj result)
       id)
      (maybe-set-fields (result obj)
                        data
                        error
                        warnings)
      result)))

(defun unmarshall-result (json-string)
  "Unmarshall a RESULT object from the decoded JSON object JSON-STRING."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (when (not (json-is-result json-obj))
      (format *debug-io* "Invalid rpc-result json object ~S~%" json-obj)
      (error 'invalid-json-obj :type 'rpc-result :json json-obj))
    (intermediate-to-result json-obj)))

;;; Request, a list of CALLs
(defun rpc-request-p (obj)
  "Return T if OBJ is a REQUEST object, NIL otherwise."
  (and (listp obj)
       (every #'rpc-call-p obj)))

(deftype rpc-request () '(and list (satisfies rpc-request-p)))

(define-condition invalid-call-object ()
  ((obj :initarg :obj)))

(defun marshall-request (obj)
  (check-type obj rpc-request)
  (when (not (every #'valid-call-p obj))
    (error 'invalid-call-obj :obj (find-if-not #'valid-call-p obj)))
  (json:with-array ()
    (loop for call in obj do
         (json:as-array-member ()
           (marshall-call call)))))

(defun json-is-request (json-obj)
  "Return T if a REQUEST object could be unmarshalled from JSON-OBJ."
  (and (listp json-obj)
       (every #'json-is-call json-obj)))

(defun intermediate-to-request (obj)
  "Convert an intermediate-format object to a request object."
  (mapcar #'intermediate-to-call obj))

(defun unmarshall-request (json-string)
  "Unmarshall a REQUEST object from the decoded JSON object JSON-OBJ."
  (let ((json-obj (json:decode-json-from-string  json-string)))
    (when (not (json-is-request json-obj))
      (format *debug-io* "Invalid rpc-request json object ~S~%" json-obj)
      (error 'invalid-json-obj :type 'rpc-request :json json-obj))
    (intermediate-to-request json-obj)))

;;; Response, a list of RESULTs
(defun rpc-response-p (obj)
  "Return T if OBJ is a RESPONSE object, NIL otherwise."
  (and (listp obj)
       (every #'rpc-result-p obj)))

(deftype rpc-response () '(and list (satisfies response-p)))

;; (defun marshall-response (obj)
;;   "Marshall the RESPONSE object OBJ to JSON."
;;   (check-type obj rpc-response)
;;   (when (not (every #'valid-result-p obj))
;;     (error 'invalid-result-obj :obj (find-if-not #'valid-result-p obj)))
;;   (json:with-array ()
;;     (loop for result in obj do
;;          (json:as-array-member ()
;;            (marshall-result result)))))

(defun json-is-response (json-obj)
  "Return T if a RESPONSE object could be unmarshalled from the decoded JSON object JSON-OBJ."
  (and (listp json-obj)
       (every #'json-is-result json-obj)))

(defun unmarshall-response (json-string)
  "Unmarshall a RESPONSE object from the decoded JSON object JSON-STRING."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (when (not (json-is-response json-obj))
      (format *debug-io* "Invalid rpc-response json object ~S~%" json-obj)
      (error 'invalid-json-obj :type 'rpc-response :json json-obj))
    (mapcar #'intermediate-to-result json-obj)))
