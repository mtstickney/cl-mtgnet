;;;; cl-mtgnet.lisp

(in-package #:cl-mtgnet)

;; TODO: RPC method calls are returning a whole result structure, not
;; the data/throwing an error.

;; Storage for out-of-order responses
(defvar *response-bucket* (make-hash-table :test 'equal))


;;; Bucket stuff for receiving responses
(define-condition duplicate-result-id ()
  ((id)))

(defun read-response-with-id (id sock-stream)
  "Read responses from SOCK-STREAM, adding results to
*RESPONSE-BUCKET* until a request containing a response with id ID
arrives. Will process all results in a response before returning."
  (declare (special *response-bucket*))
  ;; TODO: this is probably not the right thing to do, and has case issues
  (when (symbolp id)
      (setf id (string-downcase (symbol-name id))))
  (labels ((contains (id bucket)
             (format *debug-io* "Contains ~S ~S~%" id bucket)
             (nth-value 1 (gethash id bucket)))
           (add-to-bucket (result)
             (format *debug-io* "add-to-bucket ID is ~S~%" (rpc-result-id result))
             (when (contains (rpc-result-id result) *response-bucket*)
               (error 'duplicate-result-id :id (rpc-result-id result)))
             (setf (gethash (rpc-result-id result) *response-bucket*)
                   result)
             (values)))
    (format *debug-io* "Function ID is ~S~%" id)
    (when (contains id *response-bucket*)
      (format *debug-io* "Found the right response, returning~%")
      (prog1
          (gethash id *response-bucket*)
        (remhash id *response-bucket*)))

    (loop until (contains id *response-bucket*)
       with response = (unmarshall-response (recv-string sock-stream))
       do (mapc #'(lambda (result)
                    (format *debug-io* "Adding result ~S~%" result)
                    (add-to-bucket result)) response)
         (format *debug-io* "Added response to bucket~%"))
    (prog1 (gethash id *response-bucket*)
      (remhash id *response-bucket*))))

;;; Wire format wrappers
(defun send-string (sock-stream str)
  (let ((data (trivial-utf-8:string-to-utf-8-bytes str)))
    (cl-netstring+:write-netstring-bytes sock)
    (finish-output sock-stream)))

(defun recv-string (sock-stream)
  (let ((data (cl-netstring+:read-netstring-data sock-stream)))
    (trivial-utf-8:utf-8-bytes-to-string data)))

;;; TODO: set up conditions for this layer, if necessary.
(defun write-request (stream request)
  "Marshal REQUEST and send it over STREAM."
  (let* ((request-data (with-output-to-string (json:*json-output*)
                         (marshall-request request))))
    (send-string stream request-data)))

(defun make-result-future (id stream)
  "Return a (funcallable) future that produces a response with an id of ID from STREAM."
  (lambda ()
    (read-response-with-id id stream)))

(defun wait (future)
  "Wait for a future to complete, then return it's value."
  (when (boundp '*rpc-batch*)
    (warn "CL-MTGNET:WAIT called with *RPC-BATCH* bound, this will probably deadlock."))
  (funcall future))

(defun make-call-obj (service method parameters &key notification)
  (make-rpc-call :service service
             :method method
             :args parameters
             :id (if notification
                     nil
                     (gensym "CALL"))))

;; TODO: add timeout and keepalive parameters if it seems reasonable
(defun invoke-rpc-method (sock service method args &key notification)
  (let ((call (make-call-obj service method args))
        (batch '()))
    (declare (special *rpc-batch*))
    ;; if *rpc-batch* is bound, just add the call to it
    (if (boundp '*rpc-batch*)
      (push call *rpc-batch*)
      (progn
        (push call batch)
        (write-request sock batch)
        (if notification
            (values)
            (make-result-future (rpc-call-id call) sock))))))


(defmacro with-batch-calls ((stream &optional (batch-req nil batch-supplied-p)) &body body)
  `(let ((*rpc-batch* ,(if batch-supplied-p batch-req '())))
     (declare (special *rpc-batch*))
     ,@body
     (write-request ,stream (reverse  *rpc-batch*))))

;; TODO: allow strings for method and service
;; TODO: add typespecs for return value and arguments
(defmacro define-rpc-method ((method &optional service  &key notification) &body args)
  (check-type method symbol)
  (check-type service symbol)
  (check-type service (or symbol null) "a SYMBOL or NIL")
  (let* ((sock-symb (gensym "SOCK"))
         (service-symb (gensym "SERVICE"))
         (service-string (if service
                             (format nil "~A-" (symbol-name service)) ""))
         (method-string (symbol-name method))
         (funcname (intern (concatenate 'string service-string method-string)))
         (arglist (mapcar #'first args)))

    `(defun ,funcname ,(cons sock-symb
                             (if (null service)
                                 (cons service-symb arglist)
                                 arglist))
       (invoke-rpc-method ,sock-symb ,(if service `(quote ,service)
                                 service-symb)
                 (quote ,method) ,(cons 'list arglist)
                 ,@(if notification '(:notification t) nil)))))
