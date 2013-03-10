;;;; cl-mtgnet.lisp

(in-package #:cl-mtgnet)

;; TODO: RPC method calls are returning a whole result structure, not
;; the data/throwing an error.

;; Utility funcs
(declaim (inline has-key))
(defun has-key (key map)
  (check-type map hash-table)
  (nth-value 1 (gethash key map)))

(defclass rpc-connection ()
  ((address :initarg :address :accessor connection-address)
   (port :initarg :port :accessor connection-port)
   (socket :accessor socket)
   (result-bucket :initform (make-hash-table :test 'equal)
                  :accessor result-bucket))
  (:documentation "Class representing a connection to an RPC server"))

(defmethod initialize-instance :after ((con rpc-connection) &rest initargs)
  (declare (ignore initargs))
  (setf (socket con) (usocket:socket-connect (connection-address con)
                                             (connection-port con)
                                             :element-type '(unsigned-byte 8))))

(defgeneric connection-disconnect (con)
  (:documentation "Disconnect the connection CON")
  (:method ((con rpc-connection))
    (usocket:socket-close (socket con))))

(defgeneric read-response (con)
  (:documentation "Read and unmarshall an RPC response from CON.")
  (:method ((con rpc-connection))
    (unmarshall-response (recv-string (usocket:socket-stream (socket con))))))

(defgeneric send-request (con request &key flush)
  (:documentation "Marshall and send REQUEST over CON. If FLUSH is T, flush the output buffer.")
  (:method ((con rpc-connection) request &key (flush t))
    (check-type request rpc-request)
    (let* ((encoded-request (with-output-to-string (json:*json-output*)
                             (marshall-request request)))
           (request-data (trivial-utf-8:string-to-utf-8-bytes
                          encoded-request))
           (stream (usocket:socket-stream (socket con))))
      (cl-netstring+:write-netstring-bytes stream
                                           request-data)
      (when flush
        (finish-output stream)))))

(define-condition duplicate-result-id ()
  ((id)))

(defgeneric add-result (con result)
  (:documentation "Add an RPC result to the results received by CON.")
  (:method ((con rpc-connection) (result rpc-result))
    (let ((id (rpc-result-id result))
          (bucket (result-bucket con)))
      (when (has-key id bucket)
        (error 'duplicate-result-id :id id))
      (setf (gethash id bucket) result)
      (values))))

;; TODO: Error approach: set error-status/condition on connection on
;; read; when a future completes later, it will either return the
;; received result (if one was retrieved without error), or re-signal
;; the stored condition (is it reasonable to re-throw the same
;; condition more than once? What if error handling has been done
;; in-between?).
(defun read-result-with-id (con id)
  "Read responses from CON, storing results until a request containing
a response with id ID arrives. Will process all results in a response
before returning."
  ;; TODO: this is probably not the right thing to do, and has case issues
  (let ((id (if (symbolp id)
                (string-downcase (symbol-name id))
                id))
        (bucket (result-bucket con)))
    (loop until (has-key id bucket)
       with response = (read-response con)
       ;; a response is a list of results
       do (mapc #'(lambda (r) (add-result con r))
                response))
    ;; Return the result and remove it from the bucket
    (prog1 (gethash id bucket)
      (remhash id bucket))))

(defun make-result-future (con id)
  "Return a future that produces a result with an id of ID from CON."
  (lambda ()
    (read-response-with-id con id)))

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
(defun invoke-rpc-method (con service method args &key notification)
  (let ((call (make-call-obj service method args)))
    (declare (special *rpc-batch*))
    ;; if *rpc-batch* is bound, just add the call to it
    (if (boundp '*rpc-batch*)
        (push call *rpc-batch*)
        (write-request (socket con) (list call)))
    (if notification
        (values)
        (make-result-future con (rpc-call-id call)))))

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
