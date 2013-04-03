;;;; cl-mtgnet.lisp

(in-package #:cl-mtgnet)

;; TODO: RPC method calls are returning a whole result structure, not
;; the data/throwing an error.

(defvar *default-encoder* #'json:encode-json)

;; Utility funcs
(declaim (inline has-key))
(defun has-key (key map)
  (nth-value 1 (gethash key map)))

;; Stuff for type checks
(deftype id () '(or symbol string))
(deftype future () `(function))

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

(declaim (inline connect))
(defun connect (address port)
  "Create and return an RPC-CONNECTION for ADDRESS and PORT."
  (make-instance 'rpc-connection :address address :port port))

(defgeneric connection-disconnect (con)
  (:documentation "Disconnect the connection CON")
  (:method ((con rpc-connection))
    (usocket:socket-close (socket con))))

(defgeneric read-response (con)
  (:documentation "Read and unmarshall an RPC response from CON.")
  (:method ((con rpc-connection))
    (let* ((stream (usocket:socket-stream (socket con)))
           (str (trivial-utf-8:utf-8-bytes-to-string
                 (cl-netstring+:read-netstring-data stream))))
      (unmarshall-rpc-response str))))

(defgeneric send-request (con request &key flush)
  (:documentation "Marshall and send REQUEST over CON. If FLUSH is T, flush the output buffer.")
  (:method ((con rpc-connection) request &key (flush t))
    (check-type request rpc-request)
    (let* ((encoded-request (with-output-to-string (json:*json-output*)
                             (marshall-rpc-request request)))
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
  (:method ((con rpc-connection) result)
    (check-type result rpc-result)
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
(declaim (ftype (function (rpc-connection id) rpc-result)
                read-response-with-id))
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

(declaim (ftype (function (rpc-connection id)
                          future)
                make-result-future))
(defun make-result-future (con id)
  "Return a future that produces a result with an id of ID from CON."
  (lambda ()
    (read-result-with-id con id)))

(define-condition remote-warning (warning) (msg code))
(define-condition remote-error (error) (type msg code))
(declaim (ftype (function (future)
                          rpc-result)
                wait))
(defun wait (future)
  "Wait for a future to complete, then return it's value."
  (when (boundp '*rpc-batch*)
    (warn "CL-MTGNET:WAIT called with *RPC-BATCH* bound, this will probably deadlock."))
  (let* ((result (funcall future))
         (error (rpc-result-error result)))
    (mapc (lambda (w) (warn 'remote-warning
                            :msg (rpc-error-message w)
                            :code (rpc-error-code w)))
          (rpc-result-warnings result))
    (when error
      (let ((condition-type (rpc-error-data error)))
        (error 'remote-error
               :msg (rpc-error-message error)
               :code (rpc-error-code error)
               :type (if (typep condition-type 'string) type nil))))
    (rpc-result-data result)))

(declaim (ftype (function (id id list &key (:notification boolean)) rpc-call)
                make-call-obj))
(defun make-call-obj (service method parameters &key notification)
  (make-rpc-call :service (symbol-name service)
                 :method (symbol-name method)
                 :args parameters
                 :id (if notification
                         nil
                         (gensym "CALL"))))

;; TODO: add timeout and keepalive parameters if it seems reasonable
(declaim (ftype (function (rpc-connection id id list &key (:notification boolean))
                          (values &optional future))
                invoke-rpc-method))
(defun invoke-rpc-method (con service method args &key notification)
  (let ((call (make-call-obj service method args)))
    (declare (special *rpc-batch*))
    ;; if *rpc-batch* is bound, just add the call to it
    (if (boundp '*rpc-batch*)
        (push call *rpc-batch*)
        (send-request con (list call)))
    (if notification
        (values)
        (make-result-future con (rpc-call-id call)))))

(defmacro with-batch-calls ((con &optional (batch-req nil batch-supplied-p)) &body body)
  "Arrange for RPC calls in this block to collect their calls into one
request, which will be sent at the end of the block."
  `(let ((*rpc-batch* ,(if batch-supplied-p batch-req '())))
     (declare (special *rpc-batch*))
     ,@body
     (send-request ,con (reverse  *rpc-batch*))))

(defmacro bind-args ((arg-var encoder-var &optional typespec-var) arg-obj &body body)
  (let ((encoder-var (if encoder-var encoder-var (gensym "ENCODER")))
        (typespec-var (if typespec-var typespec-var (gensym "TYPESPEC"))))
    `(destructuring-bind (,arg-var &optional
                                   (,encoder-var *default-encoder*)
                                   ,typespec-var)
         ,arg-obj
       (declare (ignore ,typespec-var))
       ,@body)))

;; TODO: allow strings for method and service
;; TODO: add typespecs for return value and arguments
;; Note: args is a list of lists, for providing typespecs in the
;; future.
(defmacro define-rpc-method ((method &optional service  &key notification) &body args)
  (check-type method symbol)
  (check-type service (or symbol null) "a SYMBOL or NIL")
  (check-type notification boolean)
  (flet ((passed-arg-forms (args)
           (loop for a in args
              collect (destructuring-bind (arg &optional
                                               (encoder *default-encoder*)
                                               typespec)
                          (declare (ignore typespec))
                        `(list ,arg ,encoder)))))
    (let* ((sock-symb (gensym "SOCK"))
           (service-symb (gensym "SERVICE"))
           (service-string (if service
                               (format nil "~A-" (symbol-name service))
                               ""))
           (method-string (symbol-name method))
           (funcname (intern (concatenate 'string service-string method-string)))
           (arglist (mapcar #'first args))
           (passed-args (mapcar (lambda (a) (bind-args (arg encoder)
                                              (list arg encoder)))
                                args)))
      `(defun ,funcname ,(cons sock-symb
                               (if (null service)
                                   (cons service-symb arglist)
                                   arglist))
         (invoke-rpc-method ,sock-symb ,(if service `(quote ,service)
                                            service-symb)
                            (quote ,method)
                            ,passed-args
                            :notification ,notification)))))
