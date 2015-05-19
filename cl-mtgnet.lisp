;;;; cl-mtgnet.lisp

(in-package #:mtgnet-sys)

;; TODO: RPC method calls are returning a whole result structure, not
;; the data/throwing an error.

(defvar *default-encoder* #'json:encode-json)

;; Utility funcs
(declaim (inline has-key))
(defun has-key (key map)
  (nth-value 1 (gethash key map)))

;; Stuff for type checks
(deftype id () '(or string fixnum))

(defclass rpc-connection ()
  ((transport :initarg :transport :accessor connection-transport)
   (framer :initarg :framer :accessor connection-framer)
   (next-response :initform nil :accessor next-response-promise)
   (result-bucket :initform (make-hash-table :test 'equal) :accessor result-bucket))
  (:documentation "Class representing a connection to an RPC server"))

(defgeneric connect (con)
  (:documentation "Connect CON to the remote end.")
  (:method ((con rpc-connection))
    (transport-connect (connection-transport con))))

(defgeneric disconnect (con)
  (:documentation "Disconnect the connection CON.")
  (:method ((con rpc-connection))
    (transport-disconnect (connection-transport con))))

(defgeneric send-frame (con &rest datae)
  (:documentation "Use CON's framer to write a frame over CON's transport. Mostly for extension convenience.")
  (:method ((con rpc-connection) &rest datae)
    (send-frame* con datae)))

(defgeneric send-frame* (con datae)
  (:documentation "Use CON's framer to write a frame over CON's transport. Wraps WRITE-FRAME, mostly for extension convenience.")
  (:method ((con rpc-connection) datae)
    (write-frame (connection-framer con)
                 (connection-transport con)
                 datae)))

(defgeneric receive-frame (con)
  (:documentation "Read and return a frame of data sent over CON. Wraps READ-FRAME, mostly for extension convenience.")
  (:method ((con rpc-connection))
    (read-frame (connection-framer con)
                (connection-transport con))))

(defgeneric send-response (con response)
  (:documentation "Marshall and send RESPONSE over CON.")
  (:method ((con rpc-connection) response)
    (check-type response rpc-response)
    (let* ((response-message (with-output-to-string (json:*json-output*)
                               (marshall-rpc-response response)))
           (request-data (trivial-utf-8:string-to-utf-8-bytes response-message)))
      (send-frame con request-data))))

(defgeneric read-response (con)
  (:documentation "Read and unmarshall an RPC response from CON.")
  (:method ((con rpc-connection))
    (blackbird:multiple-promise-bind (data) (receive-frame con)
      (unmarshall-rpc-response (trivial-utf-8:utf-8-bytes-to-string data)))))

(define-condition duplicate-result-id ()
  ((id)))

(defgeneric add-result (con result)
  (:documentation "Add an RPC result to the results received by CON.")
  (:method ((con rpc-connection) result)
    (check-type result rpc-result)
    (let* ((id (rpc-result-id result))
           (bucket (result-bucket con)))
      (when (has-key id bucket)
        (error 'duplicate-result-id :id id))
      (setf (gethash id bucket)
            result)
      (values))))

(defgeneric process-next-response (con)
  (:documentation "Process the next response, without triggering an additional read if one is already in progress.")
  (:method ((con rpc-connection))
    (let ((next-promise (next-response-promise con)))
      (if (or (not (blackbird:promisep next-promise))
              (blackbird:promise-finished-p next-promise))
          ;; If the previous next-promise is complete, go read another response.
          (setf (next-response-promise con)
                (blackbird:multiple-promise-bind (response) (read-response con)
                  (map nil (lambda (result) (add-result con result)) response)
                  (values)))
          ;; Otherwise return the existing promise.
          next-promise))))

(defgeneric send-request (con request)
  (:documentation "Marshall and send REQUEST over CON.")
  (:method ((con rpc-connection) request)
    (check-type request rpc-request)
    (let* ((encoded-request (with-output-to-string (json:*json-output*)
                             (marshall-rpc-request request)))
           (request-data (trivial-utf-8:string-to-utf-8-bytes
                          encoded-request)))
      (send-frame con request-data))))

(defgeneric read-request (con)
  (:documentation "Read and unmarshall an RPC request from CON.")
  (:method ((con rpc-connection))
    (blackbird:multiple-promise-bind (data) (receive-frame con)
      (unmarshall-rpc-request (trivial-utf-8:utf-8-bytes-to-string data)))))

;; TODO: Error approach: set error-status/condition on connection on
;; read; when a future completes later, it will either return the
;; received result (if one was retrieved without error), or re-signal
;; the stored condition (is it reasonable to re-throw the same
;; condition more than once? What if error handling has been done
;; in-between?).
(declaim (ftype (function (rpc-connection id) rpc-result)
                read-result-with-id))
(defun read-result-with-id (con id)
  "Read responses from CON, storing results until a request containing
a response with id ID arrives. Will process all results in a response
before returning."
  (check-type id id)
  ;; FIXME: This needs a timeout, or we might keep chipmunking results
  ;; forever if the remote end never sends the right id back.
  (labels ((get-result ()
             (let ((bucket (result-bucket con)))
               (if (has-key id bucket)
                   (prog1 (gethash id bucket)
                     (remhash id bucket))
                   (blackbird:multiple-promise-bind () (process-next-response con)
                     (get-result))))))
    (get-result)))

(define-condition remote-warning (warning) (msg code))
(define-condition remote-error (error) (type msg code))

(declaim (ftype (function (rpc-connection id)
                          blackbird:promise)
                make-result-promise))
(defun make-result-promise (con id)
  "Return a promise that produces a result with an id of ID from CON."
  (blackbird:multiple-promise-bind (result) (read-result-with-id con id)
    (mapc (lambda (w) (warn 'remote-warning
                            :msg (rpc-error-message w)
                            :code (rpc-error-code w)))
          (rpc-result-warnings result))
    (let* ((error (rpc-result-error result))
           (condition-type (and error (rpc-error-data error))))
      (when error
        (error 'remote-error
               :msg (rpc-error-message error)
               :code (rpc-error-code error)
               :type (if (typep condition-type 'string) condition-type nil))))
    (rpc-result-data result)))

;; FIXME: parent type and/or name needs adjusting
(define-condition event-loop-exited (simple-error) ())

(defun wait (promise)
  "Wait for a promise to complete, then return it's value."
  (when (boundp '*rpc-batch*)
    (warn "CL-MTGNET:WAIT called with *RPC-BATCH* bound, this will probably deadlock."))
  (let ((finished nil)
        result-vals
        error)
    (blackbird:chain (blackbird:attach promise
                                       (lambda (&rest vals)
                                         (setf result-vals vals)))
      (:catch (ev)
        (setf error ev))
      (:finally ()
        (setf finished t)))

    (loop with res = 1
       while (and (/= res 0)
                  (not finished))
       do (setf res (uv:uv-run (cl-async-base:event-base-c cl-async-base:*event-base*)
                               (cffi:foreign-enum-value 'uv:uv-run-mode :run-once))))

    (cond
      ((and finished error) (error error))
      (finished (values-list result-vals))
      (t (error 'event-loop-exited "No more events to process")))))

(declaim (ftype (function (id id list &key (:notification boolean)) rpc-call)
                make-call-obj))
(defun make-call-obj (service method parameters &key notification)
  (make-rpc-call :service (if (typep service 'string) service (symbol-name service))
                 :method (if (typep method 'string) method (symbol-name method))
                 :args parameters
                 :id (if notification
                         nil
                         (random most-positive-fixnum))))

(defun rpc-call-promise (con rpc-call)
  (check-type rpc-call rpc-call)
  (let ((id (rpc-call-id rpc-call)))
    (if id
        (make-result-promise con id)
        ;; null id -> notification, return a trivial promise.
        (values))))

;; TODO: add timeout and keepalive parameters if it seems reasonable
(declaim (ftype (function (rpc-connection id id list &key (:notification boolean))
                          blackbird:promise)
                invoke-rpc-method))
(defun invoke-rpc-method (con service method args &key notification)
  (let ((call (make-call-obj service method args :notification notification)))
    (declare (special *rpc-batch*))
    (cond
      ((boundp '*rpc-batch*)
       ;; If *rpc-batch* is bound we want to return a stub promise and
       ;; let WITH-BATCH-CALLS trigger the read and complete it.
       (blackbird:with-promise (resolve reject :resolve-fn resolve-it)
         (push (cons call resolve-it) *rpc-batch*)))
      (t (send-request con (list call))
         (rpc-call-promise con call)))))

(defmacro with-batch-calls ((con &optional (batch-req nil batch-supplied-p)) &body body)
  "Arrange for RPC calls in this block to collect their calls into one
request, which will be sent at the end of the block."
  `(let ((*rpc-batch* ,(if batch-supplied-p batch-req '())))
     (declare (special *rpc-batch*))
     ,@body
     (let ((batch (reverse *rpc-batch*)))
       (send-request ,con (mapcar #'first batch))
       (blackbird:all (mapcar (lambda (cell)
                                (let ((resolver (cdr cell))
                                      (promise (rpc-call-promise ,con (car cell))))
                                  (funcall resolver promise)
                                  promise))
                              batch)))))

(defmacro bind-args ((arg-var encoder-var &optional typespec-var) arg-obj &body body)
  (let ((typespec-var (if typespec-var typespec-var (gensym "TYPESPEC"))))
    `(destructuring-bind (,arg-var &optional
                                   (,encoder-var '*default-encoder*)
                                   ,typespec-var)
         ,arg-obj
       (declare (ignore ,typespec-var))
       ,@body)))

;; TODO: add typespecs for return value and arguments
;; Note: args is a list of lists, for providing typespecs in the
;; future.
;; (defmacro define-rpc-method ((method &optional service  &key notification async) &body args)
;;   (check-type method (or symbol string))
;;   (check-type service (or symbol string null) "a symbol, string, or NIL")
;;   (check-type notification boolean)
;;   (let* ((con-symb (gensym "CON"))
;;          (service-symb (gensym "SERVICE"))
;;          (promise-symb (gensym "PROMISE"))
;;          (service-string (etypecase service
;;                            (null "")
;;                            (symbol (format nil "~A-" (symbol-name service)))
;;                            (string (format nil "~A-" service))))
;;          (method-string (etypecase method
;;                           (string method)
;;                           (symbol (symbol-name method))))
;;          (funcname (intern (string-upcase (concatenate 'string service-string method-string))))
;;          (arglist (mapcar #'first args))
;;          (passed-args (cons 'list (mapcar (lambda (a) (bind-args (arg encoder) a
;;                                                         `(list ,arg ,encoder)))
;;                                           args))))
;;     `(defun ,funcname ,(cons con-symb
;;                              (if (null service)
;;                                  (cons service-symb arglist)
;;                                  arglist))
;;        (let ((,promise-symb (invoke-rpc-method ,con-symb ,(etypecase service
;;                                                                      (null service-symb)
;;                                                                      (symbol `(quote ,service))
;;                                                                      (string service))
;;                                                ,(if (symbolp method)
;;                                                     `(quote ,method)
;;                                                     method)
;;                                                ,passed-args
;;                                                :notification ,notification)))
;;          ,(if async
;;               promise-symb
;;               `(wait ,promise-symb))))))
