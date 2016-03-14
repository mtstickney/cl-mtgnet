(defpackage #:mtgnet.sync
  (:use #:cl #:mtgnet-sys)
  (:export #:synchronous-transport
           #:synchronous-tcp-transport
           #:synchronous-tcp-byte-transport
           #:wait))

(in-package #:mtgnet.sync)

(defun wait (promise)
  "Wait for a promise to complete, then return it's value."
  (when (boundp '*rpc-batch*)
    (warn "MTGNET.SYNC:WAIT called with *RPC-BATCH* bound, this will probably deadlock."))
  (let* ((no-vals (gensym "NO-VALS"))
         (result-vals no-vals)
         (error no-vals))
    (blackbird:chain (blackbird:attach promise
                                       (lambda (&rest vals)
                                         (setf result-vals vals)))
      (:catch (ev)
        (setf error ev)))
    (when (and (eq error no-vals)
               (eq result-vals no-vals))
      (error "Synchronous promise is not complete at time of WAIT. Something is very wrong here..."))
    (if (not (eq error no-vals))
        ;; Throw the received error.
        (error error)
        (values-list result-vals))))

(defclass synchronous-transport (transport) ())

(defclass synchronous-tcp-transport (synchronous-transport)
  ((address :initarg :address :accessor tcp-address)
   (port :initarg :port :accessor tcp-port)
   (socket :initarg :socket :accessor socket)
   (element-type :initarg :element-type :accessor tcp-element-type)
   (keep-alive :initarg :keep-alive :accessor keep-alive-p
               :documentation "Whether to use TCP keep-alive on this socket."))
  (:default-initargs :socket nil :keep-alive nil)
  (:documentation "Class for transporting data over a TCP socket."))

(defclass synchronous-tcp-byte-transport (synchronous-tcp-transport)
  ()
  (:default-initargs :element-type '(unsigned-byte 8))
  (:documentation "Transport for sending bytes over a TCP socket."))

(defun keep-socket-alive (socket)
  #+sbcl (setf (sb-bsd-sockets:sockopt-keep-alive socket) t)
  #+allegro (socket:set-socket-options socket :keepalive t)
  #+ccl (ccl::set-socket-options socket :keepalive t)
  #-(or sbcl allegro ccl) (error "KEEP-SOCKET-ALIVE not implemented on this platform."))

(defmethod transport-connect ((transport synchronous-tcp-transport))
  (when (socket transport)
    (transport-disconnect transport))
  (setf (socket transport) (usocket:socket-connect (tcp-address transport)
                                                   (tcp-port transport)
                                                   :element-type (tcp-element-type transport)))
  (when (keep-alive-p transport)
    (keep-socket-alive (usocket:socket (socket transport))))
  (values))

(defmethod transport-disconnect ((transport synchronous-tcp-transport))
  (let ((sock (socket transport)))
    (when sock
      (handler-case (usocket:socket-close sock)
        (error (c)
          (warn "Error signalled while closing socket (~S): ~A" c c)))
      (setf (socket transport) nil))))

(defmethod transport-read ((transport synchronous-tcp-transport) size)
  (check-type size (integer 0))
  ;; TODO: use a data-{input,output}-stream generic function here.
  (let* ((stream (usocket:socket-stream (socket transport)))
         (seq (make-sequence `(vector ,(tcp-element-type transport))
                             size))
         (count (read-sequence seq stream
                               :end size)))
    (when (< count size)
      (error 'end-of-file :stream stream))
    seq))

(defmethod transport-read-into! ((transport synchronous-tcp-transport) seq &key (start 0) end)
  ;; TODO: use a data-{input,output}-stream generic function here.
  (let* ((stream (usocket:socket-stream (socket transport)))
         (size (or end (length seq)))
         (count (read-sequence seq stream :start start :end end)))
    (when (< count size)
      (error 'end-of-file :stream stream))
    (values)))

(defmethod transport-write ((transport synchronous-tcp-transport) data)
  (let ((stream (usocket:socket-stream (socket transport))))
    (write-sequence data stream)))

(defmethod transport-flush ((transport synchronous-tcp-transport))
  (finish-output (usocket:socket-stream (socket transport))))
