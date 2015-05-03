(in-package #:mtgnet-sys)

(defclass transport () ())

(defclass synchronous-transport (transport) ())

(defclass asynchronous-transport (transport) ())

;; Should transport-connect/disconnect use the :APPEND method
;; combination? Seems to make sense for inheriting from e.g. input-
;; and output- classes. What does that do to return values (useful for
;; e.g. string-stream transport punning)?
(defgeneric transport-connect (transport)
  (:documentation "Connect TRANSPORT to the remote end, if necessary.")
  (:method (transport)))

(defgeneric transport-disconnect (transport)
  (:documentation "Disconnect TRANSPORT from the remote end, if necessary.")
  (:method (transport)))

(defgeneric transport-read (transport size)
  (:documentation "Read the next SIZE elements of data from TRANSPORT."))

(defgeneric transport-read-into! (transport seq &key start end)
  (:documentation "Destructively modify SEQ with elements from TRANSPORT."))

(defgeneric transport-write (transport data &key flush)
  (:documentation "Send DATA to the remote end of TRANSPORT, optionally flushing the write."))

;;; Useful for testing/debugging
(defclass string-output-transport (synchronous-transport)
  ((stream :accessor output-stream)
   (external-format :initarg :external-format :accessor external-format)))

(defmethod initialize-instance :after ((instance string-output-transport) &key)
  (setf (output-stream instance) (flexi-streams:make-flexi-stream
                                  (flexi-streams:make-in-memory-output-stream)
                                  :external-format (external-format instance))))

(defmethod transport-disconnect ((transport string-output-transport))
  (let* ((stream (flexi-streams:flexi-stream-stream (output-stream transport)))
         (seq (flexi-streams:get-output-stream-sequence stream)))
    (flexi-streams:octets-to-string seq :external-format (external-format transport))))

(defmethod transport-write ((transport string-output-transport) data &key flush)
  (let ((stream (output-stream transport)))
    (write-sequence data stream)
    (when flush
      (finish-output stream))))

(defclass string-input-transport (synchronous-transport)
  ((stream :accessor input-stream)
   (data :initarg :data :accessor input-data)
   (external-format :initarg :external-format :accessor external-format)))

(defmethod transport-connect ((transport string-input-transport))
  (let* ((bytes (flexi-streams:string-to-octets (input-data transport)
                                                :external-format (external-format transport)))
         (mem-stream (flexi-streams:make-in-memory-input-stream bytes )))
    (setf (input-stream transport)
          (flexi-streams:make-flexi-stream mem-stream
                                           :external-format (external-format transport)))))

(defmethod transport-read ((transport string-input-transport) size)
  (let ((seq (make-array size :element-type '(unsigned-byte 8))))
    (transport-read-into! transport seq)
    seq))

(defmethod transport-read-into! ((transport string-input-transport) seq &key (start 0) end)
  (read-sequence seq (input-stream transport) :start start :end end))

;;; Actual serious-business transports.
(defclass synchronous-tcp-transport (synchronous-transport)
  ((address :initarg :address :accessor tcp-address)
   (port :initarg :port :accessor tcp-port)
   (socket :initarg :socket :accessor socket)
   (element-type :initarg :element-type :accessor tcp-element-type))
  (:default-initargs :socket nil)
  (:documentation "Class for transporting messages over a TCP socket."))

(defclass synchronous-tcp-byte-transport (synchronous-tcp-transport)
  ()
  (:default-initargs :element-type '(unsigned-byte 8))
  (:documentation "Transport for sending bytes over a TCP socket."))

(defmethod transport-connect ((transport synchronous-tcp-transport))
  (when (socket transport)
    (transport-disconnect transport))
  (setf (socket transport) (usocket:socket-connect (tcp-address transport)
                                                   (tcp-port transport)
                                                   :element-type (tcp-element-type transport)))
  (values))

(defmethod transport-disconnect ((transport synchronous-tcp-transport))
  (let ((sock (socket transport)))
    (when sock
      (usocket:socket-close sock)
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
