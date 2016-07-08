(in-package #:mtgnet-sys)

(defclass transport () ())

;; Should transport-connect/disconnect use the :APPEND method
;; combination? Seems to make sense for inheriting from e.g. input-
;; and output- classes. What does that do to return values (useful for
;; e.g. string-stream transport punning)?
(defgeneric transport-connect (transport)
  (:documentation "Connect TRANSPORT to the remote end, if necessary.")
  (:method (transport)))

(defgeneric transport-disconnect (transport &key abort)
  (:documentation "Disconnect TRANSPORT from the remote end, if necessary.")
  (:method (transport &key abort)
    (declare (ignore abort))))

(defgeneric transport-read (transport size)
  (:documentation "Read the next SIZE elements of data from TRANSPORT."))

(defgeneric transport-read-into! (transport seq &key start end)
  (:documentation "Destructively modify SEQ with elements from TRANSPORT."))

(defgeneric transport-write (transport data)
  (:documentation "Send DATA to the remote end of TRANSPORT."))

(defgeneric transport-flush (transport)
  (:documentation "Flush buffers associated with TRANSPORT, if any.")
  (:method (transport)
    (values)))

;;; Useful for testing/debugging
(defclass string-output-transport (synchronous-transport)
  ((stream :accessor output-stream)
   (external-format :initarg :external-format :accessor external-format)))

(defmethod initialize-instance :after ((instance string-output-transport) &key)
  (setf (output-stream instance) (flexi-streams:make-flexi-stream
                                  (flexi-streams:make-in-memory-output-stream)
                                  :external-format (external-format instance))))

(defmethod transport-disconnect ((transport string-output-transport) &key abort)
  (if abort
      ""
      (let* ((stream (flexi-streams:flexi-stream-stream (output-stream transport)))
             (seq (flexi-streams:get-output-stream-sequence stream)))
        (flexi-streams:octets-to-string seq :external-format (external-format transport)))))

(defmethod transport-write ((transport string-output-transport) data)
  (let ((stream (output-stream transport)))
    (write-sequence data stream)))

(defmethod transport-flush ((transport string-output-transport))
  (finish-output (output-stream transport)))

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
