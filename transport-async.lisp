(defpackage #:mtgnet.async
  (:use #:cl #:mtgnet-sys)
  (:export #:asynchronous-transport
           #:asynchronous-tcp-transport))

(in-package #:mtgnet.async)

(defclass asynchronous-transport (transport) ())

(defclass asynchronous-tcp-transport (asynchronous-transport)
  ((address :initarg :address :accessor tcp-address)
   (port :initarg :port :accessor tcp-port)
   (stream :initarg :stream :accessor socket-stream)
   (read-callback :initarg :read-cb :accessor read-cb)
   (write-callback :initarg :write-cb :accessor write-cb)
   (error-callback :initarg :err-cb :accessor error-cb))
  (:default-initargs :stream nil)
  (:documentation "Class for transporting data over a TCP socket asynchronously."))

(defmethod transport-connect ((transport asynchronous-tcp-transport))
  (when (socket-stream transport)
    (transport-disconnect transport))
  (blackbird:with-promise (resolve reject)
    ;; We want to catch an initial error during connection and fail
    ;; this promise:
    (setf (error-cb transport)
          (lambda (ev)
            (reject ev)
            ;; un-register ourselves so we don't trigger on subsequent
            ;; errors.
            (setf (error-cb transport) nil)))

    (setf (socket-stream transport)
          (as:tcp-connect (tcp-address transport) (tcp-port transport)
                          (lambda (socket stream)
                            (when (and (slot-boundp transport 'read-callback)
                                       (read-cb transport))
                              (funcall (read-cb transport) socket stream)))
                          :write-cb (lambda (socket)
                                      (when (and (slot-boundp transport 'write-callback)
                                                 (write-cb transport))
                                        (funcall (write-cb transport) socket)))
                          :event-cb (lambda (ev)
                                      (when (and (slot-boundp transport 'error-callback)
                                                 (error-cb transport))
                                        (funcall (error-cb transport) ev)))
                          :connect-cb (lambda (socket)
                                        (resolve socket))
                          :stream t
                          :dont-drain-read-buffer t))))

;; FIXME: do we need to run the error callback for any in-progress operation?
(defmethod transport-disconnect ((transport asynchronous-tcp-transport))
  (blackbird:with-promise (resolve reject)
    (when (socket-stream transport)
      (handler-case
          (close (socket-stream transport))
        (serious-condition (c)
          (reject c))))
    (resolve)))

(defun unregister-op! (transport)
  (setf (read-cb transport) nil
        (write-cb transport) nil
        (error-cb transport) nil))

(defmethod transport-read ((transport asynchronous-tcp-transport) size)
  (let* ((seq (make-array size :element-type '(unsigned-byte 8))))
    (blackbird:attach (transport-read-into! transport seq)
                      (lambda ()
                        seq))))

;; FIXME: the state tracking in here is kinda shaky, it'd be better to
;; use a proper queuing scheme.
(defmethod transport-read-into! ((transport asynchronous-tcp-transport) seq &key (start 0) end)
  (let ((read-pos start)
        (end (or end (length seq))))
    (blackbird:with-promise (resolve reject :resolve-fn resolve-fn :reject-fn reject-fn)
      (labels ((resume-read (socket stream)
                 (declare (ignore socket))
                 ;; We ran out of data before, and are now being woken
                 ;; back up.
                 (read-available stream)
                 ;; If we've got enough, unregister the wakeup routine.
                 (when (= read-pos end)
                   (unregister-op! transport)))
               (read-available (stream)
                 (let ((bytes (read-sequence seq stream :start read-pos :end end)))
                   (incf read-pos bytes)
                   (when (= read-pos end)
                     ;; Done, complete the promise.
                     (resolve)))))
        ;; Try to use already buffere data first
        (read-available (socket-stream transport))
        ;; If that wasn't enough, set a handler to try again when
        ;; there's more data to read.
        (setf (read-cb transport) #'resume-read
              (write-cb transport) nil
              (error-cb transport) (lambda (ev)
                                     (unregister-op! transport)
                                     (reject ev)))))))

(defmethod transport-write ((transport asynchronous-tcp-transport) data)
  (blackbird:with-promise (resolve reject)
    (setf (read-cb transport) nil
          (write-cb transport) (lambda (socket)
                                 (declare (ignore socket))
                                 ;; Write's complete, unregister the
                                 ;; handlers
                                 (unregister-op! transport)
                                 (resolve))
          (error-cb transport) (lambda (ev)
                                 (unregister-op! transport)
                                 (reject ev)))
    ;; How does {finish,force}-output play with async-io-stream?
    (write-sequence data (socket-stream transport))))
