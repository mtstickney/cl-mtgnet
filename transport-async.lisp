(defpackage #:mtgnet.async
  (:use #:cl #:mtgnet-sys)
  (:export #:asynchronous-transport
           #:asynchronous-tcp-transport
           #:wait))

(in-package #:mtgnet.async)

;;; A utility function for waiting on asynchronous promises. Also
;; works on synchronous promises.

;; FIXME: parent type and/or name needs adjusting
(define-condition event-loop-exited (simple-error) ())

(defun wait (promise)
  "Wait for a promise to complete, then return it's value."
  (when (boundp '*rpc-batch*)
    (warn "MTGNET.ASYNC:WAIT called with *RPC-BATCH* bound, this will probably deadlock."))
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

(defclass asynchronous-transport (transport) ())

(defclass asynchronous-tcp-transport (asynchronous-transport)
  ((address :initarg :address :accessor tcp-address)
   (port :initarg :port :accessor tcp-port)
   (stream :initarg :stream :accessor socket-stream)
   (read-callback :initarg :read-cb :accessor read-cb)
   (write-callback :initarg :write-cb :accessor write-cb)
   (error-callback :initarg :err-cb :accessor error-cb)
   (keep-alive :initarg :keep-alive :accessor keep-alive-p)
   (last-error :initform nil :accessor last-error))
  (:default-initargs :stream nil :keep-alive nil)
  (:documentation "Class for transporting data over a TCP socket asynchronously."))

(defun register-op! (transport read write error)
  (check-type read (or function null))
  (check-type write (or function null))
  (check-type error (or function null))
  (setf (read-cb transport) read
             (write-cb transport) write
             (error-cb transport) error)
  (let ((err nil))
    ;; Check and clear the last-error status.
    (rotatef (last-error transport) err)
    (cond
      (err
       ;; Signal the error directly.
       (funcall error err)
       (unregister-op! transport))
      (t
       ;; All is well, register the op.
       (setf (read-cb transport) read
             (write-cb transport) write
             (error-cb transport) error))))
  (values))

(defun set-keepalive (socket delay)
  (check-type delay (integer 0))
  (let ((result (uv:uv-tcp-keepalive socket
                                     1 ;; enable it
                                     delay)))
    (unless (= result 0)
      (error "Error setting keepalive options: ~A." result))
    (values)))

(defun unregister-op! (transport)
  (setf (read-cb transport) nil
        (write-cb transport) nil
        (error-cb transport) (lambda (ev)
                               (setf (last-error transport) ev))))

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
            (unregister-op! transport)))

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
                                        (when (keep-alive-p transport)
                                          (set-keepalive (as:socket-c (as:streamish (socket-stream transport)))
                                                         ;; probe every 2 minutes.
                                                         120))
                                        (resolve socket))
                          :stream t
                          :dont-drain-read-buffer t))))

;; FIXME: do we need to run the error callback for any in-progress operation?
(defmethod transport-disconnect ((transport asynchronous-tcp-transport) &key abort)
  (blackbird:with-promise (resolve reject)
    (when (socket-stream transport)
      (handler-case
          (close (socket-stream transport) :abort t)
        (serious-condition (c)
          (reject c))))
    (resolve)))

(defmethod transport-read ((transport asynchronous-tcp-transport) size)
  (let* ((seq (make-array size :element-type '(unsigned-byte 8))))
    (blackbird:wait (transport-read-into! transport seq)
      seq)))

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
                   (unregister-op! transport)
                   (resolve)))
               (read-available (stream)
                 (let ((bytes (read-sequence seq stream :start read-pos :end end)))
                   (incf read-pos bytes))))
        ;; Try to use already buffered data first
        (resume-read nil (socket-stream transport))
        (unless (= read-pos end)
          ;; If that wasn't enough, set a handler to try again when
          ;; there's more data to read.
          (register-op! transport
                        #'resume-read
                        nil
                        (lambda (ev)
                          (unregister-op! transport)
                          (reject ev))))))))

(defmethod transport-write ((transport asynchronous-tcp-transport) data)
  (blackbird:with-promise (resolve reject)
    (register-op! transport
                  nil
                  (lambda (socket)
                    (declare (ignore socket))
                    ;; Write's complete, unregister the handlers.
                    (unregister-op! transport)
                    (resolve))
                  (lambda (ev)
                    (unregister-op! transport)
                    (reject ev)))
    ;; How does {finish,force}-output play with async-io-stream?
    (write-sequence data (socket-stream transport))))
