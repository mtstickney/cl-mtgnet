(in-package #:mtgnet-sys)

;;; Protocol for data framing and framing+transmission
(defclass data-framer () ())

(defgeneric frame-data (framer &rest datae)
  (:documentation "Frame one or more data fragments for transmission."))

(defgeneric frame-data* (framer datae)
  (:documentation "Frame multiple data fragments for transmission."))

(defgeneric unframe-data (framer frame)
  (:documentation "Return the data framed by FRAME."))

(defgeneric write-frame (framer transport datae)
  (:documentation "Send multiple data fragments as a single frame over TRANSPORT"))

(defgeneric read-frame (framer transport)
  (:documentation "Read a frame from TRANSPORT."))


(defclass netstring-framer (data-framer)
  ((state :initform (nsp:make-decoder-state) :accessor netstring-state)))

(defmethod frame-data ((framer netstring-framer) &rest datae)
  (nsp:netstring-bytes* datae))

(defmethod frame-data* ((framer netstring-framer) datae)
  (nsp:netstring-bytes datae))

(defmethod unframe-data ((framer netstring-framer) frame)
  (nsp:netstring-data frame))

;; FIXME: this is duplicating pretty much all of NSP's encoding logic.
(defmethod write-frame ((framer netstring-framer) transport datae)
  (blackbird:alet* ((len (reduce #'+ datae :key #'length))
                    (header (trivial-utf-8:string-to-utf-8-bytes (format nil "~X:" len)))
                    (end #.(trivial-utf-8:string-to-utf-8-bytes (string #\Linefeed)))
                    (nil (transport-write transport header))
                    (nil (reduce (lambda (promise chunk)
                                   (blackbird:attach promise
                                                     (lambda (&rest r)
                                                       (declare (ignore r))
                                                       (transport-write transport chunk))))
                                 datae
                                 ;; Trigger the chain with a
                                 ;; non-promise value.
                                 :initial-value nil))
                    (nil (transport-write transport end))
                    (nil (transport-flush transport)))
    ;; All done
    (values)))

(defmethod read-frame ((framer netstring-framer) transport)
  (labels ((completep (state)
             (= (nsp:next-read-size state) 0))
           (process-input (state)
             (if (completep state)
                 (progn
                   ;; Give the framer a fresh state
                   (setf (netstring-state framer) (nsp:make-decoder-state))
                   (nsp::msg-data state))
                 (let ((size (nsp:next-read-size state)))
                   (blackbird:attach (transport-read transport size)
                                     (lambda (seq)
                                       (nsp:pump-vector! seq state)
                                       (process-input state)))))))
    (process-input (netstring-state framer))))
