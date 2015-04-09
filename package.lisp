;;;; package.lisp

(defpackage #:cl-mtgnet
  (:use #:cl)
  (:nicknames #:mtgnet)
  (:export #:define-rpc-method
           #:with-batch-calls
           #:wait
           #:remote-warning
           #:remote-warning-msg
           #:remote-warning-code
           #:remote-error-type
           #:remote-error-msg
           #:remote-error-code))
