;;;; package.lisp

(defpackage #:cl-rpc
  (:use #:cl)
  (:export #:define-rpc-method
           #:with-batch-calls
           #:wait))
