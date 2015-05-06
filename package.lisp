;;;; package.lisp

(defpackage #:mtgnet-sys
  (:use #:cl)
  ;; Basic API
  (:export #:*default-encoder*
           #:*default-connection-class*
           #:define-rpc-method
           #:with-batch-calls
           #:wait
           #:remote-warning
           #:remote-warning-msg
           #:remote-warning-code
           #:remote-error-type
           #:remote-error-msg
           #:remote-error-code
           #:invalid-json-obj)
  ;; Extension points
  (:export #:rpc-connection
           #:connect
           #:disconnect
           #:data-input-stream
           #:data-output-stream
           #:read-response
           #:send-request
           #:add-result)
  ;;; Things that extensions might use.
  ;; Standard objects.
  (:export #:rpc-call-p
           #:rpc-call-service
           #:rpc-call-id
           #:rpc-call-method
           #:rpc-call-args
           #:build-rpc-call
           #:make-rpc-call
           #:marshall-rpc-call
           #:unmarshall-rpc-call

           #:rpc-error-p
           #:rpc-error-message
           #:rpc-error-code
           #:rpc-error-data
           #:build-rpc-error
           #:make-rpc-error
           #:marshall-rpc-error
           #:unmarshall-rpc-error

           #:rpc-result
           #:rpc-result-p
           #:rpc-result-data
           #:rpc-result-error
           #:rpc-result-warnings
           #:rpc-result-id
           #:build-rpc-result
           #:make-rpc-result
           #:marshall-rpc-result
           #:unmarshall-rpc-result

           #:rpc-request
           #:rpc-request-p
           #:build-rpc-request
           #:marshall-rpc-request
           #:unmarshall-rpc-request

           #:rpc-response
           #:rpc-response-p
           #:build-rpc-response
           #:marshall-rpc-response
           #:unmarshall-rpc-response

           #:rpc-warning-list
           #:rpc-warning-list-p
           #:build-rpc-warning-list
           #:marshall-rpc-warning-list
           #:unmarshall-rpc-warning-list)
  ;; Utility functions
  (:export #:make-result-future
           #:all-futures
           #:all-futures*
           #:rpc-call-future
           #:make-call-obj
           #:invoke-rpc-method
           #:with-batch-calls
           #:connection-address
           #:connection-port
           #:socket))

(defpackage #:cl-mtgnet
  (:use #:cl)
  (:nicknames #:mtgnet)
  (:import-from #:mtgnet-sys
                #:connect
                #:disconnect
                #:*default-encoder*
                #:*default-connection-class*
                #:define-rpc-method
                #:with-batch-calls
                #:wait
                #:remote-warning
                #:remote-warning-msg
                #:remote-warning-code
                #:remote-error-type
                #:remote-error-msg
                #:remote-error-code
                #:invalid-json-obj)
  (:export #:connect
           #:disconnect
           #:*default-encoder
           #:*default-connection-class*
           #:define-rpc-method
           #:with-batch-calls
           #:wait
           #:remote-warning
           #:remote-warning-msg
           #:remote-warning-code
           #:remote-error-type
           #:remote-error-msg
           #:remote-error-code
           #:invalid-json-obj))
