(in-package #:mtgnet-sys)

(defun encode-field (field value &optional (encoder #'json:encode-json))
  "Encode VALUE as an object field named FIELD, using ENCODER to
encode VALUE."
  (json:as-object-member (field)
    (funcall encoder value)))

(defun json-key (symb)
  "Return the symbol produced by JSON encoding and decoding SYMB."
  (funcall json::*identifier-name-to-key*
           (json:decode-json-from-string
            (funcall json::*json-identifier-name-to-lisp*
                     (json:encode-json-to-string symb)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun serial-slot-p (s)
    (or (symbolp s)
        (and (listp s)
             (destructuring-bind (name &key (marshall t) &allow-other-keys)
                 s
               (declare (ignore name))
               marshall))))

  (defun optional-slot-p (s)
    (and (listp s)
         (getf (rest s) :optional)))

  (defun slot-symbol (s)
    (etypecase s
      (symbol s)
      (list (destructuring-bind (name &rest r) s
              (declare (ignore r))
              name))))

  (defun slot-marshall-type (s)
    (etypecase s
      (symbol nil)
      (list (getf (rest s) :marshall-type))))

  (defun cat-symbol (symbol &rest rest)
  (intern (format nil "~{~A~}" (mapcar #'symbol-name
                                       (cons symbol rest)))))

  (defun slot-optional-p (slot type)
    (check-type type symbol)
    (let ((optional-type (optional-slot-p slot)))
      (ecase type
        (:write (ecase optional-type
                  ((:write :read-write) t)
                  ((:read nil) nil)))
        (:read (ecase optional-type
                 ((:read :read-write) t)
                 ((:write nil) nil)))
        ((nil) (ecase optional-type
                 ((nil) t)
                 ((:read :write :read-write) nil)))))))

(define-condition invalid-json-obj (error)
  ((type :initarg :type :accessor obj-type)
   (json :initarg :json :accessor obj-json))
  (:report (lambda (c s)
             (format s "Invalid json data for object of type ~S: ~S"
                     (obj-type c)
                     (obj-json c)))))

;; TODO: handle packages on e.g. type symbols appropriately (I'm
;; looking at you, :marshall-type)
;; TODO: See if it's possible to generate docstrings for these things
(defmacro define-json-obj (name &body body)
  (let* ((slots (if (stringp (first body))
                    (cdr body)
                    body))
         (serialized-slots (remove-if-not #'serial-slot-p slots))
         (make-func (cat-symbol '#:make- name))
         (build-func (cat-symbol '#:build- name))
         (unmarshall-func (cat-symbol '#:unmarshall- name))
         (marshall-func (cat-symbol '#:marshall- name)))
    `(progn
       (defstruct ,name
         ;; Optional docstring
         ,@(if (stringp (first body))
               (list (first body))
               nil)
         ;; Slot list
         ,@(loop for slot in slots
              collect
                (etypecase slot
                  (symbol slot)
                  (list
                   (destructuring-bind (name &rest keyword-pairs &key initial
                                             &allow-other-keys)
                       slot
                     `(,name ,initial :allow-other-keys t ,@keyword-pairs))))))
       ;; Produce a structure from a decoded json object
       (defun ,build-func (json-obj)
         (let ((arglist '()))
           ,@(loop for s in serialized-slots
                collect `(let ((cell (assoc (json-key ',(slot-symbol s)) json-obj)))
                           ,@(unless (slot-optional-p s :read)
                                     `((unless cell
                                         (error 'invalid-json-obj :type ',name :json json-obj))))
                           (when cell
                             (setf (getf arglist ,(intern (symbol-name (slot-symbol s)) 'keyword))
                                   ,(if (slot-marshall-type s)
                                        (progn
                                          `(,(cat-symbol '#:build- (slot-marshall-type s)) (cdr cell)))
                                        `(cdr cell))))))
           (apply #',make-func arglist)))
       (defun ,unmarshall-func (json-string)
         (let ((json-obj (json:decode-json-from-string json-string)))
           (,build-func json-obj)))
       (defun ,marshall-func (obj)
         (json:with-object ()
           ,@(flet ((accessor (field) (cat-symbol name '#:- field)))
                   (loop for s in serialized-slots
                      collect (let ((encode-form `(encode-field ',(slot-symbol s)
                                                                (,(accessor (slot-symbol s)) obj)
                                                                ,@(if (slot-marshall-type s)
                                                                      (list `#',(cat-symbol '#:marshall-
                                                                                            (slot-marshall-type s)))
                                                                      '()))))
                                (if (slot-optional-p s :write)
                                    `(when (,(accessor (slot-symbol s)) obj)
                                       ,encode-form)
                                    encode-form)))))))))

(define-json-obj rpc-call
  "A single method invocation."
  (service :initial "" :type string :read-only t)
  (method :initial "" :type string :read-only t)
  (args :initial '() :type list :read-only t)
  (id :initial nil :optional :read :read-only t))

(define-json-obj rpc-error
  "Error object contained in a failed rpc-result."
  (message :initial "" :type string :read-only t)
  (code :initial 0 :type integer :read-only t)
  (data :initial nil :read-only t :optional :read-write))

(define-json-obj rpc-result
  "The result of an RPC method invocation."
  (data :initial nil :optional :read-write :read-only t)
  (error :initial nil :optional :read-write :read-only t :marshall-type rpc-error)
  (warnings :initial '() :optional :read-write :read-only t :marshall-type rpc-warning-list)
  (id :intitial nil :optional :read-write :read-only t))

(defmacro define-object-array (name type)
  (let* ((predicate-name (cat-symbol name '#:-p))
         (obj-predicate-name (cat-symbol type '#:-p)))
    `(progn
       (defun ,predicate-name (thing)
         (and (listp thing)
              (every #',obj-predicate-name thing)))
       (deftype ,name () '(satisfies ,predicate-name))
       (defun ,(cat-symbol '#:marshall- name) (obj)
         (check-type obj ,name)
         (json:with-array ()
           (mapc #',(cat-symbol '#:marshall- type) obj)))
       (defun ,(cat-symbol '#:build- name) (obj)
         (check-type obj list)
         (mapcar #',(cat-symbol '#:build- type) obj))
       (defun ,(cat-symbol '#:unmarshall- name) (str)
         (check-type str string)
         (let ((obj (json:decode-json-from-string str)))
           (,(cat-symbol '#:build- name) obj))))))

(define-object-array rpc-request rpc-call)

(define-object-array rpc-response rpc-result)

(define-object-array rpc-warning-list rpc-error)
