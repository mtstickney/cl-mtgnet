(in-package #:cl-mtgnet)

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
         (destructuring-bind (name &key optional &allow-other-keys)
             s
           (declare (ignore name))
           optional)))

  (defun slot-symbol (s)
    (etypecase s
      (symbol s)
      (list (destructuring-bind (name &rest r) s
              (declare (ignore r))
              name))))

  (defun cat-symbol (symbol &rest rest)
  (intern (format nil "~{~A~}" (mapcar #'symbol-name
                                       (cons symbol rest)))))

  (defun slot-optional-p (symbol optional-slots type)
    (check-type symbol symbol)
    (check-type optional-slots list)
    (check-type type keyword)
    (let ((cell (assoc symbol optional-slots)))
      (and cell
           (ecase type
             (:write
              (or (eq (cdr cell) :write)
                  (eq (cdr cell) :read-write)))
             (:read
              (or (eq (cdr cell) :read)
                  (eq (cdr cell) :read-write)))
             (:read-write (eq (cdr cell) :read-write)))))))

;; TODO: See if it's possible to generate docstrings for these things
(defmacro define-json-obj (name &body body)
  (let* ((slots (if (stringp (first body))
                    (cdr body)
                    body))
         (serialized-slots (mapcar #'slot-symbol
                                   (remove-if-not #'serial-slot-p slots)))
         (optional-slots (remove-if #'null (mapcar (lambda (s)
                                                     (let ((optional (optional-slot-p s)))
                                                       (if optional
                                                           (cons (slot-symbol s) optional)
                                                           nil)))
                                                   slots)))
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
                collect `(let ((cell (assoc (json-key ',s) json-obj)))
                           ,@(unless (slot-optional-type s :read)
                                     `((unless cell
                                         (error 'invalid-json-obj :type ',name :json json-obj))))
                           (when cell
                             (setf (getf arglist ,(intern (symbol-name s) 'keyword))
                                   (cdr cell)))))
           (apply #',make-func arglist)))
       (defun ,unmarshall-func (json-string)
         (let ((json-obj (json:decode-json-from-string json-string)))
           (,build-func json-obj)))
       (defun ,marshall-func (obj)
         (json:with-object ()
           ,@(flet ((accessor (field) (cat-symbol name '#:- field)))
                   (loop for s in serialized-slots
                      collect (let ((encode-form `(encode-field ',s (,(accessor s) obj))))
                                (if (slot-optional-type s :write)
                                    `(when (,(accessor s) obj)
                                       ,encode-form)
                                    encode-form))))))
       (defun ,make-func (&rest initargs)
         ;; Explicitly defined options take precedence, otherwise use :default-initargs
         (let ((initargs (concatenate 'list initargs ,(cdr default-initargs))))
               (apply #',ctor initargs))))))

(define-json-obj rpc-call
  "A single method invocation."
  (service :initial "" :type string :read-only t)
  (method :initial "" :type string :read-only t)
  (args :initial '() :type list :read-only t)
  (id :initial nil :optional :read :read-only t))

(define-json-obj rpc-result
  "The result of an RPC method invocation."
  (data :initial nil :optional :read-write :read-only t)
  (error :initial nil :optional :read-write :read-only t)
  (warnings :initial '() :optional :read-write :read-only t)
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
