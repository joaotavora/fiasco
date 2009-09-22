;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

;; from arnesi
(defmacro defprint-object ((self class-name &key (identity t) (type t) with-package)
                           &body body)
  "Define a print-object method using print-unreadable-object.
  An example:
  (defprint-object (self parenscript-dispatcher)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream)
    `(defmethod print-object ((,self ,class-name) ,stream)
      (print-unreadable-object (,self ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)
              ,@(when with-package `((*package* ,(find-package with-package)))))
          ,@body)))))

(defmacro define-dynamic-context
    (name direct-slots &key direct-superclasses
     export-symbols (class-name name) chain-parents
     (create-struct nil) (create-class (not create-struct))
     struct-options
     (defclass-macro-name 'defclass))
  "The purpose of this macro is to provide an easy way to access a group of related special variables. To do so, it generates
   with-NAME/in-NAME/current-NAME/has-NAME macros to access either a CLOS instance or a defstruct in a special variable.
   Optionally it can chain the \"parent\" bindings (use :CHAIN-PARENTS T and access with PARENT-CONTEXT-OF)."
  (assert (and (or create-class
                   create-struct
                   (not (or direct-slots direct-superclasses chain-parents)))
               (or (not create-struct)
                   (not direct-superclasses)))
          () "Invalid combination of DIRECT-SLOTS, DIRECT-SUPERCLASSES, CHAIN-PARENTS and CREATE-CLASS/CREATE-STRUCT.")
  (assert (or (not struct-options) create-struct) () "STRUCT-OPTIONS while no CREATE-STRUCT?")
  (assert (not (and create-class create-struct)) () "Only one of CREATE-CLASS and CREATE-STRUCT is allowed.")
  (flet ((concatenate-symbol (&rest args)
           (let* ((package nil)
                  (symbol-name (string-upcase
                                (with-output-to-string (str)
                                  (dolist (arg args)
                                    (typecase arg
                                      (string (write-string arg str))
                                      (package (setf package arg))
                                      (symbol (unless package
                                                (setf package (symbol-package arg)))
                                              (write-string (symbol-name arg) str))
                                      (integer (write-string (princ-to-string arg) str))
                                      (character (write-char arg) str)
                                      (t (error "Cannot convert argument ~S to symbol" arg))))))))
             (if package
                 (intern symbol-name package)
                 (intern symbol-name))))
         (strcat (&rest string-designators)
           (with-output-to-string (str)
             (dolist (s string-designators)
               (when s (princ s str))))))
    (let ((special-var-name (concatenate-symbol "*" name "*"))
          (extractor-name (concatenate-symbol "current-" name))
          (has-checker-name (concatenate-symbol "has-" name))
          (in-macro-name (concatenate-symbol "in-" name))
          (with-new-macro-name (concatenate-symbol "with-new-" name))
          (with-macro-name (concatenate-symbol "with-" name))
          (ensure-macro-name (concatenate-symbol "ensure-" name))
          (struct-constructor-name (when create-struct
                                     (or (second (assoc :constructor struct-options))
                                         (concatenate-symbol "make-" name))))
          (struct-conc-name (when create-struct
                              (or (second (assoc :conc-name struct-options))
                                  (concatenate-symbol class-name "-")))))
      `(progn
        (defvar ,special-var-name)
        (declaim (inline ,has-checker-name ,extractor-name (setf ,extractor-name)))
        ,(when export-symbols
           `(export (list
                     ',special-var-name
                     ',extractor-name
                     ',has-checker-name
                     ',with-new-macro-name
                     ',with-macro-name
                     ',in-macro-name)))
        ;; generate the context class definition
        ,(when create-class
           `(,defclass-macro-name ,class-name ,direct-superclasses
             ,(if chain-parents
                  (append `((parent-context :initform nil :accessor parent-context-of)) direct-slots) ; accessor is explicitly given to force it to be interned in this package
                  direct-slots)))
        ,(when create-struct
           `(defstruct (,name ,@struct-options)
             ,@(if chain-parents
                   (append `((parent-context nil :type (or null ,class-name))) direct-slots)
                   direct-slots)))
        ;; generate the with-new-... macro
        (defmacro ,with-new-macro-name ((&rest initargs) &body forms)
          `(,',with-macro-name ,,(if create-struct
                                   ``(,',struct-constructor-name ,@initargs)
                                   ``(make-instance ',',class-name ,@initargs))
            ,@forms))
        ;; generate the ensure-... macro
        (defmacro ,ensure-macro-name ((&rest initargs) &body forms)
          `(,',with-macro-name (or (and (boundp ',',special-var-name)
                                        ,',special-var-name)
                                   ,,(if create-struct
                                         ``(,',struct-constructor-name ,@initargs)
                                         ``(make-instance ',',class-name ,@initargs)))
            ,@forms))
        ;; generate the with-... macro
        (defmacro ,with-macro-name (context &body forms)
          (let ((context-instance (gensym "CONTEXT-INSTANCE"))
                (parent (gensym "PARENT")))
            (declare (ignorable parent))
            `(let* ((,context-instance ,context)
                    ,@,(when chain-parents
                             ``((,parent (when (,',has-checker-name)
                                           (,',extractor-name)))))
                    (,',special-var-name ,context-instance))
              (declare (special ,',special-var-name)) ; KLUDGE with-call/cc in arnesi needs it currently
              ,@,(when chain-parents
                   ``((setf (,',(if create-struct
                                    (concatenate-symbol struct-conc-name "parent-context")
                                    'parent-context-of) ,context-instance)
                       ,parent)))
              (unless ,context-instance
                (error ,',(strcat "Called with nil " (string-downcase name))))
              ,@forms)))

        (defun ,has-checker-name ()
          (boundp ',special-var-name))

        ;; generate the current-... function
        (defun ,extractor-name ()
          ,special-var-name)
        (defun (setf ,extractor-name) (value)
          (setf ,special-var-name value))))))

#+nil
(defmacro define-dynamic-context* (name direct-slots &rest args
                                   &key (defclass-macro-name 'defclass*)
                                   &allow-other-keys)
  (remove-from-plistf args :defclass-macro-name)
  `(define-dynamic-context ,name ,direct-slots
     :defclass-macro-name ,defclass-macro-name
     ,@args))
