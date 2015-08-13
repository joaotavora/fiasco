;;; -*- mode: Lisp -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :fiasco)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defmacro defprint-object (&whole whole (self class-name &key (identity t) (type t) with-package (muffle-errors t))
                           &body body)
  "Define a print-object method using print-unreadable-object.
  An example:
  (defprint-object (self parenscript-dispatcher)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (multiple-value-bind (body declarations documentation) (parse-body body :documentation t :whole whole)
      `(defmethod print-object ((,self ,class-name) ,stream)
         ,@(when documentation
             (list documentation))
         ,@declarations
         (print-unreadable-object (,self ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream))
             (block ,printing
               (,@(if muffle-errors
                      `(handler-bind ((error (lambda (error)
                                               (declare (ignore error))
                                               (write-string "<<error printing object>>")
                                               (return-from ,printing)))))
                      `(progn))
                  (let (,@(when with-package `((*package* ,(find-package with-package)))))
                    ,@body)))))
         ;; primary PRINT-OBJECT methods are supposed to return the object
         ,self))))

(defmacro define-dynamic-context
    (name direct-slots &key direct-superclasses
     export-symbols (class-name name) chain-parents
     (create-struct nil) (create-class (not create-struct))
     struct-options
     (defclass-macro-name 'defclass))
  "Defined a dynamic context named NAME.

A dynamic context is both a class/struct definition and a lot accessor
macros.

Generates with-NAME/in-NAME/current-NAME/has-NAME macros to access
either a CLOS instance or a defstruct in a special variable.

Optionally it can chain the \"parent\" bindings (use :CHAIN-PARENTS T
and access with PARENT-CONTEXT-OF)."
  (assert (and (or create-class
                   create-struct
                   (not (or direct-slots direct-superclasses chain-parents)))
               (or (not create-struct)
                   (not direct-superclasses)))
          () "Invalid combination of DIRECT-SLOTS, DIRECT-SUPERCLASSES, CHAIN-PARENTS and CREATE-CLASS/CREATE-STRUCT.")
  (assert (or (not struct-options) create-struct) () "STRUCT-OPTIONS while no CREATE-STRUCT?")
  (assert (not (and create-class create-struct)) () "Only one of CREATE-CLASS and CREATE-STRUCT is allowed.")
  (flet ((strcat (&rest string-designators)
           (with-output-to-string (str)
             (dolist (s string-designators)
               (when s (princ s str))))))
    (let ((special-var-name (alexandria:symbolicate '#:* name '#:*))
          (extractor-name (alexandria:symbolicate '#:current- name))
          (has-checker-name (alexandria:symbolicate '#:has- name))
          (in-macro-name (alexandria:symbolicate '#:in- name))
          (with-new-macro-name (alexandria:symbolicate '#:with-new- name))
          (with-macro-name (alexandria:symbolicate '#:with- name))
          (ensure-macro-name (alexandria:symbolicate '#:ensure- name))
          (struct-constructor-name (when create-struct
                                     (or (second (assoc :constructor struct-options))
                                         (alexandria:symbolicate '#:make- name))))
          (struct-conc-name (when create-struct
                              (or (second (assoc :conc-name struct-options))
                                  (alexandria:symbolicate class-name '#:-)))))
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
                                    (alexandria:symbolicate struct-conc-name '#:parent-context)
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

;; from hu.dwim.common
(defun import-all-owned-symbols (source-package target-package &key (overwrite nil))
  (setf source-package (find-package source-package))
  (setf target-package (find-package target-package))
  (let ((count 0))
    (do-symbols (symbol source-package)
      (let ((target-symbol-with-same-name (find-symbol (symbol-name symbol) target-package)))
        (when (and (eq (symbol-package symbol) source-package)
                   (or overwrite
                       (not target-symbol-with-same-name)))
          (when (and target-symbol-with-same-name
                     (not (eq symbol target-symbol-with-same-name))
                     overwrite)
            (unintern target-symbol-with-same-name target-package))
          (import symbol target-package)
          (incf count))))
    count))
