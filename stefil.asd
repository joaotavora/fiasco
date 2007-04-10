;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage #:stefil-system
  (:use :cl :asdf)
  (:export
   #:*load-with-debug-p*))

(in-package #:stefil-system)

(defparameter *load-with-debug-p* nil)

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*features* *features*))
    (when *load-with-debug-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defsystem :stefil
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Stefil - Simple Test Framework In Lisp"
  :depends-on (:swank :alexandria :iterate :metabang-bind :defclass-star)
  :default-component-class local-cl-source-file
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:file "configuration")
   (:file "stefil")
   (:file "suite")
   (:file "swank-integration")))

(defsystem :stefil-test
  :description "Tests for the STEFIL test system."
  :depends-on (:stefil)
  :components
  ((:file "self-tests")))

(defmethod perform ((op test-op) (system (eql (find-system :stefil))))
  (operate 'load-op :stefil-test)
  (in-package :stefil-test)
  (eval (read-from-string "(stefil::enable-sharp-boolean-syntax)"))
  (declaim (optimize (debug 3)))
  (warn "Enabled the #t/#f syntax in the repl thread and set (declaim (optimize (debug 3))) for easy C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'stefil-test:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :stefil))))
  nil)
