;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage #:sinfol-system
  (:use :cl :asdf)
  (:export
   #:*load-with-debug-p*))

(in-package #:sinfol-system)

(defparameter *load-with-debug-p* nil)

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*features* *features*))
    (when *load-with-debug-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defsystem :sinfol
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD"
  :description "Sinfol Is Not FiveAM or Lift"
  :depends-on (:alexandria :iterate :metabang-bind :defclass-star)
  :default-component-class local-cl-source-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "configuration" :depends-on ("duplicates"))
   (:file "deftest" :depends-on ("configuration"))))

(defsystem :sinfol-test
  :description "Tests for the SINFOL test system."
  :depends-on (:sinfol)
  :components
  ((:file "self-tests")))

(defmethod perform :after ((op load-op) (system (eql (find-system :sinfol-test))))
  ;; globally enable the syntax in the repl thread
  (eval (read-from-string "(sinfol::enable-sharp-boolean-syntax)")))

(defmethod perform ((op test-op) (system (eql (find-system :sinfol))))
  (operate 'load-op :sinfol-test)
  (in-package :sinfol-test)
  ;;(funcall (read-from-string "5am:run!"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :sinfol))))
  nil)
