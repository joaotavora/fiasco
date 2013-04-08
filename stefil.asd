;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :stefil
  :description "A Simple Test Framework In Lisp."
  :depends-on (:alexandria)
  :components
  ((:module "source"
    :components
    ((:file "asserts" :depends-on ("infrastructure"))
     (:file "package")
     (:file "duplicates" :depends-on ("package"))
     (:file "fixture" :depends-on ("test"))
     (:file "infrastructure" :depends-on ("duplicates"))
     (:file "test" :depends-on ("infrastructure"))
     (:file "suite" :depends-on ("infrastructure" "test"))))))
