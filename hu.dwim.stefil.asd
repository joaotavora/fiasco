;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.stefil
  :class hu.dwim.system
  :description "A Simple Test Framework In Lisp."
  :depends-on (:alexandria)
  :components ((:module "source"
                :components ((:file "asserts" :depends-on ("infrastructure"))
                             (:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "fixture" :depends-on ("test"))
                             (:file "infrastructure" :depends-on ("duplicates"))
                             (:file "test" :depends-on ("infrastructure"))
                             (:file "suite" :depends-on ("infrastructure" "test"))))))

(defmethod perform :after ((o develop-op) (c (eql (find-system :hu.dwim.stefil))))
  (asdf:load-system :hu.dwim.stefil+swank))
