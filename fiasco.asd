;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :fiasco
  :description "A Common Lisp test framework that treasures your failures"
  :depends-on (:alexandria)
  :components
  ((:module "source"
    :components
    ((:file "asserts" :depends-on ("infrastructure"))
     (:file "package")
     (:file "duplicates" :depends-on ("package"))
     (:file "infrastructure" :depends-on ("duplicates"))
     (:file "test" :depends-on ("infrastructure"))
     (:file "suite" :depends-on ("infrastructure" "test"))))))
