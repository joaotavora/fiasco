;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :fiasco
    :description "A Common Lisp test framework that treasures your failures"
  :depends-on (:alexandria)
  :components
  ((:module "src"
    :components
    :serial t
    ((:file "package")
     (:file "utils")
     (:file "infrastructure")
     (:file "asserts")
     (:file "test")
     (:file "suite")))))
