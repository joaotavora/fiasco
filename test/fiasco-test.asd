;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(asdf:defsystem :fiasco-self-tests
    :licence "BSD / Public domain"
  :depends-on (:fiasco)
  :serial t
  :components ((:file "package")
               (:file "basic")
               (:file "fixtures")
               (:file "intro-example")))
