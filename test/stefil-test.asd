;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(asdf:defsystem :stefil-self-tests
    :licence "BSD / Public domain"
  :depends-on (:stefil)
  :serial t
  :components ((:file "package")
               (:file "basic")
               (:file "fixtures")
               (:file "intro-example")))
