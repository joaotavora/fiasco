;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :stefil-test
  :licence "BSD / Public domain"
  :depends-on (:stefil)
  :serial t
  :components ((:file "package")
               (:file "basic")
               (:file "fixtures")))
