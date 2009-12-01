;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.stefil.test
  :class hu.dwim.test-system
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.stefil)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))))))
