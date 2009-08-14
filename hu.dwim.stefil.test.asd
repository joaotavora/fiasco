;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.stefil.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :description "Test suite for hu.dwim.stefil"
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.stefil)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))))))
