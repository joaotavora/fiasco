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
  :depends-on (:alexandria
               :anaphora
               :iterate
               :metabang-bind)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "stefil" :depends-on ("duplicates"))
                             (:file "suite" :depends-on ("duplicates"))))))
