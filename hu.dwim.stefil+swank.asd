;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.stefil+swank
  :class hu.dwim.system
  :depends-on (:hu.dwim.stefil
               :swank)
  :components ((:module "integration"
                :components ((:file "swank")))))
