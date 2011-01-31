;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.stefil.documentation
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.stefil
        :hu.dwim.presentation
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.presentation)))
