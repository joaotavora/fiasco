;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.stefil :hu.dwim.stefil.test) 'setup-readtable)
