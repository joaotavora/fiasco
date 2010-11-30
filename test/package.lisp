;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.stefil.test
  (:use :alexandria
        :common-lisp
        :hu.dwim.stefil)

  (:shadow
   #:deftest
   #:test
   #:fixtures))

(in-package :hu.dwim.stefil)

(import-all-owned-symbols :hu.dwim.stefil :hu.dwim.stefil.test)
