;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :sinfol
  (:shadow #:log)
  
  (:use :cl :swank :metabang-bind :defclass-star :alexandria :iterate :sinfol-system)

  (:export
   #:*display-all-slots-in-inspector*
   #:get-test
   #:rem-test
   #:deftest
   #:is
   #:signals
   #:finishes
   #:in-suite
   #:in-suite*
   #:defsuite
   #:*debug-on-unexpected-error*
   #:*debug-on-assertion-failure*
   ))

(defpackage :sinfol-test
  (:use :cl :metabang-bind :defclass-star :alexandria :iterate :sinfol))

