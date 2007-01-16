;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :stefil
  (:shadow #:log)
  
  (:use :cl :swank :metabang-bind :defclass-star :alexandria :iterate :stefil-system)

  (:export
   #:get-test
   #:rem-test
   #:deftest
   #:is
   #:signals
   #:finishes
   #:in-suite
   #:in-suite*
   #:defsuite

   #:*display-all-slots-in-inspector* ; to also display the raw slots, disabled by default
   #:*test-progress-print-right-margin*
   
   ;; these are the defaults from which the test context is initialized
   #:*print-test-run-progress*
   #:*debug-on-unexpected-error*
   #:*debug-on-assertion-failure*
   ))


