;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :sinfol
  (:shadow #:log)
  
  (:use :cl :metabang-bind :defclass-star :alexandria :iterate :sinfol-system)

  (:export
   #:deftest
   #:is
   #:signals
   #:finishes)

  ;; stuff exported only for debug purposes
  (:export))

(defpackage :sinfol-test
  (:use :cl :sinfol :alexandria :iterate))

