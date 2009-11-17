;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.stefil.test
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.stefil
        :iterate
        :metabang-bind)

  (:shadow #:deftest))

(in-package :hu.dwim.stefil)

(import
 '(*suite* count-tests
   parent-of name-of *tests*
   extract-assert-expression-and-message record-failure record-failure*
   assertion-count-of run-tests-of failure-descriptions-of
   *global-context* *context* debug-on-unexpected-error-p
   debug-on-assertion-failure-p print-test-run-progress-p
   rem-test lambda-list-to-variable-name-list
   lambda-list-to-value-list-expression lambda-list-to-funcall-expression
   illegal-lambda-list)
 (find-package :hu.dwim.stefil.test))

