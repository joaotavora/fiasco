;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.stefil
  (:use :alexandria
        :common-lisp)

  (:export #:find-test
           #:deftest
           #:is
           #:signals
           #:not-signals
           #:finishes
           #:with-expected-failures
           #:with-expected-failures*
           #:with-captured-lexical-environment
           #:in-suite
           #:root-suite
           #:defsuite
           #:defsuite*
           #:run-child-tests ; TODO delete eventually
           #:-run-child-tests-
           #:defixture
           #:-body-
           #:-here-
           #:with-fixture
           #:with-fixtures
           #:runs-without-failure?
           #:without-debugging
           #:without-test-progress-printing
           #:funcall-test-with-feedback-message
           #:run-failed-tests
           #:extract-test-run-statistics

           #:*display-all-slots-in-inspector* ; to also display the raw slots, disabled by default
           #:*test-progress-print-right-margin*
           #:*test-result-history*
           #:*last-test-result*

           ;; these are the defaults from which the test context is initialized
           #:*print-test-run-progress*
           #:*debug-on-unexpected-error*
           #:*debug-on-assertion-failure*))
