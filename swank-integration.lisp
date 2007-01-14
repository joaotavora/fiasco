;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :sinfol)

(defmethod inspect-for-emacs ((context global-context) inspector)
  (values "Sinfol test results"
          `("Executed tests: " ,(princ-to-string (test-count-of context)) (:newline)
            "Executed assertions: " ,(princ-to-string (assertion-count-of context)) (:newline)
            "Failures: " (:newline)
            ;; intentionally reverse the order by push'ing
            ,@(let ((content))
                (map nil (lambda (description)
                           (push `(:newline) content)
                           (push `(:value ,description) content))
                     (failure-descriptions-of context))
                content)
            ,@(swank::all-slots-for-inspector context inspector))))

(defmethod inspect-for-emacs ((context context) inspector)
  (values "Sinfol test context"
          `("Test: " (:value ,(test-of context)) (:newline)
            "Test arguments: " (:value ,(test-arguments-of context)) (:newline)
            ,@(swank::all-slots-for-inspector context inspector))))

(defmethod inspect-for-emacs ((failure failed-assertion) inspector)
  (values "Failed assertion"
          `("Form: " (:value ,(form-of failure)) (:newline)
            "Test backtrace: " (:newline)
            ,@(iter (for context :in (test-context-backtrace-of failure))
                    (for idx :upfrom 0)
                    (collect (format nil "~D: " idx))
                    (collect `(:value ,context))
                    (collect `(:newline)))
            ,@(swank::all-slots-for-inspector failure inspector))))

(defmethod swank::type-for-emacs ((context global-context))
  nil)
(defmethod swank::type-for-emacs ((context context))
  nil)
(defmethod swank::type-for-emacs ((context failed-assertion))
  nil)



