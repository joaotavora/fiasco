;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(defun stefil-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((test (get-test form :otherwise nil)))
      (when test
        (values test t)))))

(pushnew 'stefil-inspector-lookup-hook swank:*inspector-dwim-lookup-hooks*)

(defvar *display-all-slots-in-inspector* #f)

(defmethod inspect-for-emacs ((context global-context) inspector)
  (values "Stefil test results"
          `("Executed tests: " (:value ,(hash-table-values (run-tests-of context))
                                       ,(princ-to-string (hash-table-count (run-tests-of context)))) (:newline)
            "Executed assertions: " ,(princ-to-string (assertion-count-of context)) (:newline)
            "Failures: " (:newline)
            ;; intentionally reverse the order by push'ing
            ,@(iter (for description :in-vector (failure-descriptions-of context))
                    (collect `(:value ,description))
                    (collect `(:newline)))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector context inspector)))))

(defmethod inspect-for-emacs ((context context) inspector)
  (values "Stefil test context"
          `("Test: " (:value ,(test-of context)) (:newline)
            "Test arguments: " (:value ,(test-arguments-of context)) (:newline)
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector context inspector)))))

(defmethod inspect-for-emacs ((failure failed-assertion) inspector)
  (values "Failed Stefil assertion"
          `("Form: " (:value ,(form-of failure)) (:newline)
            "Test backtrace: " (:newline)
            ,@(iter (for context :in (test-context-backtrace-of failure))
                    (for idx :upfrom 0)
                    (collect (format nil "~D: " idx))
                    (collect `(:value ,context))
                    (collect `(:newline)))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector failure inspector)))))

(defmethod inspect-for-emacs ((test test) inspector)
  (values "Stefil test"
          `("Name: " (:value ,(name-of test)) ,@(when (get-test (name-of test) :otherwise #f)
                                                  `(" " (:action "[undefine]" ,(lambda () (rem-test (name-of test)))))) (:newline)
            "Package: " (:value ,(package-of test)) (:newline)
            "Compile before run?: " ,(if (compile-before-run-p test) "yes" "no") (:newline)
            "Documentation: " ,@(when (documentation-of test) `((:value ,(documentation-of test)))) (:newline)
            "Parent: " (:value ,(parent-of test)) (:newline)
            "Children: " (:newline)
            ,@(iter (for (nil child) :in-hashtable (children-of test))
                    (appending `((:value ,child) " "
                                 (:action "[undefine]" ,(lambda () (rem-test (name-of child))))
                                 (:newline))))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector test inspector)))))

(macrolet ((no-type-for (&rest args)
             `(progn
               ,@(iter (for type :in args)
                       (collect `(defmethod swank::type-for-emacs ((thing ,type))
                                  nil))))))
  (no-type-for global-context context failed-assertion test))



