;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(defun stefil-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((test (find-test form :otherwise nil)))
      (when test
        (values test t)))))

(pushnew 'stefil-inspector-lookup-hook swank:*inspector-dwim-lookup-hooks*)

(defvar *display-all-slots-in-inspector* #f)

(defmethod inspect-for-emacs ((global-context global-context) inspector)
  (values "Stefil test results"
          `("Executed tests: " (:value ,(hash-table-values (run-tests-of global-context))
                                       ,(princ-to-string (hash-table-count (run-tests-of global-context)))) (:newline)
            "Executed assertions: " ,(princ-to-string (assertion-count-of global-context)) (:newline)
            "Failures: " ,@(unless (emptyp (failure-descriptions-of global-context))
                             `((:action "[rerun all failed tests]"
                                ,(lambda ()
                                         (swank::inspect-object (run-failed-tests global-context) inspector)))))
            (:newline)
            ;; intentionally reverse the order by push'ing
            ,@(iter (for description :in-vector (failure-descriptions-of global-context))
                    (for context = (first (test-context-backtrace-of description)))
                    (collect `(:action "[rerun]" ,(rerun-action-for-inspector context inspector)))
                    (collect " ")
                    (collect `(:value ,description))
                    (collect `(:newline)))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector global-context inspector)))))

(defmethod inspect-for-emacs ((context context) inspector)
  (values "Stefil test context"
          `("Test: " (:value ,(test-of context)) (:newline)
            "Test arguments: " (:value ,(test-arguments-of context)) (:newline)
            "Real time spent in body: " (:value ,(real-time-spent-in-seconds context)) " sec" (:newline)
            "Parent test frames:" (:newline)
            ,@(iter (for parent-context :first (parent-context-of context) :then (parent-context-of parent-context))
                    (while parent-context)
                    (collect `(:value ,parent-context))
                    (collect `(:newline)))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector context inspector)))))

(defun rerun-action-for-inspector (context inspector)
  (lambda ()
    (apply (name-of (test-of context))
           (test-arguments-of context))
    (swank::inspect-object *last-test-result* inspector)))

(defun test-backtrace-for-emacs (description inspector)
  (iter (for context :in (test-context-backtrace-of description))
        (for idx :upfrom 0)
        (collect (format nil "~D: " idx))
        (collect `(:action "[rerun]" ,(rerun-action-for-inspector context inspector)))
        (collect " ")
        (collect `(:value ,context))
        (collect `(:newline))))

(defmethod inspect-for-emacs ((failure failed-assertion) inspector)
  (values "Failed Stefil assertion"
          `("Form: " (:value ,(form-of failure)) (:newline)
            "Test backtrace: " (:newline)
            ,@(test-backtrace-for-emacs failure inspector)
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector failure inspector)))))

(defmethod inspect-for-emacs ((description unexpected-error) inspector)
  (values "Unexpected error in a Stefil test"
          `("Condition: " (:value ,(condition-of description)) (:newline)
            "Test backtrace: " (:newline)
            ,@(test-backtrace-for-emacs description inspector)
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector description inspector)))))

(defmethod inspect-for-emacs ((test test) inspector)
  (values "Stefil test"
          `("Name: " (:value ,(name-of test)) ,@(when (find-test (name-of test) :otherwise #f)
                                                  `(" " (:action "[undefine]" ,(lambda () (rem-test (name-of test)))))) (:newline)
            "Package: " (:value ,(package-of test)) (:newline)
            "Compile before run?: " ,(if (compile-before-run-p test) "yes" "no") (:newline)
            "Auto call by its suite?: " ,(if (auto-call-p test) "yes" "no") (:newline)
            "Documentation: " ,@(when (documentation-of test) `((:value ,(documentation-of test)))) (:newline)
            "Parent: " (:value ,(parent-of test)) (:newline)
            "Children: " (:newline)
            ,@(iter (for (nil child) :in-hashtable (children-of test))
                    (appending `((:value ,child) " "
                                 (:action "[undefine]" ,(lambda () (rem-test (name-of child))))
                                 (:newline))))
            ,@(when *display-all-slots-in-inspector*
                (swank::all-slots-for-inspector test inspector)))))

;; this will disable the header in the inspector, but vanilla Slime dies with nil instead of a proper type
#+branched-slime
(macrolet ((no-type-for (&rest args)
             `(progn
               ,@(iter (for type :in args)
                       (collect `(defmethod swank::type-for-emacs ((thing ,type))
                                  nil))))))
  (no-type-for global-context context failed-assertion unexpected-error test))



