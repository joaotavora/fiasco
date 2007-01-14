;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :sinfol)

#.(file-header)

(eval-always
  (assert (eq *defclass-macro-name-for-dynamic-context* 'defclass*)))

(define-dynamic-context global-context
  ((failure-descriptions '())
   (assertion-count 0)
   (test-count 0)
   (test-lambdas (make-hash-table) :documentation "test -> compiled test lambda mapping for this test run")))

(defprint-object (self global-context :identity #f :type #f)
  (format t "results :tests ~A :assertions ~A :failures ~A"
          (test-count-of self) (assertion-count-of self) (length (failure-descriptions-of self))))

(defgeneric get-test-lambda (test global-context)
  (:method ((test test) (context global-context))
           (bind (((values test-lambda found-p) (gethash test (test-lambdas-of context))))
             (unless found-p
               (setf test-lambda (bind ((*package* (package-of test))
                                        (*readtable* (copy-readtable)))
                                   (compile nil `(lambda ,(lambda-list-of test)
                                                  ,@(body-of test)))))
               (setf (gethash test (test-lambdas-of context)) test-lambda))
             test-lambda)))

(define-dynamic-context context
  ((test)
   (test-arguments)
   (number-of-added-failure-descriptions 0))
  :chain-parents #t)

(defvar *debug-on-unexpected-error* #t)
(defvar *debug-on-assertion-failure* #t)

(defmacro deftest (&whole whole name args &body body)
  (bind (((values remaining-forms declarations documentation) (parse-body body :documentation #t :whole whole))
         ((name &rest test-args &key (compile-before-run #t) &allow-other-keys) (ensure-list name)))
    (with-unique-names (toplevel-p test test-lambda global-context result-values)
      `(progn
        (load-time-value
          (make-test ',name
           :package ,*package*
           :lambda-list ',args
           :compile-before-run ,compile-before-run
           :declarations ',declarations
           :documentation ',documentation
           :body ',remaining-forms
           ,@test-args))
        (handler-bind ((style-warning #'muffle-warning)) ; to get rid of the redefining defun warning
          (defun ,name ,args
            ,@(when documentation (list documentation))
            ,@declarations
            (declare (optimize (debug 3)))
            (bind ((,test (get-test ',name))
                   (,toplevel-p (not (has-global-context)))
                   (,global-context (unless ,toplevel-p
                                      (current-global-context)))
                   (,result-values '()))
              (assert ,test)
              (flet ((body ()
                       (with-new-context (:test ,test :test-arguments ,(lambda-list-to-value-list args))
                         (incf (test-count-of ,global-context))
                         (setf ,result-values
                               (multiple-value-list
                                   (labels ((prune-failure-descriptions ()
                                              ;; drop failures recorded by the previous run of this test
                                              (bind ((context (current-context)))
                                                (dotimes (i (number-of-added-failure-descriptions-of context))
                                                  (pop (failure-descriptions-of ,global-context)))
                                                (setf (number-of-added-failure-descriptions-of context) 0)))
                                            (run-it ()
                                              (handler-bind ((assertion-failed (lambda (c)
                                                                                 (declare (ignore c))
                                                                                 (unless *debug-on-assertion-failure*
                                                                                   (invoke-restart
                                                                                    (find-restart 'continue-testing)))))
                                                             (serious-condition (lambda (c)
                                                                                  (unless (or *debug-on-unexpected-error*
                                                                                              (typep c 'assertion-failed))
                                                                                    (record-failure* 'unexpected-error
                                                                                                     :description-initargs (list :condition c)
                                                                                                     :signal-assertion-failed #f)
                                                                                    (return-from run-it)))))
                                                (restart-case (bind ((*package* (package-of ,test))
                                                                     (*readtable* (copy-readtable)))
                                                                ,(if compile-before-run
                                                                     `(bind ((,test-lambda (get-test-lambda ,test ,global-context)))
                                                                       (funcall ,test-lambda ,@(lambda-list-to-funcall-list args)))
                                                                     `(progn
                                                                       ,@remaining-forms)))
                                                  (retest ()
                                                    :report (lambda (stream)
                                                              (format stream "~@<Rerun the test ~S~@:>" ',name))
                                                    (prune-failure-descriptions)
                                                    (return-from run-it (run-it)))
                                                  (retest-without-debugging ()
                                                    :report (lambda (stream)
                                                              (format stream "~@<Turn off debugging for the rest of this test run and rerun the current test ~S~@:>" ',name))
                                                    (setf *debug-on-unexpected-error* #f)
                                                    (setf *debug-on-assertion-failure* #f)
                                                    (prune-failure-descriptions)
                                                    (return-from run-it (run-it)))
                                                  (skip-test ()
                                                    :report (lambda (stream)
                                                              (format stream "~@<Abort the test ~S and record that it was skipped~@:>" ',name))
                                                    (record-failure* 'skipped-by-restart :signal-assertion-failed #f)
                                                    (values))))))
                                     (run-it)))))))
                (if ,toplevel-p
                    (with-new-global-context ()
                      (setf ,global-context (current-global-context))
                      (rebind (*debug-on-unexpected-error*
                               *debug-on-assertion-failure*)
                        (body)))
                    (body))
                (if ,toplevel-p
                    (if ,result-values
                        (values ,global-context ,result-values)
                        ,global-context)
                    (values-list ,result-values))))))))))
    

(defcondition* assertion-failed (test-related-condition simple-error)
  ())

(defun record-failure (description-type &rest args)
  (record-failure* description-type :description-initargs args))

(defun record-failure* (type &key (signal-assertion-failed #t) description-initargs)
  (in-global-context (failure-descriptions)
    (in-context context
      (bind ((description (apply #'make-instance type
                                 :test-context-backtrace (iter (for context :first (current-context) :then (parent-context-of context))
                                                               (while context)
                                                               (collect context))
                                 description-initargs)))

        (when signal-assertion-failed
          (with-simple-restart
              (continue-testing "Record the failure and continue with the pending tests")
            (error 'assertion-failed
                   :test (test-of context)
                   :format-control (etypecase description
                                     (failed-assertion "Assertion ~S failed."))
                   :format-arguments (typecase description
                                       (failed-assertion (list (form-of description)))))))
        (push description failure-descriptions)
        (incf (number-of-added-failure-descriptions-of context))))))

(defmacro is (&whole whole &rest args)
  (bind ((((predicate expected actual)) args))
    (with-unique-names (global-context)
      `(in-global-context ,global-context
        (incf (assertion-count-of ,global-context))
        (unless (,predicate ,expected ,actual)
          (record-failure 'failed-assertion :form ',whole))
        (values)))))

(defmacro signals (what &body body)
  (bind ((condition-type what))
    (with-unique-names (test-block global-context)
      `(in-global-context ,global-context
        (incf (assertion-count-of ,global-context))
        (block ,test-block
          (handler-bind ((,condition-type (lambda (c)
                                            (declare (ignore c))
                                            (return-from ,test-block (values)))))
            (multiple-value-prog1
                (progn ,@body)
              (record-failure 'missing-condition :condition ',condition-type))))))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; some utils

(defun lambda-list-to-funcall-list (args)
  (iter (with in-keywords = #f)
        (for cell :first args :then (cdr cell))
        (while cell)
        (for arg = (first (ensure-list (car cell))))
        (case arg
          (&key (setf in-keywords #t))
          (&allow-other-keys)
          (&rest (setf cell (cdr cell)))
          (t (if in-keywords
                 (progn
                   (collect (intern (symbol-name (first (ensure-list arg)))
                                    #.(find-package "KEYWORD")))
                   (collect arg))
                 (collect arg))))))

(defun lambda-list-to-value-list (args)
  `(list ,@(iter (for cell :first args :then (cdr cell))
                 (while cell)
                 (for arg = (first (ensure-list (car cell))))
                 (case arg
                   (&rest (setf cell (cdr cell)))
                   ((&key &allow-other-keys &optional))
                   (t (collect `(cons ',arg ,arg)))))))

