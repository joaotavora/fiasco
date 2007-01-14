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
   (check-count 0)))

(define-dynamic-context context
  ((test-name)
   (test-arguments))
  :chain-parents #t)

(defclass* failure-description ()
  ((test-context-backtrace)))

(defclass* failed-assertion (failure-description)
  ())

(defclass* unexpected-error (failure-description)
  ((condition)))

(defmacro deftest (&whole whole name args &body body)
  (bind (((values remaining-forms declarations documentation) (parse-body body :documentation #t :whole whole)))
    (with-unique-names (toplevel-p)
      `(defun ,name ,args
        ,documentation
        ,@declarations
        (declare (optimize (debug 3)))
        (bind ((,toplevel-p (not (has-global-context))))
          (ensure-global-context ()
            (with-new-context (:test-name ',name
                                          :test-arguments (list ,@(iter (for cell :first args :then (cdr cell))
                                                                        (while cell)
                                                                        (for arg = (car cell))
                                                                        (case arg
                                                                          ('&rest (setf cell (cdr cell)))
                                                                          (('&key &allow-other-keys))
                                                                          (t (collect `(cons ',arg ,arg)))))))
              (handler-bind
                  ((serious-condition (lambda (c)
                                        (record-failure 'unexpected-error :condition c)
                                        (throw 'abort-test-body (values)))))
                (multiple-value-prog1 (catch 'abort-test-body
                                        ,@remaining-forms)
                  (when ,toplevel-p
                    ;; when leaving the toplevel test then ignore the return value and return the toplevel context
                    (return-from ,name (current-global-context))))))))))))

(defun record-failure (type &rest args)
  (in-global-context (failure-descriptions)
    (bind ((desc (apply #'make-instance type
                        :test-context-backtrace (iter (for context :first (current-context) :then (parent-context-of context))
                                                      (while context)
                                                      (collect context))
                        args)))
      (push desc failure-descriptions))))

(defmacro is (&rest args)
  (bind ((((predicate expected actual)) args))
    `(in-global-context global-context
      (incf (check-count-of global-context))
      (unless (,predicate ,expected ,actual)
        (record-failure 'failure-description))
      (values))))

