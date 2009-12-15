;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

(defun make-suite (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))

(defmacro defsuite (name-or-name-with-args &optional args &body body)
  (destructuring-bind (name &rest deftest-args)
      (ensure-list name-or-name-with-args)
    (with-unique-names (test)
      `(progn
        (deftest (,name ,@deftest-args) ,args
          (let* ((,test (find-test ',name)))
            (flet ((run-child-tests ()
                     (loop
                       :for subtest :being :the :hash-values :of (children-of ,test)
                       :when (and (auto-call-p subtest)
                                  (or (zerop (length (lambda-list-of subtest)))
                                      (member (first (lambda-list-of subtest)) '(&key &optional))))
                         :do (funcall (name-of subtest)))))
              ,@(or body
                    `((if (test-was-run-p ,test)
                          (warn "Skipped executing already run tests suite ~S" (name-of ,test))
                          (run-child-tests))))))
          (values))
        (values (find-test ',name))))))

(defmacro defsuite* (name-or-name-with-args &optional args &body body)
  "Equivalent to (in-suite (defsuite ...)) which is the preferred way to define suites."
  `(setf *suite* (%in-suite (defsuite ,name-or-name-with-args ,args ,@body))))

(setf *root-suite* (make-suite 'root-suite :documentation "Root Suite"))
(setf *suite* *root-suite*)

(defmacro in-root-suite ()
  "Used to reset the current suite to protect from other project's last in-suite statement. Unfortunately there's noone for us to rebind *suite* when a file is loaded, so we can't behave exactly like *package* and in-package."
  `(setf *suite* *root-suite*))

(defun %in-suite (name)
  (assert (typep name '(or symbol test)) () "Suite names should be symbols or literal TEST instances instead of ~S" name)
  (etypecase name
    (symbol (find-test name :otherwise (lambda ()
                                         (cerror "Create a new suite named ~A." "Unkown suite ~A." name)
                                         (eval `(defsuite ',name)))))
    (test name)))

(defmacro in-suite (name)
  `(setf *suite* (%in-suite ',name)))
