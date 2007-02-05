;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(defmacro defsuite (name-or-name-with-args &body body)
  (bind (((name &rest args) (ensure-list name-or-name-with-args)))
    (with-unique-names (test)
      `(progn
        (rem-test ',name :otherwise nil)
        (deftest (,name ,@args) ()
          (bind ((,test (get-test ',name)))
            (flet ((run-child-tests ()
                     (iter (for (nil subtest) :in-hashtable (children-of ,test))
                           (when (and (auto-call-p subtest)
                                      (or (zerop (length (lambda-list-of subtest)))
                                          (member (first (lambda-list-of subtest)) '(&key &optional))))
                             (funcall (name-of subtest))))))
              ,@(or body
                    `((if (test-was-run-p ,test)
                          (warn "Skipped executing already ran tests suite ~S" (name-of ,test))
                          (run-children))))))
          (values))
        (values (get-test ',name))))))

(defmacro defsuite* (name &body body)
  `(setf *suite* (defsuite ,name ,@body)))

(setf *suite* (make-suite 'global-suite :documentation "Default Suite"))

(defmacro in-suite (suite-name)
  `(setf *suite* (get-test ',suite-name
                  :otherwise (lambda ()
                               (cerror "Create a new suite named ~A."
                                       "Unkown suite ~A." ',suite-name)
                               (defsuite ,suite-name)))))

(defmacro in-suite* (name &body body)
  "Just like in-suite, but silently creates the named suite if it does not exists."
  (with-unique-names (suite)
    `(let ((,suite (get-test ,name :otherwise (lambda ()
                                                (defsuite ,name ,@body)))))
      (in-suite ,suite))))



