;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(defmacro defsuite (name &rest args &key in &allow-other-keys)
  (declare (ignore in))
  `(progn
    (rem-test ',name :otherwise nil)
    (deftest (,name ,@args) ()
      (bind ((test (get-test ',name)))
        (if (test-was-run-p test)
            (warn "Skipped executing already ran tests suite ~S" (name-of test))
            (iter (for (nil subtest) :in-hashtable (children-of test))
                  (if (or (zerop (length (lambda-list-of subtest)))
                          (member (first (lambda-list-of subtest)) '(&key &optional)))
                      (funcall (name-of subtest))
                      (warn "Skipped test ~S because it has mandatory arguments" subtest)))))
      (values))
    (values (get-test ',name))))

(setf *suite* (make-suite 'global-suite :documentation "Global Suite"))

(defmacro in-suite (suite-name)
  `(%in-suite ,suite-name))

(defmacro in-suite* (suite-name &rest args &key &allow-other-keys)
  "Just like in-suite, but silently creates missing suites."
  `(%in-suite ,suite-name :fail-on-error #f ,@args))

(defmacro %in-suite (suite-name &rest args &key (fail-on-error #t) &allow-other-keys)
  (remf-keywords args :fail-on-error)
  (with-unique-names (suite)
    `(progn
      (if-bind ,suite (get-test ',suite-name :otherwise nil)
        (setf *suite* ,suite)
        (progn
          (when ,fail-on-error
            (cerror "Create a new suite named ~A."
                    "Unkown suite ~A." ',suite-name))
          (setf *suite* (eval `(defsuite ,',suite-name ,@',args)))))
      ',suite-name)))

