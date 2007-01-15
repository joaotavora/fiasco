;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(defvar *suite*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lookup

(defcondition* test-related-condition ()
  ((test nil)))

(defcondition* test-style-warning (style-warning test-related-condition simple-warning)
  ())

;; this is not thread-safe, but...
(defparameter *tests* (make-hash-table :test 'eql))

(defun get-test (name &key (otherwise :error))
  (bind (((values test found-p) (gethash name *tests*)))
    (when (and (not found-p)
               otherwise)
      (etypecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf get-test) (value key)
  (when (gethash key *tests*)
    (warn 'test-style-warning
          :format-control "Redefining test ~A"
          :format-arguments (list (let ((*package* #.(find-package "KEYWORD")))
                                    (format nil "~S" key)))))
  (setf (gethash key *tests*) value))

(defun rem-test (name &rest args)
  (bind ((test (apply #'get-test name args))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite and has no parent")
      (remhash name *tests*)
      (setf (parent-of test) nil)
      (fmakunbound (name-of test))
      (iter (for (nil subtest) :in-hashtable (children-of test))
            (rem-test (name-of subtest)))
      (when (eq *suite* test)
        (setf *suite* parent)))
    test))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the classes

(defclass* testable ()
  ((name :type symbol)
   (parent nil :initarg nil :type (or null testable))
   (children (make-hash-table) :documentation "A mapping from testable names to testables")))

(defprint-object (self testable :identity #f :type #f)
  (format t "test ~S" (name-of self))
  (bind ((children (count-tests self)))
    (unless (zerop children)
      (format t " :tests ~S" children))))

(defmethod shared-initialize :after ((self testable) slot-names &key (in (and (boundp '*suite*) *suite*)) &allow-other-keys)
  (assert (name-of self))
  (setf (get-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (setf (parent-of self) (if in
                             (if (typep in 'testable)
                                 in
                                 (get-test in))
                             (parent-of self))))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (bind ((old-parent (parent-of self)))
    (when old-parent
      (remhash (name-of self) (children-of old-parent)))
    (prog1
        (call-next-method)
      (when new-parent
        (setf (gethash (name-of self) (children-of new-parent)) self)))))

(defgeneric count-tests (testable)
  (:method ((self testable))
           (+ (hash-table-count (children-of self))
              (iter (for (nil child) :in-hashtable (children-of self))
                    (summing (count-tests child))))))

(defclass* test (testable)
  ((package)
   (lambda-list)
   (compile-before-run #t :type boolean)
   (declarations)
   (documentation)
   (body)))

(defun make-test (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))

(defun make-suite (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; failure descriptions

(defclass* failure-description ()
  ((test-context-backtrace)))

(defclass* skipped-by-restart (failure-description)
  ())

(defclass* failed-assertion (failure-description)
  ((form)
   (format-control)
   (format-arguments)))

(defmethod describe-object ((self failed-assertion) stream)
  (let ((*print-circle* nil))
    (apply #'format stream (format-control-of self) (format-arguments-of self))))

(defprint-object (self failed-assertion :identity #f :type #f)
  (format t "failure ~{~A~^/~} ~S"
          (mapcar (compose #'name-of #'test-of)
                  (reverse (test-context-backtrace-of self)))
          (form-of self)))

(defclass* missing-condition (failure-description)
  ((condition)))

(defclass* unexpected-error (failure-description)
  ((condition)))



