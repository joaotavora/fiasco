;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :fiasco)

(defun find-suite-for-package (package)
  (gethash package *package-bound-suites*))

(defun make-suite (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))

(defmacro defsuite (name-or-name-with-args &optional args &body body)
  (destructuring-bind (name &rest deftest-args)
      (ensure-list name-or-name-with-args)
    (let ((bind-to-package (getf deftest-args :bind-to-package)))
      (setq bind-to-package
            (if (eq t bind-to-package)
                *package*
                (find-package bind-to-package)))
      (remf deftest-args :bind-to-package)
      (with-unique-names (test)
        `(progn
           (deftest (,name ,@deftest-args) ,args
             (let* ((,test (find-test ',name)))
               (labels ((-run-child-tests- ()
                          (loop
                            :for subtest :being :the :hash-values
                              :of (children-of ,test)
                            :when (and (auto-call? subtest)
                                       (or (zerop (length
                                                   (lambda-list-of subtest)))
                                           (member (first
                                                    (lambda-list-of subtest))
                                                   '(&rest &key &optional))))
                              :do (funcall (name-of subtest))))
                        (run-child-tests ()
                          ;; TODO delme eventually?
                          ;; (simple-style-warning "~S is obsolete,
                          ;; use ~S to invoke child tests in a
                          ;; testsuite!" 'run-child-tests
                          ;; '-run-child-tests-)
                          (-run-child-tests-)))
                 (declare (ignorable #'run-child-tests))
                 ,@(or body
                       `((if (test-was-run-p ,test)
                             (warn "~
Skipped executing already run tests suite ~S" (name-of ,test))
                             (-run-child-tests-))))))
             (values))
           (let ((suite (find-test ',name)))
             ,(when bind-to-package
                `(setf (gethash ,bind-to-package *package-bound-suites*) suite))
             (values suite)))))))

(setf *root-suite* (make-suite 'root-suite :documentation "Root Suite" :in nil))
(setf *suite* *root-suite*)


;;; define-test-package and friends
(defpackage :fiasco-suites
  (:use)
  (:documentation "~
Namespace for Fiasco suites defined via DEFINE-TEST-PACKAGE."))

(defsuite (fiasco-suites::all-tests :in root-suite))

(defun all-tests ()
  "Run all currently defined tests."
  (fiasco-suites::all-tests))

(defmacro define-test-package (name &body package-options)
  "Defines package NAME and binds to it a new suite test suite.

The binding between package and suite means that tests defined under
the package NAME are automatically added to the bounded suite. The
function RUN-PACKAGE-TESTS is the preferred way to execute the
suite.

Package NAME is defined via normal `defpackage', and in addition to processing
PACKAGE-OPTIONS, automatically USEs the :FIASCO and :CL packages."
  (unless (find-package name)
    (make-package name :use nil))
  (let ((suite-sym (intern (string name) :fiasco-suites)))
    `(progn
       (defpackage ,name
         ,@(append `((:use :fiasco :cl))
                   package-options))
       (defsuite (,suite-sym :bind-to-package ,name
                             :in fiasco-suites::all-tests)))))

(defvar *pretty-log-stream* nil)
(defvar *pretty-log-verbose-p* nil)

(defun run-package-tests (&key (package *package* package-supplied-p)
                               (packages (list *package*) packages-supplied-p)
                               (describe-failures t)
                               verbose
                               (stream *standard-output*)
                               interactive)
  "Execute test suite associated with PACKAGE.
PACKAGE defaults to the current package.

With optional INTERACTIVE, run tests interactively, i.e. break on
errors and unexpected assertion failures.

With optional DESCRIBE-FAILURES, T by default, describe failures to
optional STREAM, which defaults to *STANDARD-OUTPUT*.  With optional
VERBOSE print more information about each test run, like its
docstring."
  (assert (not (and packages-supplied-p package-supplied-p))
          nil
          "Provide either :PACKAGE or :PACKAGES, not both")
  (loop for package in (alexandria:ensure-list (if packages-supplied-p
                                                   packages
                                                   package))
        for suite = (find-suite-for-package (find-package package))
        do
           (assert suite nil "Can't find a test suite for package ~a" package)
           (run-suite-tests suite
                            :verbose verbose
                            :stream stream
                            :interactive interactive)
           (unless (or interactive
                       (null describe-failures)
                       (zerop (length (failure-descriptions-of
                                       *last-test-result*))))
             (describe-failed-tests :stream stream))
        collect *last-test-result* into results
        finally
           (return (values (every #'zerop
                                  (mapcar
                                   (alexandria:compose #'length #'failure-descriptions-of)
                                   results))
                           results))))

(defun run-suite-tests (suite-designator &key verbose (stream t) interactive)
  (let ((*debug-on-unexpected-error* interactive)
        (*debug-on-assertion-failure* interactive)
        (*print-test-run-progress* nil)
        (*pretty-log-stream* stream)
        (*pretty-log-verbose-p* verbose)
        (*run-test-function* #'pretty-run-test))
    (funcall (etypecase suite-designator
               (symbol suite-designator)
               (test (name-of suite-designator))))
    (terpri stream)))

(defvar *within-non-suite-test* nil
  "True within the scope of a non-suite test. Used to suppress printing test
  status for recursive test calls.")

(defun pretty-run-test (test function)
  ;; HACK: until printing of recursive tests is implemented nicely we avoid
  ;; reporting non-toplevel tests altogether.
  (when *within-non-suite-test*
    (return-from pretty-run-test (run-test-body-in-handlers test function)))
  (labels
      ((depth-of (context)
         (let ((depth 0))
           (loop while (setf context (parent-context-of context))
                 do (incf depth))
           depth))
       (pp (suffix format-control &rest format-args)
         (let* ((depth (depth-of *context*))
                (body (format nil "~A~A"
                              (make-string (* depth 2) :initial-element #\space)
                              (apply #'format nil format-control format-args))))
           (if suffix
               (format *pretty-log-stream* "~&~A~A[~A]~%"
                       body
                       (make-string (max 1 (- *test-progress-print-right-margin*
                                              (length body)
                                              (length "[XXXX]")))
                                    :initial-element #\space)
                       suffix)
               (format *pretty-log-stream* "~&~A" body))))
       (suite-p ()
         (not (zerop (hash-table-count (children-of test))))))
    (when (suite-p)
      (pp nil "~&~A (Suite)" (name-of test)))
    (let* ((*within-non-suite-test* (not (suite-p)))
           (retval-v-list (multiple-value-list
                           (run-test-body-in-handlers test function)))
           (failures (failure-descriptions-of *context*)))
      (unless (suite-p)
        (pp (if failures " OK " "FAIL")
            "~&~A" (fiasco::name-of test))
        (when *pretty-log-verbose-p*
          (pp nil
              "    (~A)"
              (or (documentation (name-of test) 'function)
                  "no docstring for this test"))
          (pp nil
              "    (~A assertions, ~A failed, ~A errors, ~A expected)~%"
              (assertion-count-of *context*)
              (count-if (alexandria:rcurry #'typep 'failed-assertion) failures)
              (count-if (alexandria:rcurry #'typep 'unexpected-error) failures)
              (count-if 'expected-p failures))))
      (values-list retval-v-list))))

(defun indented-format (level stream format-control &rest format-arguments)
  (let ((line-prefix (make-string level :initial-element #\Space)))
    (let ((output (format nil "~?~%" format-control format-arguments)))
      (with-input-from-string (s output)
        (loop for line = (read-line s nil nil) until (null line)
              do (format stream "~A~A~%" line-prefix line))))))

(defun describe-failed-tests (&key (result *last-test-result*) (stream t))
  "Prints out a report for RESULT in STREAM.

RESULT defaults to `*last-test-result*' and STREAM defaults to t"
  (format stream "~&~%Fiasco! (~a failures)~%"
          (length (failure-descriptions-of result)))
  (let ((descs (failure-descriptions-of result)))
    (cond ((zerop (length descs))
           (format stream "~&~%[no failures!]"))
          (t
           (dotimes (i (length descs))
             (let* ((desc (aref descs i)))
               (format stream "~%  Failure ~A: ~A when running ~S~%"
                       (1+ i)
                       (type-of desc)
                       (name-of (test-of
                                 (first (test-context-backtrace-of desc)))))
               (indented-format 4 stream "~a" (describe-object desc nil))))))))
