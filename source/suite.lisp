;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

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
                            :for subtest :being :the :hash-values :of (children-of ,test)
                            :when (and (auto-call? subtest)
                                       (or (zerop (length (lambda-list-of subtest)))
                                           (member (first (lambda-list-of subtest)) '(&rest &key &optional))))
                              :do (funcall (name-of subtest))))
                        (run-child-tests ()
                          ;; TODO delme eventually?
                          ;; (simple-style-warning "~S is obsolete, use ~S to invoke child tests in a testsuite!" 'run-child-tests '-run-child-tests-)
                          (-run-child-tests-)))
                 (declare (ignorable #'run-child-tests))
                 ,@(or body
                       `((if (test-was-run-p ,test)
                             (warn "Skipped executing already run tests suite ~S" (name-of ,test))
                             (-run-child-tests-))))))
             (values))
           (let ((suite (find-test ',name)))
             ,(when bind-to-package
                `(setf (gethash ,bind-to-package *package-bound-suites*) suite))
             (values suite)))))))

(defmacro defsuite* (name-or-name-with-args &optional args &body body)
  "Equivalent to (in-suite (defsuite ...)) which is the preferred way to define suites."
  `(setf *suite* (%in-suite (defsuite ,name-or-name-with-args ,args ,@body))))

(setf *root-suite* (make-suite 'root-suite :documentation "Root Suite" :in nil))
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


;;; define-test-package and friends
(defpackage :stefil-suites
  (:use)
  (:documentation "Namespace for Stefil suites defined via DEFINE-TEST-PACKAGE."))

(defsuite (stefil-suites::all-tests :in root-suite :ignore-home-package t))

(defun all-tests ()
  "Run all currently defined tests."
  (stefil-suites::all-tests))

(defmacro define-test-package (name &body package-options)
  "Defines a package NAME and binds to it a new suite STEFIL-SUITES::NAME.

The binding between package and suite means that tests defined under the package
NAME are automatically added to the bounded suite. A new function
RUN-PACKAGE-TESTS (exported from the newly created package) is the preferred way
to execute the suite.

Package NAME is defined via normal `defpackage', and in addition to processing
PACKAGE-OPTIONS, automatically USEs the :STEFIL and :CL packages."
  (unless (find-package name)
    (make-package name :use nil))
  (let ((run-package-tests (intern "RUN-PACKAGE-TESTS" name))
        (suite-sym (intern (string name) :stefil-suites)))
    `(progn
       (defpackage ,name
         ,@(append `((:export ,(symbol-name run-package-tests))
                     (:use :stefil :cl))
                   package-options))
       (defsuite (,suite-sym :ignore-home-package t
                             :bind-to-package ,name
                             :in stefil-suites::all-tests))
       (defun ,run-package-tests (&key (describe-failures t)
                                       verbose
                                       (stream t)
                                       interactive)
         (run-suite-tests ',suite-sym
                          :verbose verbose
                          :stream stream
                          :interactive interactive)
         (unless (or interactive
                     (null describe-failures)
                     (zerop (length (failure-descriptions-of *last-test-result*))))
           (describe-failed-tests))
         *last-test-result*))))

(defvar *pretty-log-accumulated-assertion-count* 0)
(defvar *pretty-log-accumulated-failure-descriptions* nil)
(defvar *pretty-log-stream* nil)
(defvar *pretty-log-verbose-p* nil)

(defun run-suite-tests (suite-sym &key verbose (stream t) interactive)
  (let ((*debug-on-unexpected-error* interactive)
        (*debug-on-assertion-failure* interactive)
        (*print-test-run-progress* nil)
        (*pretty-log-stream* stream)
        (*pretty-log-verbose-p* verbose)
        (*pretty-log-accumulated-failure-descriptions* nil)
        (*pretty-log-accumulated-assertion-count* 0)
        (*run-test-function* #'pretty-run-test))
    (funcall suite-sym)
    (terpri stream)))

(defvar *within-non-suite-test* nil
  "True within the scope of a non-suite test. Used to suppress printing test
  status for recursive test calls.")

(defun pretty-run-test (test function)
  ;; HACK: until printing of recursive tests is implemented nicely we avoid
  ;; reporting non-toplevel tests altogether.
  (when *within-non-suite-test*
    (return-from pretty-run-test (run-test-body-in-handlers test function)))
  (labels ((depth-of (context)
             (let ((depth 0))
               (loop while (setf context (parent-context-of context))
                     do (incf depth))
               depth))
           (pp (suffix format-control &rest format-args)
             (let* ((depth (depth-of (current-context)))
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
           (v-list (multiple-value-list (run-test-body-in-handlers test function)))
           (results *global-context*))
      (unless (suite-p)
        (pp (if (zerop (number-of-added-failure-descriptions-of (current-context)))
                " OK "
                "FAIL")
            "~&~A" (stefil::name-of test))
        (when *pretty-log-verbose-p*
          (pp nil
              "    (~A)"
              (or (documentation (name-of test) 'function)
                  "no docstring for this test"))
          (let* ((assertion-count (- (assertion-count-of results) *pretty-log-accumulated-assertion-count*))
                 (failure-descriptions (remove-if #'(lambda (desc)
                                                      (find desc *pretty-log-accumulated-failure-descriptions*))
                                                  (failure-descriptions-of results)))
                 (failed-assertion-count
                   (count-if (alexandria:rcurry #'typep 'failed-assertion) failure-descriptions))
                 (unexpected-error-count
                   (count-if (alexandria:rcurry #'typep 'unexpected-error) failure-descriptions))
                 (expected-count
                   (count-if 'expected-p failure-descriptions)))
            (pp nil
                "    (~A assertions, ~A failed, ~A errors, ~A expected)~%"
                assertion-count failed-assertion-count unexpected-error-count expected-count))))
      (setf *pretty-log-accumulated-assertion-count* (assertion-count-of results))
      (setf *pretty-log-accumulated-failure-descriptions* (failure-descriptions-of results))
      (values-list v-list))))

(defun indented-format (level stream format-control &rest format-arguments)
  (let ((line-prefix (make-string level :initial-element #\Space)))
    (let ((output (format nil "~?~%" format-control format-arguments)))
      (with-input-from-string (s output)
        (loop for line = (read-line s nil nil) until (null line)
              do (format stream "~A~A~%" line-prefix line))))))

(defun describe-failed-tests (&key (result *last-test-result*) (stream t))
  "Prints out a report for RESULT in STREAM.

RESULT defaults to `*last-test-result*' and STREAM defaults to t"
  (format stream "~&~%Test failures:~%")
  (let ((descs (failure-descriptions-of result)))
    (cond ((zerop (length descs))
           (format stream "~&~%[no failures!]"))
          (t
           (dotimes (i (length descs))
             (let* ((desc (aref descs i))
                    (format-control "~A")
                    (format-arguments (list desc)))
               ;; XXX: most of Stefil's conditions specialise DESCRIBE-OBJECT
               ;; with nice human-readable messages. We should add any missing
               ;; ones (like UNEXPECTED-ERROR) and ditch this code.
               (etypecase desc
                 (unexpected-error
                  (let ((c (condition-of desc)))
                    (when (typep c 'simple-condition)
                      ;; SIMPLE-CONDITIONs behaving badly, like some Allegro
                      ;; ones, won't have a bound FORMAT-CONTROL slot.
                      ;;
                      (handler-case
                          (progn
                            (setf format-control (simple-condition-format-control c))
                            (setf format-arguments
                                  (simple-condition-format-arguments c)))
                        (error ()
                          (setf format-arguments (list c)))))))
                 (failed-assertion
                  (setf format-control (format-control-of desc)
                        format-arguments (format-arguments-of desc)))
                 (missing-condition
                  (setf format-control "~A"
                        format-arguments (list (with-output-to-string (stream)
                                                 (describe desc stream)))))
                 (null
                  (setf format-control "Test succeeded!")))
               (format stream "~%Failure ~A: ~A when running ~S~%~%"
                       (1+ i)
                       (type-of desc)
                       (name-of (test-of (first (test-context-backtrace-of desc)))))
               (indented-format 4 stream "~?" format-control format-arguments)))))))
