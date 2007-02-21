;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

(defpackage :stefil-test
    (:use :common-lisp :metabang-bind :defclass-star :alexandria :iterate :stefil)
  (:shadow #:deftest))

(eval-always
  (import
   '(enable-sharp-boolean-syntax *suite* test count-tests
     remf-keywords rebind parent-of name-of *tests* eval-always
     extract-assert-expression-and-message record-failure record-failure*
     assertion-count-of run-tests-of failure-descriptions-of
     in-global-context in-context debug-on-unexpected-error-p
     debug-on-assertion-failure-p print-test-run-progress-p
     file-header)
   (find-package :stefil-test)))

(in-package :stefil-test)

#.(file-header)

(defsuite (stefil-temp-suite :description "Suite active when the Stefil self-tests are being run"))

(defsuite* (stefil-self-test :description "Stefil self tests"))

;; hide deftest with a local version that rebinds and sets *suite* when executing the body
(defmacro deftest (name args &body body)
  `(stefil:deftest ,name ,args
    (rebind (*suite*)
      (in-suite stefil-temp-suite)
      ,@body)))

(deftest lifecycle (&key (test-name (gensym "TEMP-TEST")) (suite-name (gensym "TEMP-SUITE")))
  (bind ((original-test-count (count-tests *suite*))
         (original-current-suite *suite*)
         (transient-test-name (gensym "TRANSIENT-TEST")))
    (unwind-protect
         (progn
           (eval `(deftest ,test-name ()))
           (is (= (count-tests *suite*) (1+ original-test-count))))
      (rem-test test-name :otherwise nil))
    (is (= (count-tests *suite*) original-test-count))
    (unwind-protect
         (bind ((temp-suite (eval `(defsuite (,suite-name :in ,*suite*)))))
           (is (= (count-tests *suite*) (1+ original-test-count)))
           (is (eq (parent-of temp-suite) *suite*))
           (is (eq (get-test (name-of temp-suite)) temp-suite))
           (eval `(in-suite ,(name-of temp-suite)))
           (is (eq *suite* (get-test suite-name)))
           (eval `(deftest ,transient-test-name ())))
      (rem-test suite-name))
    (signals error (get-test transient-test-name))
    (signals error (get-test suite-name))
    (is (= (count-tests *suite*) original-test-count))
    (is (eq original-current-suite *suite*))))

(defparameter *global-counter-for-lexical-test* 0)

(let ((counter 0))
  (setf *global-counter-for-lexical-test* 0)
  (deftest (counter-in-lexical-environment :compile-before-run #f) ()
    (incf counter)
    (incf *global-counter-for-lexical-test*)
    (is (= counter *global-counter-for-lexical-test*))))

(defmacro false-macro ()
  #f)

(defmacro true-macro ()
  #t)

(deftest assertions (&key (test-name (gensym "TEMP-TEST")))
  (unwind-protect
       (eval `(deftest ,test-name ()
               (is (= 42 42))
               (is (= 1 42))
               (is (not (= 42 42)))
               (is (true-macro))
               (is (not (false-macro)))))
    (in-global-context context
      ;; this uglyness here is due to testing the test framework which is inherently
      ;; not nestable, so we need to backup and restore some state
      (bind ((old-assertion-count (assertion-count-of context))
             (old-failure-description-count (length (failure-descriptions-of context)))
             (old-debug-on-unexpected-error (debug-on-unexpected-error-p context))
             (old-debug-on-assertion-failure (debug-on-assertion-failure-p context))
             (old-print-test-run-progress-p (print-test-run-progress-p context)))
        (unwind-protect
             (progn
               (setf (debug-on-unexpected-error-p context) #f)
               (setf (debug-on-assertion-failure-p context) #f)
               (setf (print-test-run-progress-p context) #f)
               (funcall test-name))
          (setf (debug-on-unexpected-error-p context) old-debug-on-unexpected-error)
          (setf (debug-on-assertion-failure-p context) old-debug-on-assertion-failure)
          (setf (print-test-run-progress-p context) old-print-test-run-progress-p))
        (is (= (assertion-count-of context)
               (+ old-assertion-count 6))) ; also includes the current assertion
        (is (= (length (failure-descriptions-of context))
               (+ old-failure-description-count 2)))
        (dotimes (i (- (length (failure-descriptions-of context))
                       old-failure-description-count))
          ;; drop failures registered by the test-test
          (vector-pop (failure-descriptions-of context))))
      (rem-test test-name :otherwise nil)))
  (values))


(defparameter *fixture-test-global* '())

(defixture test-fixture
  (:setup (push '42 *fixture-test-global*))
  (:teardown (setf *fixture-test-global* (remove '42 *fixture-test-global*))))

(defparameter *fixture-test-counter* 0)

(defixture simple-test-fixture
  (incf *fixture-test-counter*))

(deftest fixtures ()
  (with-fixture simple-test-fixture
    (is (not (zerop *fixture-test-counter*)))
    (with-fixture test-fixture
      (is (equal *fixture-test-global* '(42)))
      (nested-fixtures1)
      (nested-fixtures2)
      (is (equal *fixture-test-global* '(42)))))
  (is (equal *fixture-test-global* '())))

(defun nested-fixtures1 ()
  (with-fixture test-fixture
    (is (equal *fixture-test-global* '(42)))))

(deftest (nested-fixtures2 :auto-call #f) ()
  (with-fixture test-fixture
    (is (equal *fixture-test-global* '(42)))))
