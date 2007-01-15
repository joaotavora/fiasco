;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil-test)

(stefil::eval-always
  (import (let ((*package* (find-package :stefil)))
            (read-from-string "(enable-sharp-boolean-syntax *suite* test count-tests
                                remf-keywords rebind parent-of name-of *tests* eval-always
                                extract-assert-expression-and-message record-failure record-failure*
                                assertion-count-of test-count-of failure-descriptions-of
                                in-global-context in-context)")))
  (shadow (list 'stefil-test::deftest)))

(enable-sharp-boolean-syntax)

(in-suite* stefil-self-test :description "Stefil self tests")

(defparameter *temp-suite* (defsuite stefil-temp-suite :description "Suite used when the tests are running"))

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
         (bind ((temp-suite (eval `(defsuite ,suite-name :in *suite*))))
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

(deftest assertions (&key (test-name (gensym "TEMP-TEST")))
  (unwind-protect
       (bind ((*debug-on-unexpected-error* #f)
              (*debug-on-assertion-failure* #f))
         (eval `(deftest ,test-name ()
                 (is (= 42 42))
                 (is (= 1 42))
                 (is (not (= 42 42)))))
         (in-global-context context
           (bind ((old-assertion-count (assertion-count-of context))
                  (old-failure-descriptions (failure-descriptions-of context)))
             (funcall test-name)
             (is (= (assertion-count-of context)
                    (+ old-assertion-count 3)))
             (is (= (length (failure-descriptions-of context))
                    (+ (length old-failure-descriptions) 2)))
             (setf (failure-descriptions-of context) old-failure-descriptions))))
    (rem-test test-name :otherwise nil))
  (values))

