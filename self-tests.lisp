;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :sinfol-test)

(sinfol::eval-always
  (import (let ((*package* (find-package :sinfol)))
            (read-from-string "(enable-sharp-boolean-syntax *suite* test count-tests
                                remf-keywords rebind parent-of name-of *tests*)")))
  (shadow (list 'sinfol-test::deftest)))

(enable-sharp-boolean-syntax)

(in-suite* sinfol-self-test :description "Sinfol self tests")

(defparameter *temp-suite* (defsuite sinfol-temp-suite :description "Suite used when the tests are running"))

;; hide deftest with a local version that rebinds and sets *suite* when executing the body
(defmacro deftest (name args &body body)
  `(sinfol:deftest ,name ,args
    (rebind (*suite*)
      (in-suite sinfol-temp-suite)
      ,@body)))

(deftest lifecycle (&key (test-name (gensym "TEMP-TEST")) (suite-name (gensym "TEMP-SUITE")))
  (bind ((original-test-count (count-tests *suite*))
         (original-current-suite *suite*))
    (eval `(deftest ,test-name ()))
    (is (= (count-tests *suite*) (1+ original-test-count)))
    (rem-test test-name)
    (is (= (count-tests *suite*) original-test-count))
    (bind ((temp-suite (eval `(defsuite ,suite-name :in *suite*)))
           (transient-test-name (gensym "TRANSIENT-TEST")))
      (is (= (count-tests *suite*) (1+ original-test-count)))
      (is (eq (parent-of temp-suite) *suite*))
      (is (eq (get-test (name-of temp-suite)) temp-suite))
      (eval `(in-suite ,(name-of temp-suite)))
      (is (eq *suite* (get-test suite-name)))
      (eval `(deftest ,transient-test-name ()))
      (rem-test suite-name)
      (signals error (get-test transient-test-name)))
    (is (= (count-tests *suite*) original-test-count))
    (is (eq original-current-suite *suite*))))



