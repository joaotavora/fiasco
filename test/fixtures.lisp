;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil.test)

(defsuite* (fixtures :in test))

(defparameter *fixture-test-global* '())

(defixture test-fixture/unwind-protect
  (push 42 *fixture-test-global*)
  (unwind-protect
       (-body-)
    (removef *fixture-test-global* 42)))

(defparameter *fixture-test-counter* 0)

(defixture test-fixture/simple
  (incf *fixture-test-counter*)
  (-body-))

(deftest fixtures/simple ()
  (with-fixture test-fixture/simple
    (is (not (zerop *fixture-test-counter*)))
    (with-fixture test-fixture/unwind-protect
      (is (equal *fixture-test-global* '(42)))
      (%nested-fixtures1)
      (is (equal *fixture-test-global* '(42)))
      (%nested-fixtures2)
      (is (equal *fixture-test-global* '(42))))
    (is (equal *fixture-test-global* '()))))

(defun %nested-fixtures1 ()
  (with-fixture test-fixture/unwind-protect
    (is (equal *fixture-test-global* '(42)))))

(deftest (%nested-fixtures2 :auto-call nil) ()
  (with-fixture test-fixture/unwind-protect
    (is (equal *fixture-test-global* '(42)))))
