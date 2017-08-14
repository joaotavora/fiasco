(fiasco:define-test-package (#:fiasco-suite-tests :in fiasco::root-suite)
  ;; HACK: We exceptionally hang the resulting test suite in
  ;; FIASCO::ROOT-SUITE (not FIASCO-SUITES::ALL-TESTS) so that
  ;; FIASCO:ALL-TESTS doesn't call it. This prevent an infinite loop
  ;; later on.
  ;; 
  (:use :cl :fiasco))
(in-package #:fiasco-suite-tests)

(defvar *foo*)
(defvar *bar*)
(defvar *baz*)

;;; Test basic suite parenting and interaction with
;;; DEFINE-TEST-PACKAGE.
;;;
;;; Define four test suites: one parent suite, two children, and
;;; another unrelated suite. Except for the parent suite, all of them
;;; are defined with DEFINE-TEST-PACKAGE and carry single test that
;;; signals a flag.
;;;
(defsuite (dummy-parent-suite :in fiasco-suites::all-tests))

(define-test-package (child-suite-foo :in fiasco-suite-tests::dummy-parent-suite)
  (:use :cl :fiasco))
(in-package child-suite-foo)

(deftest dummy-foo () (setq fiasco-suite-tests::*foo* t))

(define-test-package (child-suite-bar :in fiasco-suite-tests::dummy-parent-suite)
  (:use :cl :fiasco))
(in-package child-suite-bar)

(deftest dummy-bar () (setq fiasco-suite-tests::*bar* t))

(define-test-package (child-suite-baz)
  (:use :cl :fiasco))
(in-package child-suite-baz)

(deftest dummy-baz () (setq fiasco-suite-tests::*baz* t))

(in-package #:fiasco-suite-tests)

(deftest test-nested-suites ()
  (let ((*bar* nil)
        (*foo* nil)
        (*baz* nil))
    (dummy-parent-suite)
    (is *foo*)
    (is *bar*)
    (is (not *baz*))))

(deftest test-top-suites ()
  (let ((*bar* nil)
        (*foo* nil)
        (*baz* nil))
    ;; This call would trigger an infinite loop if fiasco-suite-tests
    ;; wasn't unparented as above.
    ;;
    (all-tests)
    (is *foo*)
    (is *bar*)
    (is *baz*)))
