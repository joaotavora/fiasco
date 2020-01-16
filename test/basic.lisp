;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.
(fiasco:define-test-package #:fiasco-basic-self-tests
  (:use #:cl)

  ;; These tests are testing FIASCO's own internals, so its
  ;; more-or-less OK to explicitly import some of it. Or is it?
  (:import-from #:fiasco #:*suite*
                #:name-of
                #:parent-of
                #:delete-test
                #:count-tests

                #:failures-of
                #:assertions-of

                #:expected-p
                #:children-contexts-of
                #:parent-context-of

                #:*context*

                #:lambda-list-to-value-list-expression))
(in-package #:fiasco-basic-self-tests)

(deftest lifecycle ()
  (let* ((original-test-count (count-tests *suite*))
         (suite-name (gensym "TEMP-SUITE"))
         (test-name (gensym "TEMP-TEST"))
         (transient-test-name (gensym "TRANSIENT-TEST"))
         (transient-suite-name (gensym "TRANSIENT-SUITE"))
         (temp-suite (eval `(defsuite (,suite-name :in ,*suite*))))
         (transient-suite (eval `(defsuite (,transient-suite-name :in ,*suite*)))))
    (unwind-protect
         (progn
           ;; The suites are freshly defined, test some things
           ;; 
           (is (eq (parent-of temp-suite) *suite*))
           (is (= (count-tests *suite*) (+ 2 original-test-count)))
           (is (eq (find-test (name-of temp-suite)) temp-suite))
           (is (= (count-tests temp-suite) 0))

           ;; Now define a test
           ;; 
           (eval `(deftest (,test-name :in ,suite-name ) ()))
           (is (= (count-tests temp-suite) 1))

           ;; redefining a test should keep the original test object
           ;; identity
           (let ((test (find-test test-name)))
             (eval `(deftest ,test-name ()))
             (is (eq test (find-test test-name))))

           ;; Define a second test in the same TEMP-SUITE
           (eval `(deftest (,transient-test-name :in ,suite-name) ()))
           (is (= (count-tests temp-suite) 2))
           
           (let ((transient-test (find-test transient-test-name)))
             (is (eq temp-suite (parent-of transient-test)))
             ;; Now redefine in another suite, TRANSIENT-SUITE
             (eval `(deftest (,transient-test-name :in ,transient-suite-name) ()))
             (is (eq transient-suite (parent-of transient-test)))
             (is (= (count-tests temp-suite) 1))
             (is (= (count-tests transient-suite) 1))))
      (setf (find-test suite-name) nil)
      (setf (find-test transient-suite-name) nil))
    (signals error (find-test transient-test-name))
    (signals error (find-test suite-name))))

(defparameter *global-counter-for-lexical-test* 0)

(let ((counter 0))
  (setf *global-counter-for-lexical-test* 0)
  (deftest counter-in-lexical-environment ()
    (incf counter)
    (incf *global-counter-for-lexical-test*)
    (is (= counter *global-counter-for-lexical-test*))))

(defmacro false-macro ()
  nil)

(defmacro true-macro ()
  t)

(deftest assertions (&key (test-name (gensym "TEMP-TEST")))
  (unwind-protect
       (eval `(deftest ,test-name ()
                (is (= 42 42))
                (is (= 1 42))                                ; fails
                (is (not (= 42 42)))                         ; fails
                (is (true-macro))
                (is (true-macro) "Oh yes, glad that it's ~a" "true")
                (is (not (false-macro)))

                (signals serious-condition (error "foo"))
                (signals serious-condition 'not)             ; fails

                (not-signals warning (warn "foo"))           ; fails
                (not-signals warning 'not)
                
                (with-expected-failures
                  (ignore-errors
                    (finishes (error "expected failure"))))  ; fails
                (finishes 42)
                (ignore-errors                               ; fails
                  (finishes (error "foo")))))
    (progn
      ;; this uglyness here is due to testing the test framework which is inherently
      ;; not nestable, so we need to backup and restore some state
      (let* ((context *context*)
             (old-assertion-count (length (assertions-of context)))
             (old-failure-description-count (length (failures-of context))))
        (unwind-protect
             (progn
               (let ((*debug-on-unexpected-error* nil)
                     (*debug-on-assertion-failure* nil)
                     (*print-test-run-progress* nil))
                 (funcall test-name))))
        (is (= (length (assertions-of context))
               (+ old-assertion-count 14))) ; also includes the current assertion
        (is (= (length (failures-of context))
               (+ old-failure-description-count 6)))
        (is (= 1 (count-if 'expected-p (failures-of context))))
        (is (= 1 (length (children-contexts-of context))))
        ;; drop the subtest by the test-test
        ;; 
        (setf (parent-context-of (first (children-contexts-of context))) nil)
        (is (= 0 (length (children-contexts-of context)))))
      ;; Take this occasion to test some deleting, too
      ;;
      (delete-test test-name :otherwise nil)
      (signals error (delete-test test-name :otherwise :error))
      (is (not (find-test test-name :otherwise nil)))
      ))
  (values))

(deftest slightly-verbose-test ()
  (format *error-output* "~&Watch out for me")
  (is t))

(deftest slightly-verbose-test-2 ()
  (format *error-output* "...And me")
  (is t))

(deftest lambda-list-processing ()
  (is (equal (lambda-list-to-value-list-expression '(p1 p2 &optional o1 (o2 "o2") &key k1 (k2 "k2") &allow-other-keys))
             '(list (cons 'p1 p1) (cons 'p2 p2) (cons 'o1 o1) (cons 'o2 o2) (cons 'k1 k1)
               (cons 'k2 k2)))))

;; Local Variables:
;; coding: utf-8-unix
;; End:
