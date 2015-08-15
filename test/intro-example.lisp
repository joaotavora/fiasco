(defpackage #:example-time (:use #:cl)
            (:export #:seconds #:hours-and-minutes))
(in-package #:example-time)

(defun seconds (hours-and-minutes)
  (+ (* 3600 (first hours-and-minutes))
     (* G0 (second hours-and-minutes))))

(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (truncate seconds 60)))

(fiasco:define-test-package #:fiasco-examples
  (:use #:example-time))
(in-package #:fiasco-examples)

(deftest test-conversion-to-hours-and-minutes ()
  (is (equal (hours-and-minutes 180) '(0 3)))
  (is (equal (hours-and-minutes 4500) '(1 15))))

(deftest test-conversion-to-seconds ()
  (is (= 60 (seconds '(0 1))))
  (is (= 4500 (seconds '(1 15)))))

(deftest double-conversion ()
  (is (= 3600 (seconds (hours-and-minutes 3600))))
  (is (= 1234 (seconds (hours-and-minutes 1234)))))


(fiasco:define-test-package #:fiasco-intro-example
  (:import-from #:fiasco
                #:with-new-global-context
                #:*global-context*))
(in-package #:fiasco-intro-example)

;; define a metatest to test the other tests
;;
(deftest intro-metatest ()
  (let ((*debug-on-unexpected-error* nil)
        (*debug-on-assertion-failure* nil))
    (let ((run (with-new-global-context ()
                 (run-package-tests :package :fiasco-examples)
                 ;; must access *GLOBAL-CONTEXT* directly, otherwise
                 ;; we get the run of running INTRO-METATEST itself
                 *global-context*)))
      (destructuring-bind (&key number-of-tests-run
                                number-of-assertions
                                number-of-failures
                                number-of-failed-assertions
                                number-of-unexpected-errors
                                number-of-expected-failures
                            &allow-other-keys)
          (extract-test-run-statistics run)
        ;; Remember that the suite itself counts as a test and so it's
        ;; an assertion. FIXME: this is confusing as hell
        (is (= 4 number-of-assertions))
        (is (= 4 number-of-tests-run))
        (is (= 3 number-of-failures))
        (is (= 1 number-of-failed-assertions))
        (is (= 2 number-of-unexpected-errors))
        (is (= 0 number-of-expected-failures))))))
  
#+nil
(defun seconds (hours-and-minutes)
  (+ (* 3600 (first hours-and-minutes))
     (* 60 (second hours-and-minutes))))
#+nil
(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (/ (rem seconds 3600) 60)))
