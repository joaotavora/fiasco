;;;; mfiasco.lisp --- Small but complete test framework
(defpackage :micro-fiasco
  (:use cl) (:nicknames :mfiasco)
  (:export #:is #:fail #:signals #:skip
           #:deftest #:*interactive-tests* #:run-package-tests
           #:expected-failure
           #:defsetup
           #:defteardown
           #:*test-output*))
(in-package :mfiasco)

(defvar *total-assertions*)        (defvar *failed-assertions*)
(defparameter *interactive-tests* t)
(defparameter *suppress-fixtures* nil)
(defparameter *package-setup* (make-hash-table))
(defparameter *package-teardown* (make-hash-table))
(defparameter *test-output* *standard-output*)

(define-condition test-assertion-failed (serious-condition) ())

(defun fail (&optional message &rest message-args)
  "Fail test.  Register failure using MESSAGE and MESSAGE-ARGS."
  (with-simple-restart (continue "Continue testing")
    (error 'test-assertion-failed :format-control (or message "Failed")
                                  :format-arguments message-args)))

(defun skip () "Skip test." (invoke-restart 'skip-rest-of-test))

(defmacro is (assertion &optional message &rest message-args)
  "Assert ASSERTION evaluates to true.
Otherwise register the failure using MESSAGE and MESSAGE-ARGS."
  `(let ((probe ,assertion))
     (cond
       ((boundp '*total-assertions*)
        (incf *total-assertions*)
        (unless probe (fail ,(or message (format nil "~a failed" assertion))
                            ,@message-args)))
       (t probe))))

(defmacro signals ((type &optional message &rest message-args) &rest body)
  "Assert BODY signals a condition of TYPE.
Otherwise register the failure using MESSAGE and MESSAGE-ARGS."
  `(block signalled
     (handler-bind ((condition
                      (lambda (c) (when (typep c ',type)
                                    (return-from signalled)))))
       (if (boundp '*total-assertions*) (incf *total-assertions*))
       ,@body
       (fail ,(or message (format nil "~S failed to signal ~a" body type))
             ,@message-args))))

(defun setup-fixture (package map-sym fixture)
  (let ((mapping (symbol-value map-sym))
        (p (find-package package))
        (fn (lambda ()
              (format *test-output*
                      "~&Running ~a function for ~a~%" map-sym package)
              (funcall fixture))))
    (if fn (setf (gethash p mapping) fn) (remhash p mapping))))

(defmacro defsetup (package &body body)
  "Define tests in PACKAGE to need the setup of BODY."
  (unless (find-package package) (warn "Package ~a not know yet"))
  `(setup-fixture ,package '*package-setup* ,(and body `(lambda () ,@body))))

(defmacro defteardown (package &body body)
  "Define tests in PACKAGE to need the teardown of BODY."
  (unless (find-package package) (warn "Package ~a not know yet"))
  `(setup-fixture ,package '*package-teardown* ,(and body `(lambda () ,@body))))

(defun find-fixtures (package mapping)
  (loop with toexplore = (list (find-package package))
        for probe = (pop toexplore)
        while probe
        collect probe into visited
        when (gethash probe mapping) collect it
        do (loop for p in (package-use-list probe)
                 unless (member p visited)
                   do (pushnew p toexplore))))

(defmacro deftest (name args &body body)
  "Define a test named NAME with ARGS and BODY.
It's just like a function, but the return value is discarded.  The
return value is instead replaced by a values list of (SUCCESS
DETAILS), where DETAILS is a plist.  You can run the test by caling
the function."
  (setf (get name 'auto-run) (null args)
        (get name 'expected-failure) nil)
  `(defun ,name ,args
     ,@(when (stringp (car body)) (list (pop body)))
     ,@(when (and (consp (car body))
                  (eq 'declare (caar body)))
         (loop for l on (car body)
               for (k . v) = (cadr l)
               when (member k '(expected-failure))
                 do (setf (get name k) v)
                    (pop (cdr l)))
         (list (pop body)))
     (let ((*total-assertions* 0) (*failed-assertions* 0)
           test-start fixture-start
           (setup (unless *suppress-fixtures*
                    (find-fixtures (symbol-package ',name) *package-setup*)))
           (teardown (unless *suppress-fixtures*
                       (find-fixtures (symbol-package ',name) *package-teardown*))))
       (flet ((report () `(:total-assertions ,*total-assertions*
                           :failed-assertions ,*failed-assertions*
                           :test-time ,(- (get-internal-real-time) test-start)
                           ,@(and fixture-start
                                  `(:fixture-time ,(- test-start
                                                      fixture-start))))))
         (restart-case
             (handler-bind ((test-assertion-failed
                              (lambda (c) (declare (ignore c))
                                (incf *failed-assertions*)
                                (unless *interactive-tests* (continue)))))
               (unwind-protect
                    (progn
                      (when setup
                        (setq fixture-start (get-internal-real-time))
                        (mapc #'funcall setup))
                      (setq test-start (get-internal-real-time))
                      ,@body
                      (values (= *failed-assertions* 0) (report)))
                 (when teardown (mapc #'funcall teardown))))
           (skip-rest-of-test (&key error)
             :report "skip the rest of the test"
             (values (unless error 'skipped)
                     `(,@(if error `(:error-condition ,error) `(:skipped t))
                       ,@(report)))))))))

(defun run-package-tests (&key ((:interactive *interactive-tests*) nil)
                            (package *package* package-supplied-p)
                            (packages nil)
                            (kill-lisp nil))
  "Run tests in PACKAGE.  If INTERACTIVE, enter debugger on failures.
PACKAGE defaults to current package *PACKAGE*.  Alternatively, if
PACKAGES is non-nil, run tests of multiple packages instead.  PACKAGES
can be a list of packages, a predicate for selecting packages, or a
string designating a regexp to filter packages by name.

If KILL-LISP is non-nil, kill the listp, exiting with a non-zero code
if some test failed.

Returns two values: 1. An alist ((TESTNAME . PLIST) ...) describing
what went wrong with tests, if anything.  2. A boolean indicating if
there were unexpected results."
  (assert (not (and package-supplied-p packages))
          nil "Supply either :PACKAGE or :PACKAGES")
  (let (report (start (get-internal-real-time))
        (total 0) unexpected
        (packages
          (cond ((not (listp packages))
                 (remove-if-not
                  (etypecase packages
                    (function packages)
                    (string
                     (lambda (p) (cl-ppcre:scan
                                  (cl-ppcre:create-scanner
                                   packages :case-insensitive-mode t)
                                  (package-name p)))))
                  (list-all-packages)))
                (packages
                 (mapcar #'find-package packages))
                (t
                 (list (find-package package))))))
    (labels ((run (s)
               (let* ((expected-failure (get s 'expected-failure)) str errored)
                 (multiple-value-bind (success details)
                     (handler-bind ((error (lambda (e)
                                             (setq errored t)
                                             (unless *interactive-tests*
                                               (invoke-restart
                                                'skip-rest-of-test :error e)))))
                       (incf total)
                       (funcall s))
                   (format *test-output*
                           "  ~a ~v@{~C~:1*~}~a~2,2$s ~a~%"
                           (setq str (format nil "~s" s))
                           (- (min (or *print-right-margin* 80) 80)
                              16 (length str) 1)
                           #\.
                           (/ (getf details :test-time)
                              internal-time-units-per-second)
                           (cond ((eq success 'skipped)                "[skip]")
                                 ((and success expected-failure)
                                  (push s unexpected)                  "[YAY?]")
                                 (success                              "[ ok ]")
                                 ((and expected-failure (not errored)) "[fair]")
                                 (t
                                  (push (cons s details) report)
                                  (push s unexpected)
                                  (if errored "[GOOF]" "[FAIL]")))))))
             (run-for-package (p &aux syms (*suppress-fixtures* t)
                                   (start (get-internal-real-time)))
               (do-symbols (s p)
                 (when (and (eq p (symbol-package s))
                            (get s 'auto-run) (fboundp s))
                   (push s syms)))
               (format *test-output* "~&There are ~a tests to run in in ~a~%"
                       (length syms) (package-name p))
               (mapc #'funcall (find-fixtures p *package-setup*))
               (format *test-output* "~&Running tests in ~a (~2,2$s of setup)~%"
                       (package-name p)
                       (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second))
               (unwind-protect
                    (mapc #'run (sort syms #'string-lessp :key #'symbol-name))
                 (mapc #'funcall (find-fixtures p *package-teardown*)))))
      (mapc #'run-for-package packages)
      (format *test-output* "~&Ran ~a tests in ~,2Fs, ~a unexpected results~%"
              total
              (/ (- (get-internal-real-time) start) internal-time-units-per-second)
              (length unexpected))
      (when unexpected
        (format *test-output* "~&Tests with unexpected results: ~a~%" unexpected))
      (if kill-lisp
          (uiop:quit (if unexpected -1 0))
          (values report unexpected)))))

(defpackage :mfiasco-self-testz (:use :cl :mfiasco))
(in-package :mfiasco-self-testz)

(defsetup :mfiasco-self-testz
  (format t "Setting up stuff~%")
  (sleep 0.5))

(deftest okay-test       () (is t))
(deftest failed-test     () (is nil))
(deftest goofed-up       () (error "bla"))
(deftest expected        () (declare (expected-failure t)) (is nil))
(deftest unexpected-pass () (declare (expected-failure t)) (is t))
(deftest does-signal     () (signals (error) (is t) (error "oh no")))
(deftest fails-to-signal () (signals (error) (is t) (warn "oh no")))
(deftest skips-itself    () (skip) (error "on no"))
(deftest is-slow         () (sleep 0.2) (is t))
