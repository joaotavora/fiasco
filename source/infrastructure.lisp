;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

;; Warning: setf-ing these variables in not a smart idea because other systems may rely on their default value.
;; It's smarter to rebind them in an :around method from your .asd or shadow stefil:deftest with your own that sets
;; their keyword counterparts.
(defvar *suite*)
(defvar *root-suite*)
(defvar *print-test-run-progress* t)
(defvar *compile-tests-before-run* nil)
(defvar *compile-tests-with-debug-level* nil)
(defvar *test-progress-print-right-margin* 80)
(defvar *debug-on-unexpected-error* t)
(defvar *debug-on-assertion-failure* t)
(defvar *test-result-history* '())
(defvar *last-test-result* nil)
(defvar *failures-and-errors-are-expected* nil)

(defvar *tests* (make-hash-table :test 'eql)) ; this is not thread-safe, but...

(defmacro without-debugging (&body body)
  `(bind ((*debug-on-unexpected-error* nil)
          (*debug-on-assertion-failure* nil))
    ,@body))

;;;;;;
;;; conditions

(define-condition test-related-condition ()
  ((test :initform nil :accessor test-of :initarg :test)))

(define-condition test-style-warning (style-warning test-related-condition simple-warning)
  ())

;; assertion-failed is not a serious-condition by design, so that handler-bind's can have bindings for serious-condition
;; without being concerned about failed assertions signalled using #'error to bring up the debugger.
(define-condition assertion-failed (test-related-condition)
  ((failure-description :accessor failure-description-of :initarg :failure-description))
  (:report (lambda (c stream)
             (format stream "Test assertion failed:~%~%")
             (describe (failure-description-of c) stream))))

;;;;;;
;;; some classes

#+nil
(hu.dwim.defclass-star:defclass* testable ()
  ((name :type symbol)
   (parent nil :initarg nil :type (or null testable))
   (children (make-hash-table) :documentation "A mapping from testable names to testables")
   (auto-call t :type boolean :documentation "Controls whether to automatically call this test when its parent suite is invoked. Enabled by default.")))

(defclass testable ()
  ((name :accessor name-of :initarg :name :type symbol)
   (parent :initform nil :accessor parent-of :type (or null testable))
   (children :initform (make-hash-table) :accessor children-of :initarg :children :documentation "A mapping from testable names to testables")
   (auto-call :initform t :accessor auto-call-p :initarg :auto-call :type boolean :documentation "Controls whether to automatically call this test when its parent suite is invoked. Enabled by default.")))

(defprint-object (self testable :identity nil :type nil)
  (format t "test ~S" (name-of self))
  (bind ((children (count-tests self)))
    (unless (zerop children)
      (format t " :tests ~S" children))))

(defmethod shared-initialize :after ((self testable) slot-names
                                     &key (in (or (parent-of self)
                                                  (and (boundp '*suite*)
                                                       *suite*))))
  (assert (name-of self))
  (setf (find-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (setf (parent-of self) in))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (when (and new-parent
             (symbol-package (name-of self)) ; leave alone tests named by uninterned symbols
             (not (eq new-parent *root-suite*))
             (not (eq (symbol-package (name-of new-parent))
                      (symbol-package (name-of self)))))
    (warn 'test-style-warning :test self
          :format-control "Adding test ~S under parent ~S which is in a different package"
          :format-arguments (list (name-of self) (name-of new-parent))))
  (bind ((old-parent (parent-of self)))
    (when old-parent
      (remhash (name-of self) (children-of old-parent)))
    (prog1
        (call-next-method)
      (when new-parent
        (setf (gethash (name-of self) (children-of new-parent)) self)))))

(defgeneric count-tests (testable)
  (:method ((self testable))
           (+ (hash-table-count (children-of self))
              (loop
                :for child :being :the :hash-values :of (children-of self)
                :summing (count-tests child)))))

#+nil
(hu.dwim.defclass-star:defclass* failure-description ()
  ((test-context-backtrace)
   (progress-char #\X :allocation :class)
   (expected *failures-and-errors-are-expected* :type boolean)))

(defclass failure-description ()
  ((test-context-backtrace :accessor test-context-backtrace-of :initarg :test-context-backtrace)
   (progress-char :initform #\X :accessor progress-char-of :initarg :progress-char :allocation :class)
   (expected :initform *failures-and-errors-are-expected* :accessor expected-p :initarg :expected :type boolean)))

#+nil
(hu.dwim.defclass-star:defclass* failed-assertion (failure-description)
  ((form)
   (format-control)
   (format-arguments)))

(defclass failed-assertion (failure-description)
  ((form :accessor form-of :initarg :form)
   (format-control :accessor format-control-of :initarg :format-control)
   (format-arguments :initform nil :accessor format-arguments-of :initarg :format-arguments)))

(defmethod describe-object ((self failed-assertion) stream)
  (let ((*print-circle* nil))
    (apply #'format stream (format-control-of self) (format-arguments-of self))))

(defprint-object (self failed-assertion :identity nil :type nil)
  (format t "failure ~S backtrace: ~{~A~^,~}"
          (form-of self)
          (mapcar (compose #'name-of #'test-of)
                  (test-context-backtrace-of self))))

#+nil
(hu.dwim.defclass-star:defclass* failure-description/condition (failure-description)
  ((form)
   (condition)))

(defclass failure-description/condition (failure-description)
  ((form :accessor form-of :initarg :form)
   (condition :accessor condition-of :initarg :condition)))

(defclass missing-condition (failure-description/condition)
  ())

(defmethod describe-object ((self missing-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S failed to signal condition ~S" (form-of self) (condition-of self))))

(defclass extra-condition (failure-description/condition)
  ())

(defmethod describe-object ((self extra-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S signaled an unwanted condition ~S" (form-of self) (condition-of self))))

#+nil
(hu.dwim.defclass-star:defclass* unexpected-error (failure-description)
  ((condition)
   (progress-char #\E :allocation :class)))

(defclass unexpected-error (failure-description)
  ((condition :accessor condition-of :initarg :condition)
   (progress-char :initform #\E :accessor progress-char-of :initarg :progress-char :allocation :class)))

(defprint-object (self unexpected-error :identity nil :type nil)
  (format t "error ~{~A~^,~}: ~S"
          (mapcar (compose #'name-of #'test-of)
                  (reverse (test-context-backtrace-of self)))
          (condition-of self)))


;;;;;;
;;; test repository

(defun find-test (name &key (otherwise :error))
  (bind (((:values test found-p) (if (typep name 'testable)
                                    (values name t)
                                    (gethash name *tests*))))
    (when (and (not found-p)
               otherwise)
      (etypecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf find-test) (new-value key)
  (if new-value
      (progn
        (when (gethash key *tests*)
          (warn 'test-style-warning
                :format-control "redefining test ~A"
                :format-arguments (list (let ((*package* #.(find-package "KEYWORD")))
                                          (format nil "~S" key)))))
        (setf (gethash key *tests*) new-value))
      (delete-test key)))

(defun delete-test (name &rest args)
  (bind ((test (apply #'find-test name args))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite and has no parent")
      (remhash name *tests*)
      (setf (parent-of test) nil)
      (fmakunbound (name-of test))
      (loop
        :for subtest :being :the :hash-values :of (children-of test)
        :do (delete-test (name-of subtest)))
      (when (eq *suite* test)
        (setf *suite* parent)))
    test))


;;;;;;
;;; the real thing

#+nil
(define-dynamic-context* global-context
  ((failure-descriptions (make-array 8 :adjustable t :fill-pointer 0))
   (assertion-count 0)
   (progress-char-count 0)
   (print-test-run-progress-p *print-test-run-progress* :type boolean)
   (debug-on-unexpected-error-p *debug-on-unexpected-error* :type boolean)
   (debug-on-assertion-failure-p *debug-on-assertion-failure* :type boolean)
   (toplevel-context nil)
   (current-test nil)
   (run-tests (make-hash-table) :documentation "test -> context mapping")
   (run-fixtures (make-hash-table))
   (test-lambdas (make-hash-table) :documentation "test -> compiled test lambda mapping for this test run")))

(define-dynamic-context global-context
  ((failure-descriptions :initform (make-array 8 :adjustable t :fill-pointer 0) :accessor failure-descriptions-of :initarg :failure-descriptions)
   (assertion-count :initform 0 :accessor assertion-count-of :initarg :assertion-count)
   (progress-char-count :initform 0 :accessor progress-char-count-of :initarg :progress-char-count)
   (print-test-run-progress-p :initform *print-test-run-progress* :accessor print-test-run-progress-p :initarg :print-test-run-progress-p :type boolean)
   (debug-on-unexpected-error-p :initform *debug-on-unexpected-error* :accessor debug-on-unexpected-error-p :initarg :debug-on-unexpected-error-p :type boolean)
   (debug-on-assertion-failure-p :initform *debug-on-assertion-failure* :accessor debug-on-assertion-failure-p :initarg :debug-on-assertion-failure-p :type boolean)
   (toplevel-context :initform nil :accessor toplevel-context-of :initarg :toplevel-context)
   (current-test :initform nil :accessor current-test-of :initarg :current-test)
   (run-tests :initform (make-hash-table) :accessor run-tests-of :initarg :run-tests :documentation "test -> context mapping")
   (run-fixtures :initform (make-hash-table) :accessor run-fixtures-of :initarg :run-fixtures)
   (test-lambdas :initform (make-hash-table) :accessor test-lambdas-of :initarg :test-lambdas :documentation "test -> compiled test lambda mapping for this test run")))

(defprint-object (self global-context :identity nil :type nil)
  (let* ((failure-descriptions (failure-descriptions-of self))
         (total-failure-count (length failure-descriptions))
         (failed-assertion-count (count-if (of-type '(or failed-assertion missing-condition extra-condition)) failure-descriptions))
         (unexpected-error-count (count-if (of-type 'unexpected-error) failure-descriptions))
         (expected-count (count-if 'expected-p (failure-descriptions-of self))))
    (format t "test-run: ~A tests, ~A assertions, ~A failures in ~A sec~[~:; (~A failed assertions, ~A errors, ~A expected)~]"
            (hash-table-count (run-tests-of self))
            (assertion-count-of self)
            total-failure-count
            (bind ((toplevel-context (toplevel-context-of self))
                   (real-time-spent-in-seconds
                    (when toplevel-context
                      (real-time-spent-in-seconds toplevel-context))))
              (if (and toplevel-context
                       real-time-spent-in-seconds)
                  real-time-spent-in-seconds
                  "?"))
            total-failure-count ; index in the ~[] conditional
            failed-assertion-count
            unexpected-error-count
            (cond ((= expected-count total-failure-count)
                   "all")
                  ((zerop expected-count)
                   "none")
                  (t expected-count)))))

(defmacro without-test-progress-printing (&body body)
  (with-unique-names (old-state)
    `(let ((,old-state (print-test-run-progress-p *global-context*)))
      (unwind-protect
           (progn
             (setf (print-test-run-progress-p *global-context*) nil)
             ,@body)
        (setf (print-test-run-progress-p *global-context*) ,old-state)))))

(defmacro with-toplevel-restarts (&body body)
  (with-unique-names (with-toplevel-restarts/body)
    `(block restart-wrapper
       (restart-bind
           ((continue-without-debugging
             (lambda ()
               (setf (debug-on-unexpected-error-p *global-context*) nil)
               (setf (debug-on-assertion-failure-p *global-context*) nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~@<Turn off debugging for this test session and invoke the first CONTINUE restart~@:>")))
            (continue-without-debugging-errors
             (lambda ()
               (setf (debug-on-unexpected-error-p *global-context*) nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~@<Do not stop at unexpected errors for the rest of this test session and continue by invoking the first CONTINUE restart~@:>")))
            (continue-without-debugging-assertions
             (lambda ()
               (setf (debug-on-assertion-failure-p *global-context*) nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~@<Do not stop at failed assertions for the rest of this test session and continue by invoking the first CONTINUE restart~@:>")))
            (abort-testing
             (lambda ()
               (return-from restart-wrapper))
              :report-function (lambda (stream)
                                 (format stream "~@<Abort the entire test session~@:>"))))
         (flet ((,with-toplevel-restarts/body ()
                  ,@body))
           (if (fboundp 'call-with-sldb-quit-restart)
               (funcall 'call-with-sldb-quit-restart #',with-toplevel-restarts/body (find-restart 'abort-testing))
               (,with-toplevel-restarts/body)))))))

(defun test-was-run-p (test)
  (declare (type testable test))
  (and (gethash test (run-tests-of *global-context*))
       (not (eq (current-test-of *global-context*) test))))

(defun register-test-being-run (test)
  (declare (type testable test))
  (setf (gethash test (run-tests-of *global-context*)) (current-context))
  (setf (current-test-of *global-context*) test))

#+nil
(define-dynamic-context* context
  ((test)
   (internal-realtime-spent-with-test nil)
   (test-arguments)
   (number-of-added-failure-descriptions 0))
  :chain-parents t)

(define-dynamic-context context
  ((test :accessor test-of :initarg :test)
   (internal-realtime-spent-with-test :initform nil :accessor internal-realtime-spent-with-test-of :initarg :internal-realtime-spent-with-test)
   (test-arguments :accessor test-arguments-of :initarg :test-arguments)
   (number-of-added-failure-descriptions :initform 0 :accessor number-of-added-failure-descriptions-of :initarg :number-of-added-failure-descriptions))
  :chain-parents t)

(defprint-object (self context :identity nil :type nil)
  (format t "test-run ~@<(~S~{~^ ~S~})~@:>"
          (name-of (test-of self))
          (bind ((result (lambda-list-to-funcall-list (lambda-list-of (test-of self)))))
            (mapcar (lambda (arg-cell)
                      (setf result (substitute (cdr arg-cell) (car arg-cell) result :test #'eq)))
                    (test-arguments-of self))
            result)))

(defgeneric real-time-spent-in-seconds (context)
  (:method ((self context))
    (bind ((time-spent (internal-realtime-spent-with-test-of self)))
      (when time-spent
        (coerce (/ time-spent
                   internal-time-units-per-second)
                'float)))))

(defmacro run-failed-tests (&optional (test-result-place '*last-test-result*))
  `(with-new-global-context ()
     (if (> (length (failure-descriptions-of ,test-result-place)) 0)
         (progn
           (%run-failed-tests ,test-result-place)
           (push *global-context* *test-result-history*)
           (setf *last-test-result* *global-context*)
           (setf ,test-result-place *global-context*))
         (progn
           (warn "There are no failed tests in ~S" ',test-result-place)
           (values)))))

(defun %run-failed-tests (global-context-to-be-processed)
  (warn "Re-running failed tests without considering their dynamic environment, which may affect their behaviour!")
  (with-toplevel-restarts
    (loop
      :for failure :across (failure-descriptions-of global-context-to-be-processed)
      :for context = (elt (test-context-backtrace-of failure) 0)
      :do (apply (name-of (test-of context)) (mapcar #'cdr (test-arguments-of context))))
    (when (print-test-run-progress-p *global-context*)
      (terpri *debug-io*))))

(defmacro runs-without-failure? (&body body)
  (with-unique-names (old-failure-count)
    `(bind ((,old-failure-count (length (failure-descriptions-of *global-context*))))
       ,@body
       (= ,old-failure-count (length (failure-descriptions-of *global-context*))))))

(defmacro with-expected-failures* (condition &body body)
  "Any failure inside the dynamic extent of this block is registered as an expected failure when CONDITION evaluates to true."
  (with-unique-names (with-expected-failures-block)
    `(bind ((*failures-and-errors-are-expected* ,condition))
       (block ,with-expected-failures-block
         (restart-case
             (handler-bind ((serious-condition
                             ;; TODO comment on why it's needed here...
                             (lambda (error)
                               (record-unexpected-error error)
                               (return-from ,with-expected-failures-block (values)))))
               ,@body)
           (continue ()
             :report (lambda (stream)
                       (format stream "~@<Skip the rest of the innermost WITH-EXPECTED-FAILURES body and continue by returning (values)~@:>"))
             (values)))))))

(defmacro with-expected-failures (&body body)
  "Any failure inside the dynamic extent of this block is registered as an expected failure."
  `(with-expected-failures* t ,@body))


;;;;;;
;;; some utils

(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(defun parse-lambda-list (lambda-list visitor &key macro)
  ;; TODO delme, and use alexandria:parse-ordinary-lambda-list
  (declare (optimize (speed 3))
           (type list lambda-list)
           (type (or symbol function) visitor))
  (let ((args lambda-list))
    (labels
        ((fail ()
           (illegal-lambda-list lambda-list))
         (ensure-list (list)
           (if (listp list)
               list
               (list list)))
         (process-&whole ()
           (assert (eq (first args) '&whole))
           (pop args)
           (unless macro
             (fail))
           (let ((whole (pop args)))
             (unless whole
               (fail))
             (funcall visitor '&whole whole whole))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &aux &allow-other-keys) (fail))
             (t             (process-required))))
         (process-&body ()
           (assert (eq (first args) '&body))
           (pop args)
           (unless macro
             (fail))
           (let ((body (pop args)))
             (unless (null args)
               (fail))
             (unless body
               (fail))
             (funcall visitor '&body body body)))
         (process-&environment ()
           (assert (eq (first args) '&environment))
           (pop args)
           (unless macro
             (fail))
           (let ((env (pop args)))
             (unless env
               (fail))
             (funcall visitor '&environment env env))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&aux          (process-&aux))
             ((&whole &environment &allow-other-keys) (fail))
             (t             (process-required))))
         (process-required ()
           (unless args
             (done))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &allow-other-keys) (fail))
             (&aux          (entering-&aux))
             (t
              (let ((arg (pop args)))
                (funcall visitor nil arg arg))
              (process-required))))
         (process-&rest ()
           (assert (eq (first args) '&rest))
           (pop args)
           (let ((rest (pop args)))
             (unless rest
               (fail))
             (funcall visitor '&rest rest rest))
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&environment       (process-&environment))
             ((&whole &optional &rest &body &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t                  (fail))))
         (entering-&optional ()
           (assert (eq (first args) '&optional))
           (pop args)
           (process-&optional))
         (process-&optional ()
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&rest              (process-&rest))
             (&body              (process-&body))
             ((&whole &optional &environment &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name (first arg))
                     (default (second arg)))
                (funcall visitor '&optional name arg nil default))
              (process-&optional))))
         (entering-&key ()
           (assert (eq (first args) '&key))
           (pop args)
           (process-&key))
         (process-&key ()
           (unless args
             (done))
           (case (first args)
             (&allow-other-keys       (funcall visitor '&allow-other-keys nil nil))
             ((&key &optional &whole &environment &body) (fail))
             (&aux                    (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name-part (first arg))
                     (default (second arg))
                     (external-name (if (consp name-part)
                                        (progn
                                          (unless (= (length name-part) 2)
                                            (illegal-lambda-list lambda-list))
                                          (first name-part))
                                        (intern (symbol-name name-part) #.(find-package "KEYWORD"))))
                     (local-name (if (consp name-part)
                                     (second name-part)
                                     name-part)))
                (funcall visitor '&key local-name arg external-name default))
              (process-&key))))
         (entering-&aux ()
           (assert (eq (first args) '&aux))
           (pop args)
           (process-&aux))
         (process-&aux ()
           (unless args
             (done))
           (case (first args)
             ((&whole &optional &key &environment &allow-other-keys &aux &body) (fail))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&aux (first arg) arg))
              (process-&aux))))
         (done ()
           (return-from parse-lambda-list (values))))
      (when args
        (case (first args)
          (&whole (process-&whole))
          (t      (process-required)))))))

(defun lambda-list-to-funcall-list (args)
  (bind (((:values requireds optionals rest keywords) (parse-ordinary-lambda-list args)))
    (values (append requireds
                    (loop
                      :for entry :in optionals
                      :collect (first entry))
                    (loop
                      :for entry :in keywords
                      :appending (list (first (first entry)) (second (first entry)))))
            rest)))

#+nil ; not used
(defun lambda-list-to-lambda-list-with-quoted-defaults (args)
  (bind (((:values requireds optionals rest keywords allow-other-keys?) (parse-ordinary-lambda-list args)))
    (values `(,@requireds
              ,@(when optionals (cons '&optional optionals))
              ,@(if rest
                    `(&rest ,rest)
                    (when keywords (cons '&key keywords)))
              ,@(when (and allow-other-keys? (not rest))
                  (list '&allow-other-keys)))
            rest)))

(defun lambda-list-to-funcall-expression (function args)
  (multiple-value-bind (arg-list rest-variable)
      (lambda-list-to-funcall-list args)
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-value-list-expression (args)
  ;; TODO use alexandria:parse-ordinary-lambda-list
  `(list ,@(let ((result (list)))
             (parse-lambda-list args
                                (lambda (kind name entry &optional external-name default)
                                  (declare (ignore entry external-name default))
                                  (case kind
                                    (&allow-other-keys)
                                    (t (push `(cons ',name ,name) result)))))
             (nreverse result))))

(defun lambda-list-to-variable-name-list (args &key macro include-specials)
  ;; TODO use alexandria:parse-ordinary-lambda-list
  (let ((result (list))
        (rest-variable-name nil)
        (whole-variable-name nil)
        (env-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry external-name default))
                         (case kind
                           (&allow-other-keys )
                           (&environment      (setf env-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (&whole            (setf whole-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           ((&rest &body)     (setf rest-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (t                 (push name result))))
                       :macro macro)
    (values (nreverse result)
            rest-variable-name
            whole-variable-name
            env-variable-name)))

(defun funcall-test-with-feedback-message (test-function &rest args)
  "Run the given test non-interactively and print the results to *standard-output*.
This function is ideal for ASDF:TEST-OP's."
  (bind ((result (without-debugging (apply test-function args))))
    (let ((*package* (find-package :common-lisp)))
      (format *standard-output*
"The result of ~S is:

  ~A

For more details run it from the REPL and use the customized Slime inspector
to inspect the results (ASDF eats up the return values). Some inspector
features may only be available when using the Slime branch at
darcs get --lazy http://dwim.hu/darcs/hu.dwim.slime
but the official Slime should also work fine.~%"
              test-function result))))
