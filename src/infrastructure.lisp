;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :fiasco)


;;; Special variables
;;;
;; Warning: setf-ing these variables in not a smart idea because other
;; systems may rely on their default value.  It's smarter to rebind
;; them in an :around method from your .asd or shadow fiasco:deftest
;; with your own that sets their keyword counterparts.
(defvar *suite*)
(defvar *root-suite*)
(defvar *package-bound-suites* (make-hash-table))
(defvar *print-test-run-progress* t)
(defvar *test-progress-print-right-margin* 80)
(defvar *debug-on-unexpected-error* t)
(defvar *debug-on-assertion-failure* t)
(defvar *test-result-history* '())
(defvar *last-test-result* nil)
(defvar *always-show-failed-sexp* nil)
(defvar *warn-about-test-redefinitions* nil)

(defvar *test-run-standard-output* '*standard-output*
  "*STANDARD-OUTPUT* is bound to (eval *test-run-standard-output*) at
the toplevel entry point to any test.")

(defvar *tests* (make-hash-table :test 'eql)) ; this is not thread-safe, but...

(defmacro without-debugging (&body body)
  `(let* ((*debug-on-unexpected-error* nil)
          (*debug-on-assertion-failure* nil))
    ,@body))


;;; Testable class
;;; 
(defclass testable ()
  ((name :accessor name-of :initarg :name :type symbol)
   (parent :initform nil :accessor parent-of :type (or null testable))
   (children :initform (make-hash-table) :accessor children-of
             :initarg :children
             :documentation "A mapping from testable names to testables")
   (auto-call :initform t :accessor auto-call? :initarg :auto-call :type boolean
              :documentation "Controls whether to automatically call
this test when its parent suite is invoked. Enabled by default.")))

(defprint-object (self testable :identity nil :type nil)
  (format t "test ~S" (name-of self))
  (let* ((children (count-tests self)))
    (unless (zerop children)
      (format t " :tests ~S" children))))

(defvar *ignore-package-suite-mismatch* nil)

(defmethod shared-initialize :after
    ((self testable) slot-names &key (in (or (parent-of self)
                                             (find-suite-for-package *package*)
                                             (and (boundp '*suite*)
                                                  *suite*))
                                         in-supplied-p))
  (declare (ignore slot-names))
  (assert (name-of self))
  (setf (find-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (let ((*ignore-package-suite-mismatch* in-supplied-p))
    (setf (parent-of self) in)))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (when (and new-parent
             (symbol-package (name-of self)) ; leave alone tests named
                                        ; by uninterned symbols
             (not (eq new-parent *root-suite*))
             (not (eq (symbol-package (name-of new-parent))
                      (symbol-package (name-of self))))
             (not *ignore-package-suite-mismatch*)
             (not (gethash (package-of self) *package-bound-suites*)))
    (warn 'test-style-warning
          :test self
          :format-control "Adding test under parent ~S which is in a~
different package (parent: ~A, child: ~A). Maybe a~
missing (in-root-suite)?"
          :format-arguments (list new-parent (symbol-package
                                              (name-of new-parent))
                                  (symbol-package (name-of self)))))
  (let* ((old-parent (parent-of self)))
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


;;; Conditions
;;; 
(define-condition test-related-condition ()
  ((test :initform nil :accessor test-of :initarg :test)))

(define-condition test-started (test-related-condition) ())

(define-condition test-style-warning
    (style-warning test-related-condition simple-warning)
  ())

(define-condition failure (test-related-condition)
  ((progress-char :initform #\X :accessor progress-char-of
                  :initarg :progress-char :allocation :class)))

(define-condition assertion-failed (failure)
  ((form :accessor form-of :initarg :form)
   (format-control :accessor format-control-of :initarg :format-control)
   (format-arguments :initform nil :accessor format-arguments-of
                     :initarg :format-arguments)))

(defmethod describe-object ((self assertion-failed) stream)
  (let ((*print-circle* nil))
    (apply #'format stream (format-control-of self)
           (format-arguments-of self))))

(defprint-object (self assertion-failed :identity nil :type nil)
  (format t "failure ~S backtrace: ~{~A~^,~}"
          (form-of self)
          (mapcar (compose #'name-of #'test-of)
                  (reverse (list "TODO" "TODO")))))

(define-condition missing-condition (failure)
  ((expected-type :initarg :expected-type :accessor expected-type-of)))

(defmethod describe-object ((self missing-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S failed to signal a condition of type ~S" (form-of self)
            (expected-type-of self))))

(define-condition unwanted-condition (failure)
  ((expected-type :initarg :expected-type :accessor expected-type-of)
   (observed-condition :initarg :observed-condition :accessor observed-condition-of)))

(defmethod describe-object ((self unwanted-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S signaled an unwanted condition ~S"
            (form-of self) (observed-condition-of self))))

(define-condition unexpected-error (failure)
  ((error :accessor error-of :initform (error "Must provide ~S" 'error)
          :initarg :error)
   (progress-char :initform #\E :accessor progress-char-of
                  :initarg :progress-char :allocation :class)))

(defmethod describe-object ((self unexpected-error) stream)
  (format stream "~a" (error-of self)))

(defprint-object (self unexpected-error :identity nil :type nil)
  (format t "error ~{~A~^,~}: ~S"
          (mapcar (compose #'name-of #'test-of)
                  (reverse (list)))
          (error-of self)))


;;; Test repository
;;; 
(defun find-test (name &key (otherwise :error))
  (multiple-value-bind (test found-p)
      (if (typep name 'testable)
          (values name t)
          (gethash name *tests*))
    (when (and (not found-p)
               otherwise)
      (typecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf find-test) (new-value key)
  (if new-value
      (progn
        (when (and *warn-about-test-redefinitions*
                   (gethash key *tests*))
          (warn 'test-style-warning
                :format-control "redefining test ~A"
                :format-arguments (list
                                   (let ((*package* #.(find-package "KEYWORD")))
                                          (format nil "~S" key)))))
        (setf (gethash key *tests*) new-value))
      (delete-test key)))

(defun delete-test (name &rest args)
  (let* ((test (apply #'find-test name args))
         (name (name-of test))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite~
and has no parent")
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

(defvar *global-context*)
(setf (documentation '*global-context* 'variable)
      "Status and progression of current top-level test run.
If this variable is unbound
")

(defclass assertion-recorder ()
  ((failure-descriptions :initform nil
                         :accessor failure-descriptions-of
                         :initarg :failure-descriptions)
   (assertion-count :initform 0 :accessor assertion-count-of
                    :initarg :assertion-count)))

(defclass global-context ()
  ((progress-char-count :initform 0 :accessor progress-char-count-of
                        :initarg :progress-char-count)
   (bindings :initarg :bindings :initform nil :accessor bindings-of)
   (toplevel-context :initform nil :accessor toplevel-context-of
                     :initarg :toplevel-context)
   (current-test :initform nil :accessor current-test-of :initarg :current-test)
   (run-tests :initform (make-hash-table)
              :accessor run-tests-of
              :initarg :run-tests
              :documentation "A mapping of TEST object to their~
associated CONTEXT objects.")
   (test-lambdas :initform (make-hash-table)
                 :accessor test-lambdas-of :initarg :test-lambdas
                 :documentation "A mapping of TEST objects to their~
compiled lambda functions")))

(defmethod failure-descriptions-of ((context global-context))
  (loop for context being the hash-values of (run-tests-of context)
        append (failure-descriptions-of context)))

(defmethod assertion-count-of ((context global-context))
  (loop for context being the hash-values of (run-tests-of context)
        count (assertion-count-of context)))

(defmacro with-new-global-context ((&rest initargs) &body forms)
  `(let* ((*global-context* (make-instance 'global-context ,@initargs))
          (*standard-output* (eval *test-run-standard-output*)))
     ,@forms))

(defun extract-test-run-statistics (global-context)
  (let* ((failure-descriptions (failure-descriptions-of global-context))
         (failed-assertion-count (count-if (of-type '(or
                                                      assertion-failed
                                                      missing-condition
                                                      extra-condition))
                                           failure-descriptions))
         (unexpected-error-count (count-if (of-type 'unexpected-error)
                                           failure-descriptions))
         (expected-count 0;; (count-if 'expected-p failure-descriptions)
                         ))
    (list :number-of-tests-run (hash-table-count (run-tests-of global-context))
          :number-of-assertions (assertion-count-of global-context)
          :number-of-failures (length failure-descriptions)
          :number-of-expected-failures expected-count
          :number-of-failed-assertions failed-assertion-count
          :number-of-unexpected-errors unexpected-error-count)))

(defprint-object (self global-context :identity nil :type nil)
  (destructuring-bind (&key number-of-tests-run
                            number-of-assertions
                            number-of-failures
                            number-of-failed-assertions
                            number-of-unexpected-errors
                            number-of-expected-failures
                            &allow-other-keys)
      (extract-test-run-statistics self)
    (format t "test-run: ~A test~:P, ~A assertion~:P, ~A failure~:P in~
 ~A sec~[~:; (~A failed assertion~:P, ~A error~:P, ~A expected)~]"
            number-of-tests-run
            number-of-assertions
            number-of-failures
            (let* ((toplevel-context (toplevel-context-of self))
                   (real-time-spent-in-seconds
                     (when toplevel-context
                       (real-time-spent-in-seconds toplevel-context))))
              (if (and toplevel-context
                       real-time-spent-in-seconds)
                  real-time-spent-in-seconds
                  "?"))
            number-of-failures ; index in the ~[] conditional
            number-of-failed-assertions
            number-of-unexpected-errors
            (cond ((= number-of-expected-failures number-of-failures)
                   "all")
                  ((zerop number-of-expected-failures)
                   "none")
                  (t number-of-expected-failures)))))

(defmacro without-test-progress-printing (&body body)
  (with-unique-names (old-state)
    `(let ((,old-state *print-test-run-progress*))
      (unwind-protect
           (progn
             (setf *print-test-run-progress* nil)
             ,@body)
        (setf *print-test-run-progress* ,old-state)))))

(defmacro with-toplevel-restarts (&body body)
  (with-unique-names (with-toplevel-restarts/body)
    `(block restart-wrapper
       (restart-bind
           ((continue-without-debugging
             (lambda ()
               (setf *debug-on-unexpected-error* nil
                     *debug-on-assertion-failure* nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~
~@<Turn off debugging for this test session and invoke the first~
CONTINUE restart~@:>")))
            (continue-without-debugging-errors
             (lambda ()
               (setf *debug-on-unexpected-error* nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~
~@<Do not stop at unexpected errors for the rest of this test session~
and continue by invoking the first CONTINUE restart~@:>")))
            (continue-without-debugging-assertions
             (lambda ()
               (setf *debug-on-assertion-failure* nil)
               (continue))
              :report-function (lambda (stream)
                                 (format stream "~
~@<Do not stop at failed assertions for the rest of this test session~
and continue by invoking the first CONTINUE restart~@:>")))
            (abort-testing
             (lambda ()
               (return-from restart-wrapper))
              :report-function (lambda (stream)
                                 (format stream "~@<Abort the entire~
test session~@:>"))))
         (flet ((,with-toplevel-restarts/body ()
                  ,@body))
           (if (fboundp 'call-with-sldb-quit-restart)
               (funcall 'call-with-sldb-quit-restart
                        #',with-toplevel-restarts/body
                        (find-restart 'abort-testing))
               (,with-toplevel-restarts/body)))))))

(defun test-was-run-p (test)
  (declare (type testable test))
  (and (gethash test (run-tests-of *global-context*))
       (not (eq (current-test-of *global-context*) test))))

(defvar *context*)
(setf (documentation '*context* 'variable)
      "Status and progress info for a particular test run.")

(defclass context (assertion-recorder)
  ((test :accessor test-of :initarg :test)
   (internal-realtime-spent-with-test
    :initform nil
    :accessor internal-realtime-spent-with-test-of
    :initarg :internal-realtime-spent-with-test)
   (test-arguments :accessor test-arguments-of :initarg :test-arguments)
   (parent-context
    :initarg :parent-context :initform nil :accessor parent-context-of)))

(defprint-object (self context :identity nil :type nil)
  (format t "test-run ~@<(~S~{~^ ~S~})~@:>"
          (name-of (test-of self))
          (let* ((result (lambda-list-to-funcall-list
                          (lambda-list-of (test-of self)))))
            (mapcar (lambda (arg-cell)
                      (setf result (substitute (cdr arg-cell)
                                               (car arg-cell)
                                               result :test #'eq)))
                    (test-arguments-of self))
            result)))

(defgeneric real-time-spent-in-seconds (context)
  (:method ((self context))
    (let* ((time-spent (internal-realtime-spent-with-test-of self)))
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
  (warn "Re-running failed tests without considering their dynamic
environment, which may affect their behaviour!")
  (with-toplevel-restarts
    (loop
      :for failure in (failure-descriptions-of
                       global-context-to-be-processed)
      :do (apply (name-of (test-of failure))
                 (error "TODO!")))
    (when *print-test-run-progress*
      (terpri *debug-io*))))

(defmacro with-expected-failures* (&whole whole condition &body body)
  "Run BODY and registering failure conditions as expected failure iff
CONDITION."
  (with-unique-names (with-expected-failures-block starting-failure-count)
    `(let* ((*failures-and-errors-are-expected* ,condition)
            (,starting-failure-count
              (length (failure-descriptions-of *global-context*))))
       (block ,with-expected-failures-block
         (restart-case
             (handler-bind ((serious-condition
                             ;; TODO comment on why it's needed here...
                             (lambda (error)
                               (record-unexpected-error error)
                               (return-from ,with-expected-failures-block
                                 (values)))))
               (multiple-value-prog1
                   (progn ,@body)
                 (unless (< ,starting-failure-count
                            (length (failure-descriptions-of *global-context*)))
                   (warn "The following ~S block ran without any failures: ~S"
                         'with-expected-failures* ',whole))))
           (continue ()
             :report (lambda (stream)
                       (format stream "~
~@<Skip the rest of the innermost WITH-EXPECTED-FAILURES body and ~
continue by returning (values)~@:>"))
             (values)))))))

(defmacro with-expected-failures (&body body)
  "Run BODY registering failured conditions as expected failures."
  `(with-expected-failures* t ,@body))


;;;;;;
;;; some utils

(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(defun parse-lambda-list (lambda-list visitor &key macro)
  ;; TODO delme, and use alexandria:parse-ordinary-lambda-list
  (declare #+nil (optimize (speed 3))
           (type list lambda-list)
           (type (or symbol function) visitor))
  (let ((args lambda-list))
    (labels
        ((fail ()
           (illegal-lambda-list lambda-list))
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
             (&allow-other-keys (funcall visitor '&allow-other-keys nil nil))
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
                                        (intern (symbol-name name-part)
                                                #.(find-package "KEYWORD"))))
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
             ((&whole &optional &key &environment
                      &allow-other-keys &aux &body) (fail))
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
  (multiple-value-bind (requireds optionals rest keywords)
      (parse-ordinary-lambda-list args)
    (values (append requireds
                    (loop
                      :for entry :in optionals
                      :collect (first entry))
                    (loop
                      :for entry :in keywords
                      :appending (list (first (first entry))
                                       (second (first entry)))))
            rest)))

(defun lambda-list-to-funcall-expression (function args)
  (multiple-value-bind (arg-list rest-variable)
      (lambda-list-to-funcall-list args)
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-value-list-expression (args)
  ;; TODO use alexandria:parse-ordinary-lambda-list
  ;; JT@15/08/14: Seconded
  `(list ,@(let ((result (list)))
             (parse-lambda-list args
                                (lambda (kind name entry
                                         &optional external-name default)
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
  "Run TEST non-interactively and print results to *STANDARD-OUTPUT*.
This function is ideal for ASDF:TEST-OP's."
  (let* ((*test-run-standard-output* (make-broadcast-stream))
         (result (without-debugging (apply test-function args)))
         (*package* (find-package :common-lisp)))
    (format *standard-output*
"The result of ~S is:

  ~A

For more details run it from the REPL."
            test-function result)
    result))
