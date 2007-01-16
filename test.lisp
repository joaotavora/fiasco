;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(eval-always
  (assert (eq *defclass-macro-name-for-dynamic-context* 'defclass*)))

(defvar *print-test-run-progress* #t)
(defvar *test-progress-print-right-margin* 100)
(defvar *debug-on-unexpected-error* #t)
(defvar *debug-on-assertion-failure* #t)

(define-dynamic-context global-context
  ((failure-descriptions (make-array 8 :adjustable #t :fill-pointer 0))
   (assertion-count 0)
   (progress-char-count 0)
   (print-test-run-progress-p *print-test-run-progress* :type boolean)
   (debug-on-unexpected-error-p *debug-on-unexpected-error* :type boolean)
   (debug-on-assertion-failure-p *debug-on-assertion-failure* :type boolean)
   (current-test nil)
   (run-tests (make-hash-table))
   (test-lambdas (make-hash-table) :documentation "test -> compiled test lambda mapping for this test run")))

(defprint-object (self global-context :identity #f :type #f)
  (format t "results :tests ~A :assertions ~A :failures ~A"
          (hash-table-count (run-tests-of self)) (assertion-count-of self) (length (failure-descriptions-of self))))

(defun test-was-run-p (test)
  (declare (type testable test))
  (in-global-context context
    (and (gethash test (run-tests-of context))
         (not (eq (current-test-of context) test)))))

(defun register-test-being-run (test)
  (declare (type testable test))
  (in-global-context context
    (setf (gethash test (run-tests-of context)) (current-context))
    (setf (current-test-of context) test)))

(defgeneric get-test-lambda (test global-context)
  (:method ((test test) (context global-context))
           (bind (((values test-lambda found-p) (gethash test (test-lambdas-of context))))
             (unless found-p
               (setf test-lambda (bind ((*package* (package-of test))
                                        (*readtable* (copy-readtable)))
                                   (compile nil `(lambda ,(lambda-list-of test)
                                                  ,@(body-of test)))))
               (setf (gethash test (test-lambdas-of context)) test-lambda))
             test-lambda)))

(define-dynamic-context context
  ((test)
   (test-arguments)
   (number-of-added-failure-descriptions 0))
  :chain-parents #t)

(defprint-object (self context :identity #f :type #f)
  (format t "test-run ~@<(~S~{~^ ~S~})~@:>"
          (name-of (test-of self))
          (bind ((result (lambda-list-to-funcall-list (lambda-list-of (test-of self)))))
            (mapcar (lambda (arg-cell)
                      (setf result (substitute (cdr arg-cell) (car arg-cell) result :test #'eq)))
                    (test-arguments-of self))
            result)))

(defmacro deftest (&whole whole name args &body body)
  (bind (((values remaining-forms declarations documentation) (parse-body body :documentation #t :whole whole))
         ((name &rest test-args &key (compile-before-run #t) &allow-other-keys) (ensure-list name)))
    (with-unique-names (toplevel-p test test-lambda global-context result-values)
      `(progn
        (eval-when (:load-toplevel :execute)
          (make-test ',name
           :package ,*package*
           :lambda-list ',args
           :compile-before-run ,compile-before-run
           :declarations ',declarations
           :documentation ',documentation
           :body ',remaining-forms
           ,@test-args))
        (defun ,name ,args
          ,@(when documentation (list documentation))
          ,@declarations
          (declare (optimize (debug 3)))
          (bind ((,test (get-test ',name))
                 (,toplevel-p (not (has-global-context)))
                 (,global-context (unless ,toplevel-p
                                    (current-global-context)))
                 (,result-values '()))
            (assert ,test)
            (flet ((body ()
                     (with-new-context (:test ,test :test-arguments ,(lambda-list-to-value-list-expression args))
                       (register-test-being-run ,test)
                       (setf ,result-values
                             (multiple-value-list
                                 (labels ((prune-failure-descriptions ()
                                            ;; drop failures recorded by the previous run of this test
                                            (bind ((context (current-context)))
                                              (dotimes (i (number-of-added-failure-descriptions-of context))
                                                (vector-pop (failure-descriptions-of ,global-context)))
                                              (setf (number-of-added-failure-descriptions-of context) 0)))
                                          (run-it ()
                                            (handler-bind ((assertion-failed (lambda (c)
                                                                               (declare (ignore c))
                                                                               (unless (debug-on-assertion-failure-p ,global-context)
                                                                                 (continue))))
                                                           (serious-condition (lambda (c)
                                                                                (unless (typep c 'assertion-failed)
                                                                                  (record-failure* 'unexpected-error
                                                                                                   :description-initargs (list :condition c)
                                                                                                   :signal-assertion-failed #f)
                                                                                  (when (debug-on-unexpected-error-p ,global-context)
                                                                                    (invoke-debugger c))
                                                                                  (return-from run-it)))))
                                              (restart-case (bind ((*package* (package-of ,test))
                                                                   (*readtable* (copy-readtable)))
                                                              ,(if compile-before-run
                                                                   `(bind ((,test-lambda (get-test-lambda ,test ,global-context)))
                                                                     (funcall ,test-lambda ,@(lambda-list-to-funcall-list args)))
                                                                   `(progn
                                                                     ,@remaining-forms)))
                                                 (continue ()
                                                   :report (lambda (stream)
                                                             (format stream "~@<Skip the rest of the test ~S and continue~@:>" ',name))
                                                   (values))
                                                 (retest ()
                                                   :report (lambda (stream)
                                                             (format stream "~@<Rerun the test ~S~@:>" ',name))
                                                   (prune-failure-descriptions)
                                                   (return-from run-it (run-it)))))))
                                   (run-it)))))))
              (if ,toplevel-p
                  (with-new-global-context ()
                    (setf ,global-context (current-global-context))
                    (body))
                  (body))
              (if ,toplevel-p
                  (progn
                    (when (print-test-run-progress-p ,global-context)
                      (terpri *debug-io*))
                    (if ,result-values
                        (values-list (append ,result-values (list ,global-context)))
                        ,global-context))
                  (values-list ,result-values)))))))))

(defcondition* assertion-failed (test-related-condition simple-error)
  ())

(defcondition* assertion-failed-without-context (error)
  ((failure-description))
  (:report (lambda (c stream)
             (describe (failure-description-of c) stream))))

(defun record-failure (description-type &rest args)
  (record-failure* description-type :description-initargs args))

(defun record-failure* (type &key (signal-assertion-failed #t) description-initargs)
  (bind ((description (apply #'make-instance type
                             :test-context-backtrace (when (has-context)
                                                       (iter (for context :first (current-context) :then (parent-context-of context))
                                                             (while context)
                                                             (collect context)))
                             description-initargs)))
    (if (has-global-context)
        (in-global-context global-context
          (in-context context
            (when signal-assertion-failed
              (restart-case (error 'assertion-failed
                                   :test (test-of context)
                                   :format-control (etypecase description
                                                     (failed-assertion "Assertion ~S failed."))
                                   :format-arguments (typecase description
                                                       (failed-assertion (list (form-of description)))))
                (continue ()
                  :report (lambda (stream)
                            (format stream "~@<Record the failure and continue~@:>")))
                (continue-without-debugging ()
                  :report (lambda (stream)
                            (format stream "~@<Record the failure, turn off debugging for this run and continue~@:>"))
                  (setf (debug-on-unexpected-error-p global-context) #f)
                  (setf (debug-on-assertion-failure-p global-context) #f))))
            (vector-push-extend description (failure-descriptions-of global-context))
            (incf (number-of-added-failure-descriptions-of context))
            (write-progress-char (progress-char-of description))))
        (if *debug-on-assertion-failure*      ; we have no global-context
            (error 'assertion-failed-without-context :failure-description description)
            (describe description *debug-io*)))))

(defun extract-assert-expression-and-message (input-form)
  (bind ((negatedp #f)
         (predicate)
         (arguments '()))
    (labels ((process (form)
               (if (consp form)
                   (case (first form)
                     ((not)
                      (assert (= (length form) 2))
                      (setf negatedp (not negatedp))
                      (process (second form)))
                     (t (setf predicate (first form))
                        (setf arguments (rest form))))
                   (setf predicate form))))
      (process input-form)
      (cond ((= (length arguments) 0)
             (values nil
                     input-form
                     "Expression ~A evaluated to false."
                     (list `(quote ,input-form))))
            ((= (length arguments) 2)
             (with-unique-names (x y)
               (values `((,x ,(first arguments))
                         (,y ,(second arguments)))
                       (if negatedp
                           `(not (,predicate ,x ,y))
                           `(,predicate ,x ,y))
                       "Binary predicate ~A failed.~%~
                        x: ~A evaluated to ~S~%~
                        y: ~A evaluated to ~S"
                       (list (if negatedp
                                 `(quote (not (,predicate x y)))
                                 `(quote (,predicate x y)))
                             `(quote ,(first arguments)) x
                             `(quote ,(second arguments)) y))))
            (t (bind ((arg-values (mapcar (lambda (el) (declare (ignore el)) (gensym)) arguments))
                      (bindings (iter (for arg :in arguments)
                                      (for arg-value :in arg-values)
                                      (collect `(,arg-value ,arg))))
                      (expression (if negatedp
                                      `(not (,predicate ,@arg-values))
                                      `(,predicate ,@arg-values)))
                      ((values message message-args) (iter (with message = "Expression ~A evaluated to ~A")
                                                           (for arg :in arguments)
                                                           (for arg-value :in arg-values)
                                                           (setf message (concatenate 'string message "~%~A evaluated to ~S"))
                                                           (appending `((quote ,arg) ,arg-value) :into message-args)
                                                           (finally (return (values message message-args))))))
                 (values bindings
                         expression
                         message
                         (nconc (list `(quote ,input-form) (if negatedp "true" "false")) message-args))))))))

(defun write-progress-char (char)
  (bind ((context (when (has-global-context)
                    (current-global-context))))
    (when (and context
               (print-test-run-progress-p context))
      (when (and (not (zerop (progress-char-count-of context)))
                 (zerop (mod (progress-char-count-of context)
                             *test-progress-print-right-margin*)))
        (terpri *debug-io*))
      (incf (progress-char-count-of context)))
    (when (or (and context
                   (print-test-run-progress-p context))
              (and (not context)
                   *print-test-run-progress*))
      (write-char char *debug-io*))))

(defun register-assertion-was-successful ()
  (write-progress-char #\.))

(defun register-assertion ()
  (when (has-global-context)
    (in-global-context context
      (incf (assertion-count-of context)))))

(defmacro is (&whole whole form)
  (bind (((values bindings expression message message-args)
          (extract-assert-expression-and-message form)))
    (with-unique-names (result)
      `(progn
        (register-assertion)
        (bind ,bindings
          (bind ((,result (multiple-value-list ,expression)))
            (if (first ,result)
                (register-assertion-was-successful)
                (record-failure 'failed-assertion :form ',whole
                                :format-control ,message :format-arguments (list ,@message-args)))
            (values-list ,result)))))))

(defmacro signals (what &body body)
  (bind ((condition-type what))
    `(progn
      (register-assertion)
      (block test-block
        (handler-bind ((,condition-type (lambda (c)
                                          (declare (ignore c))
                                          (return-from test-block (values)))))
          ,@body
          (register-assertion-was-successful))
        (record-failure 'missing-condition :condition ',condition-type))
      (values))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; some utils

(defun lambda-list-to-funcall-list (args)
  (iter (with in-keywords = #f)
        (for cell :first args :then (cdr cell))
        (while cell)
        (for arg = (first (ensure-list (car cell))))
        (case arg
          (&key (setf in-keywords #t))
          (&allow-other-keys)
          (&rest (setf cell (cdr cell)))
          (t (if in-keywords
                 (progn
                   (collect (intern (symbol-name (first (ensure-list arg)))
                                    #.(find-package "KEYWORD")))
                   (collect arg))
                 (collect arg))))))

(defun lambda-list-to-value-list-expression (args)
  `(list ,@(iter (for cell :first args :then (cdr cell))
                 (while cell)
                 (for arg = (first (ensure-list (car cell))))
                 (case arg
                   (&rest (setf cell (cdr cell)))
                   ((&key &allow-other-keys &optional))
                   (t (collect `(cons ',arg ,arg)))))))

(defun lambda-list-to-ignore-list (args)
  (iter (for cell :first args :then (cdr cell))
        (while cell)
        (for arg = (first (ensure-list (car cell))))
        (case arg
          (&rest (setf cell (cdr cell)))
          ((&key &allow-other-keys &optional))
          (t (collect arg)))))

