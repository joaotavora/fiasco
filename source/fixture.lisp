;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

(defvar *fixture-body-aliases* '()
  "This is a kludge to overcome some package/dependency issues between -body- in hu.dwim.def and hu.dwim.stefil without a shared dependency.")

(defun fixture-function-name-for (name)
  (symbolicate '#:fixture/call-with/ name))

(defmacro defixture (name &body body)
  (with-unique-names (global-context finished? nesting-count)
    `(defun ,(fixture-function-name-for name) (thunk)
       (declare (optimize (debug 3)))
       (let* ((,global-context (and (has-global-context)
                                    (current-global-context)))
              (,finished? nil))
         (symbol-macrolet ((,nesting-count (gethash ',name (run-fixtures-of ,global-context) 0)))
           (assert (or (not ,global-context)
                       (>= ,nesting-count 0)))
           (unwind-protect
                (progn
                  (when ,global-context
                    (incf ,nesting-count))
                  (flet ((-body- ()
                           (when ,finished?
                             (error "The ~S of the fixture ~S has been invoked multiple times" '-body- ',name))
                           (multiple-value-prog1
                               (funcall thunk)
                             (setf ,finished? t))))
                    (if (or (not ,global-context)
                            (= ,nesting-count 1))
                        (progn
                          (multiple-value-prog1
                              (flet (,@(loop
                                         :for alias :in *fixture-body-aliases*
                                         :collect `(,alias () (-body-))))
                                ,@body)
                            (unless ,finished?
                              (error "The fixture ~S did not call ~S at all" ',name '-body-)))
                          t)
                        (progn
                          (-body-)
                          nil))))
             (when ,global-context
               (decf ,nesting-count))))))))

(defmacro with-fixture (name &body body)
  (with-unique-names (body-fn result)
    `(flet ((,body-fn ()
              (let ((,result nil))
                (,(fixture-function-name-for name) (named-lambda with-fixture-body ()
                                                     (setf ,result (multiple-value-list (progn ,@body)))))
                (values-list ,result))))
      (if (has-global-context)
          (,body-fn)
          (with-new-global-context* ()
            (,body-fn))))))

(defmacro with-fixtures (fixtures &body body)
  (if fixtures
      `(with-fixture ,(first fixtures)
        (with-fixtures ,(rest fixtures)
          ,@body))
      `(progn ,@body)))
