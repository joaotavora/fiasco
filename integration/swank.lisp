;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil)

(defun call-with-sldb-quit-restart (thunk restart)
  (bind ((swank::*sldb-quit-restart* restart))
    (funcall thunk)))

;; the inspector code in hu.dwim.slime is too far from slime head to easily provide these inspector customizations, so just turn it off
#+hu.dwim.slime
(progn

(defun stefil-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((test (find-test form :otherwise nil)))
      (when test
        (values test t)))))

(when (boundp 'swank::*inspector-dwim-lookup-hooks*)
  (pushnew 'stefil-inspector-lookup-hook swank::*inspector-dwim-lookup-hooks*))

(defvar *display-all-slots-in-inspector* nil)

(defun make-rerun-test-action-for-inspector (context)
  (lambda ()
    (apply (name-of (test-of context))
           (mapcar #'cdr (test-arguments-of context)))
    (swank::inspect-object *last-test-result*)))

(defun make-run-test-action-for-inspector (test)
  (lambda ()
    (funcall (name-of test))
    (swank::inspect-object *last-test-result*)))

(defun present-test-for-emacs (test &key name-only undefine-action actions-first)
  (when test
    (bind ((actions `(,@(if (lambda-list-of test)
                            (when actions-first
                              `((:label "[run]")))
                            `((:action "[run]" ,(make-run-test-action-for-inspector test))))
                      ,@(when undefine-action
                          (if (find-test (name-of test) :otherwise nil)
                              (when actions-first
                                `((:label "[undefine]")))
                              `((:action "[undefine]" ,(lambda () (rem-test (name-of test)))))))))
           (value `((:value ,(if name-only (name-of test) test)))))
      (if actions-first
          (append actions '(" ") value)
          (append value '(" ") actions)))))

(defun present-context-for-emacs (context)
  `((:value ,context) " " (:action "[rerun]" ,(make-rerun-test-action-for-inspector context))))

(defun present-test-backtrace-for-emacs (description)
  (iter (for context :in (test-context-backtrace-of description))
        (for idx :upfrom 0)
        (when (first-time-p)
          (appending `((:newline) (:label "Test backtrace:") (:newline))))
        (collect (format nil "  ~D: " idx))
        (appending (present-context-for-emacs context))
        (collect `(:newline))))

(defun present-all-slots-for-emacs (object)
  (if *display-all-slots-in-inspector*
      (append `((:newline)
                (:action "[hide slots]" ,(lambda () (setf *display-all-slots-in-inspector* nil)))
                (:newline))
              (swank::all-slots-for-inspector object))
      `((:newline)
        (:action "[show all slots]" ,(lambda () (setf *display-all-slots-in-inspector* t))))))

(defmacro inspector-result (title content)
  `(list :title ,title :type nil :content ,content))

(defmethod swank-backend::emacs-inspect ((global-context global-context))
  (inspector-result
   "Stefil test results"
   (swank-backend::label-value-line*
    ("Executed tests" (hash-table-values (run-tests-of global-context))
                      :value-text (princ-to-string (hash-table-count (run-tests-of global-context))))
    ("Executed assertions" (princ-to-string (assertion-count-of global-context)) :splice-as-ispec t)
    (@ (unless (emptyp (failure-descriptions-of global-context))
         `((:label ,(format nil "List of failures (~A): " (length (failure-descriptions-of global-context))))
           (:action "[rerun all failed tests]"
                    ,(lambda () (swank::inspect-object (run-failed-tests global-context))))
           (:newline))))
    ;; intentionally reverse the order by push'ing
    (@ (iter (for description :in-vector (failure-descriptions-of global-context))
             (for context = (first (test-context-backtrace-of description)))
             (collect "  ")
             (collect `(:action "[rerun]" ,(make-rerun-test-action-for-inspector context)))
             (collect " ")
             (collect `(:value ,description))
             (collect `(:newline))))
    (@ (present-all-slots-for-emacs global-context)))))

(defmethod swank-backend::emacs-inspect ((context context))
  (inspector-result
   "Stefil test context"
   (swank-backend::label-value-line*
    ("Test" (test-of context))
    ("Test arguments" (test-arguments-of context) :display-nil-value nil)
    ("Real time spent in body" (list (or (real-time-spent-in-seconds context) "?") '(:label " sec ")
                                     `(:action "[rerun]" ,(make-rerun-test-action-for-inspector context)))
                               :splice-as-ispec t)
    (@ (iter (for parent-context :first (parent-context-of context) :then (parent-context-of parent-context))
             (while parent-context)
             (when (first-time-p)
               (collect `(:label "Parent test frames:"))
               (collect `(:newline)))
             (collect "  ")
             (appending (reverse (present-context-for-emacs parent-context)))
             (collect `(:newline))))
    (@ (present-all-slots-for-emacs context)))))

(defmethod swank-backend::emacs-inspect ((failure failed-assertion))
  (inspector-result
   "Failed Stefil assertion"
   (swank-backend::label-value-line*
    ("Form" (form-of failure))
    (@ (present-test-backtrace-for-emacs failure))
    (@ (present-all-slots-for-emacs failure)))))

(defmethod swank-backend::emacs-inspect ((description unexpected-error))
  (inspector-result
   "Unexpected error in a Stefil test"
   (swank-backend::label-value-line*
    ("Condition" (condition-of description))
    (@ (present-test-backtrace-for-emacs description))
    (@ (present-all-slots-for-emacs description)))))

(defmethod swank-backend::emacs-inspect ((test test))
  (inspector-result
   "Stefil test"
   (swank-backend::label-value-line*
    ("Name" (present-test-for-emacs test :undefine-action t :name-only t) :splice-as-ispec t)
    ("Package" (package-of test))
    ("Compile before run?" (if (compile-before-run-p test) "yes" "no") :splice-as-ispec t)
    ("Auto call by its suite?" (if (auto-call-p test) "yes" "no") :splice-as-ispec t)
    ("Documentation" (documentation-of test) :display-nil-value nil)
    ("Parent" (present-test-for-emacs (parent-of test)) :splice-as-ispec t)
    (@ (iter (for (nil child) :in-hashtable (children-of test))
             (when (first-time-p)
               (appending `((:newline) (:label "Children:") (:newline))))
             (collect "  ")
             (appending (present-test-for-emacs child :actions-first t))
             (collect `(:newline))))
    (@ (present-all-slots-for-emacs test)))))

) ; #+hu.dwim.slime
