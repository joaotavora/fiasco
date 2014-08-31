;;; -*- mode: Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :fiasco-self-tests
  (:use :alexandria
        :common-lisp
        :fiasco)

  (:shadow
   #:deftest
   #:test
   #:fixtures))

(in-package :fiasco)

(import-all-owned-symbols :fiasco :fiasco-self-tests)
