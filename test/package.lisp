;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :stefil-test
  (:use :alexandria
        :common-lisp
        :stefil)

  (:shadow
   #:deftest
   #:test
   #:fixtures))

(in-package :stefil)

(import-all-owned-symbols :stefil :stefil-test)
