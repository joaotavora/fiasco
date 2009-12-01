;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;; we define "TEST" with a string name, so it will match (def test ...) no matter what is the
;; home package of 'test. this is done like that to be able to write
;; (def any-package::test some-lib::test ...) in any library to create the toplevel TEST
;; defun called some-lib::test without shadowing stefil:test everywhere.
(def (definer :available-flags "do") "TEST" ()
  (function-like-definer -definer- 'hu.dwim.stefil:deftest -whole- -environment- -options-))

(def (definer :available-flags "e") hu.dwim.stefil::suite (name &rest args)
  `(progn
     (hu.dwim.stefil:defsuite ,name ,@args)
     ,@(when (getf -options- :export)
             `((export ',(first (ensure-list name)))))))

(def (definer :available-flags "e") hu.dwim.stefil::suite* (name &rest args)
  `(progn
     (hu.dwim.stefil:defsuite* ,name ,@args)
     ,@(when (getf -options- :export)
             `((export ',(first (ensure-list name)))))))

(def (definer :available-flags "e") hu.dwim.stefil::fixture (name &body body)
  `(progn
     (hu.dwim.stefil:defixture ,name ,@body)
     ,@(when (getf -options- :export)
             `((export ',name)))))

(integrated-export '(hu.dwim.stefil::suite hu.dwim.stefil::suite* hu.dwim.stefil::fixture) :hu.dwim.def)
