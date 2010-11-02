;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.stefil.documentation)

(def project :hu.dwim.stefil)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO"))
  (chapter (:title "Tips and Tricks")
    (chapter (:title "Adding a new test interactively in slime")
      (paragraph ()
        "When interactively defining a new test \(e.g. with Slime using C-c C-c), the test will be put in the current suite (see IN-SUITE). However, if a test is already defined with the same name, then the newly (re)defined test will stick to the original suite regardless the current suite.")
      (paragraph ()
        "This behaviour is intentional: tests are not changing suites, so that you can C-c C-c freely \(even without an IN-PACKAGE like support from Slime). If you want to force a test into a different suite, you can specify the test suite explicitly for the time of a C-c C-c:")
      ;; FIXME use a lisp form type here, not shell-script...
      (shell-script ()
        "(deftest (foo :in my-new-suite) () ... )"))))
