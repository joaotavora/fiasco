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
      "Stefil stands for Simple Testing Framework in Lisp. Its philosophy is to stay out of the way and don't disrupt the normal Slime based Lisp development, only gather some extra runtime information (like a test backtrace, assertions, failures and/or errors).")
    (paragraph ()
      "The interesting extra abstractions it provides are DEFTEST, DEFSUITE and IN-SUITE. DEFTEST is basically a DEFUN, but upon invocation it ensures an internal dynamic environment where the other primitives of Stefil can collect data. DEFSUITE is basically a DEFTEST with an optional arglist and/or body, and a list of child tests that are automatically invoked when no custom arglist and body are provided.")
    (paragraph ()
      "The rest is the usual suspects: IS, SIGNALS, NOT-SIGNALS, FINISHES, WITH-EXPECTED-FAILURES.")
    (paragraph ()
      "And for advanced users there's DEFIXTURE, which is basically a setup/teardown function that is only executed once in every dynamic extent."))
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
