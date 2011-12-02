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
      "Stefil stands for Simple Testing Framework in Lisp. Its philosophy is to stay as transparent as possible, disrupting the normal Lisp application development process as little as possible.")
    (paragraph ()
      "What Stefil basically adds is that it quietly gathers extra runtime information and statistics (like a test backtraces, assertions, failures and/or errors), and introduces a wider range of ASSERT-like primitives to express the expected behavior of a program. It also integrates well with the Slime inspector, and provides useful restarts to achieve that goal.")
    (paragraph ()
      "What mostly differentiates Stefil from the other test frameworks are the following abstractions: DEFTEST, DEFSUITE and IN-SUITE. DEFTEST is basically a DEFUN, but upon invocation it ensures an internal dynamic environment where the other primitives of Stefil can collect data. DEFSUITE is basically a DEFTEST with an optional arglist and/or body, and serves as a container to collect child tests that will be automatically invoked when no custom arglist and body are provided.")
    (paragraph ()
      "The rest is the usual suspects: IS, SIGNALS, NOT-SIGNALS, FINISHES, WITH-EXPECTED-FAILURES, etc.")
    (paragraph ()
      "And for advanced users there's DEFIXTURE, which is basically a setup/teardown function that is guaranteed to be executed exactly once in every dynamic extent, regardless of which entry point of the test suite was used to invoke it."))
  (chapter (:title "Unique selling points")
    ;; TODO use bullet lists or somesuch
    (paragraph ()
      "Non-intrusive to the normal development process (debugging a failed test is exactly the same experience as debugging your application)")
    (paragraph ()
      "Integrates well with Slime")
    (paragraph ()
      "Lets you run tests both in non-interactive and in interactive mode with useful restarts")
    (paragraph ()
      "Makes it trivial to invoke even only parts of the test suite: just call the DEFTEST (DEFUN) of your choice")
    (paragraph ()
      "Allows you to interactively skip parts of the test suite (e.g. to skip a long test (and remember that suite are also tests) you can abort with C-c C-c and select a restart)")
    (paragraph ()
      "Its fixture implementation allows invoking specific parts of the test suite without sacrificing performance"))
  (chapter (:title "Tips and Tricks")
    (chapter (:title "Adding a new test interactively in Slime")
      (paragraph ()
        "When interactively defining a new test \(e.g. with Slime using C-c C-c), the test will be put in the current suite (see IN-SUITE, which is very much like IN-PACKAGE, but for test suites). However, if a test has already been defined with the same name, then the newly (re)defined test will stick to its original suite, regardless of the current suite at the time of redefinition.")
      (paragraph ()
        "This behaviour is intentional: tests are not changing suites, so that you can easily redefine them dinamically \(even without an IN-PACKAGE like IN-SUITE support from Slime). If you want to force a test into a different suite without restarting your lisp session, then you can explicitly specify the test suite for one redefinition:")
      ;; FIXME use a lisp form type here, not shell-script...
      (shell-script ()
        "(deftest (foo :in my-new-suite) () ... )")))
  (chapter (:title "Implementation details")
    (paragraph ()
      "The basic idea is that at the beginning of each DEFTEST (DEFUN) a piece of code ensures a test session context object called GLOBAL-CONTEXT, and stores it in a special variable when created. When the DEFTEST (DEFUN) that was used to invoke the test suite returns, this test session context is returned as the first value (with the original values following it). The assertion primitives collect their statistics into this context object which has a fancy PRINT-OBJECT to present the test session's outcome when returned to the REPL. This object can be inspected with a customized Slime inspector.")
    (paragraph ()
      "On top of that DEFTEST also makes a test context called CONTEXT unconditionally each time a test is invoked. These contexts are collected into a 'test-backtrace' and are captured when failures are recorded. This test-backtraces are available for inspection from the GLOBAL-CONTEXT.")
    (paragraph ()
      "Fixtures (see DEFIXTURE and WITH-FIXTURE) are pairs of lambda's, one for setup, and one for teardown. There's a hashtable in the GLOBAL-CONTEXT that registers whether the setup/teardown function needs to be called, which is guaranteed to happen exactly once per test session, regardless of how many WITH-FIXTURE primitives get crossed in the test session.")))
