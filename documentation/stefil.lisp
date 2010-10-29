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
    (paragraph (:title "Adding a new test interactively in slime")
      "When working in slime, you might be interested in using
slime-compile-defun \(usually bound to C-c C-c) on a new test, but you
would be surprised: the test is not added to the suite (as you can
verify by using slime-inspect on the name of the suite). Even if you
have issued a (in-suite mysuite) before, the number of the tests in
the suite remains constant.")
    (paragraph ()
      "In fact this behaviour is intentional (a 'feature', if you like):
tests are not changing suites once they ended up in one of them so
that you can C-c C-c freely \(without an in-package like support from
slime). In other words: tests are sticking to suites. So, if you want
to add a test to an existing suite, you have to use")
    (shell-script ()
      "(deftest (foo :in mysuite) () ... )"
      "or"
      "(def test (foo :in mysuite) () ...)")
    (paragraph ()
      "During the loading of the package with asdf, there is no need
to specify the suite with the :in clause, so you might want to remove
it afterwards.")))
