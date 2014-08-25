Stefil
======

Stefil is a simple and powerful test framework for Common Lisp.

There's a fairly outdated introduction [here][old-intro], but scroll
down for a simple example demonstrating just few features.

Up and running
--------------

This assumes you're using a recent version of [quicklisp][quicklisp]

```lisp
(push "path/to/stefils/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :stefil)
```

or alternatively, just use [asdf][asdf]

```lisp
(push "path/to/stefils/dir" asdf:*central-registry*)
(asdf:require-system :stefil)
```

now create some Lisp file with

```lisp
(defpackage #:example-time (:export #:seconds #:hours-and-minutes))
(in-package #:example-time)

(defun seconds (hours-and-minutes)
  (+ (* 3600 (first hours-and-minutes))
     (* 60 (seconds hours-and-minutes))))

(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (truncate seconds 60)))

(stefil:define-test-package #:stefil-examples
  (:use #:example-time))
(in-package #:stefil-examples)

(deftest test-conversion-to-hours-and-minutes ()
  (is (equal (hours-and-minutes 180) '(0 3)))
  (is (equal (hours-and-minutes 4500) '(1 15))))

(deftest test-conversion-to-seconds ()
  (is (= 60 (seconds '(0 1))))
  (is (= 4500 (seconds '(1 15)))))

(deftest double-conversion ()
  (is (= 3600 (seconds (hours-and-minutes 3600))))
  (is (= 1234 (seconds (hours-and-minutes 1234)))))
```
load or compile it, and in your REPL run

    > (in-package :stefil-examples)
    STEFIL-EXAMPLES> (run-package-tests)
    STEFIL-EXAMPLES (Suite)
      TEST-CONVERSION-TO-SECONDS                                                    [FAIL]
      TEST-CONVERSION-TO-HOURS-AND-MINUTES                                          [FAIL]
      DOUBLE-CONVERSION                                                             [FAIL]

    Test failures:

    Failure 1: UNEXPECTED-ERROR when running STEFIL-EXAMPLES::TEST-CONVERSION-TO-SECONDS

        Stack overflow (signal 1000)

    Failure 2: FAILED-ASSERTION when running STEFIL-EXAMPLES::TEST-CONVERSION-TO-HOURS-AND-MINUTES

        Binary predicate (EQUAL X Y) failed.
        x: (STEFIL-EXAMPLES::HOURS-AND-MINUTES 4500) => (1 75)
        y: '(1 15) => (1 15)

    Failure 3: UNEXPECTED-ERROR when running STEFIL-EXAMPLES::DOUBLE-CONVERSION

        Stack overflow (signal 1000)
    #<test-run: 4 tests, 4 assertions, 3 failures in 0.031 sec (1 failed assertion, 2 errors, none expected)>

Yay, everything fails!

Debugging failures
------------------

Run the example again, with `:interactive t` to bring up the Lisp debugger 
every time a test failure happens. They are caused by error conditions or 
test assertion failures. We have two of the former and one of the latter.

In this case, we see that the the stack overflow erros are due to a typo 
(`seconds` should be `second` in line 6) and that `hours-and-minutes` should 
be rewritten like:

```lisp
(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (/ (rem seconds 3600) 60)))
```

After that, you'll see a nice

    > (in-package :stefil-examples)
    STEFIL-EXAMPLES> (run-package-tests)
    STEFIL-EXAMPLES (Suite)
      TEST-CONVERSION-TO-SECONDS                                                    [ OK ]
      TEST-CONVERSION-TO-HOURS-AND-MINUTES                                          [ OK ]
      DOUBLE-CONVERSION                                                             [ OK ]
    #<test-run: 4 tests, 6 assertions, 0 failures in 0.0 sec>

Support
-------

Open an [issue][issues], discuss in the [stefil-devel][stefil-devel] mailing 
list or in the [#lisp][sharp-lisp] IRC channel.


[old-intro]: http://common-lisp.net/project/stefil/index-old.shtml
[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[stefil-devel]: http://dir.gmane.org/gmane.lisp.stefil.devel
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/luismbo/stefil/issues
