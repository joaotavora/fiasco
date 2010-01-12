test suite - specialized app (asserts)

others:
- too much "framework"-y stuff
- test suites look much different than real applications
- why new abstractions? aren't DEFUN's good enough?

stefil:
- ensured dynamic environment
- ensured by DEFTEST, which is an insturmented DEFUN
- macrology: IS, SIGNALS, FINISHES, WITH-EXPECTED-FAILURES
- copy-pastable into the REPL
- integration with slime
- collected data is returned as the n+1th value
- entry point anywhere

IN-SUITE: mimicing in-package.
 - SUITE's are also DEFUN's. RUN-CHILD-TESTS is a lexically visible function
   for wrapping suite bodies in a certain environment.

...which leads to fixtures: code pieces :setup/:teardown

result:
- small, minimal impedance mismatch
- test failure is the same as application failure: same tools, same way of fixing bugs.
- test suite is just a specialized application with loads of asserts

(hu.dwim.asdf:develop-system :hu.dwim.stefil)

(in-package :hu.dwim.stefil.test)

(is (= 2 2))

(without-debugging (is (= 2 4)))

(deftest foo (a b)
  (is (= a b)))

(foo 1 2)

(deftest bar ()
  (foo 1 1)
  (foo 1 2)
  (with-expected-failures
    (foo 1 3))
  "original-return-value")

(bar)

fixtures

result:
- small, minimal impedance mismatch
- test failure is the same as application failure: same tools, same way of fixing bugs.
- test suite is just a specialized application with loads of asserts
