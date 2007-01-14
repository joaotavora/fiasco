;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :sinfol-test)

(sinfol::eval-always
  (import (let ((*package* (find-package :sinfol)))
            (read-from-string "(enable-sharp-boolean-syntax)"))))

(enable-sharp-boolean-syntax)

;;(def-suite :sinfol :description "sinfol tests")

;;(in-suite :sinfol)


