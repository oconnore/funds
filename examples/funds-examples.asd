
;;;; -*- Lisp -*-

(in-package :cl-user)

(defpackage #:funds-examples-asd
  (:use :cl :asdf))

(in-package :funds-examples-asd)

(defsystem funds-examples
  :serial t
  :components ((:file "package")
	       (:file "sudoku")
	       (:file "puzzles"))
  :depends-on (:iterate :funds))
