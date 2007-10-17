;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:terse-ppcre
  (:use :cl :ppcre)
  (:documentation "TERSE-PPCRE allows you to use create CL-PPCRE parse trees
using a simpler syntax that is still S-expression based.  It tries to mimic
perl string regular expression syntax as closely as possible, although in some
cases the lisp reader prevents us from doing that (for example, we can't use
a period to represent any character, so ANY is used instead).")
  (:export
   #:re
   #:defre))



