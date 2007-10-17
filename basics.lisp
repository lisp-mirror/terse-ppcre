;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Basic parse-tree-synonyms

(in-package :terse-ppcre)

(defmacro define-combination-symbols ()
  `(progn
    ,@(loop for class in '(b d w s !b !d !w !s any)
            appending
            (loop for modifier in '(? + +? * *?)
                  collecting
                  `(defre ,(intern (format nil "~A~A"
                                           (symbol-name class)
                                           (symbol-name modifier)))
                    ,modifier ,class)))
    (values)))


(defre d+ (+ d))
(defre d+? (+? d))