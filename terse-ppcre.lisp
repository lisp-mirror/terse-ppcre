;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Main code for terse-ppcre

(in-package :terse-ppcre)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; always eval, because these helper functions/vars are used by macros.

  (in-package :terse-ppcre)

  (defparameter *regex-symbol-abbrev*
    (let ((h (make-hash-table :test 'equal)))
      (loop for (in out) in '((any :everything)
                              (b :word-boundary)
                              (!b :non-word-boundary)
                              (d :digit-class)
                              (!d :non-digit-class)
                              (w :word-char-class)
                              (!w :non-word-char-class)
                              (s :whitespace-char-class)
                              (!s :non-whitespace-char-class)
                              (^ :start-anchor)
                              ($ :end-anchor)
                              (a :modeless-start-anchor)
                              (z :modeless-end-anchor)
                              (zn :modeless-end-anchor-no-newline))
            do (setf (gethash (symbol-name in) h) out))
      h))



  (defparameter *regex-car-abbrev*
    (let ((h (make-hash-table :test 'equal)))
      (loop for (in . out) in '((do :sequence)
                                (all :group)
                                (g :group)
                                (or :alternation)
                                (?= :positive-lookahead)
                                (?! :negative-lookahead)
                                (?<= :positive-lookbehind)
                                (?<! :negative-lookbehind)
                                (? :greedy-repetition 0 1)
                                (* :greedy-repetition 0 nil)
                                (+ :greedy-repetition 1 nil)
                                (*? :non-greedy-repetition 0 nil)
                                (+? :non-greedy-repetition 1 nil)
                                (/ :regex)
                                ([] :char-class)
                                ([^] :inverted-char-class)
                                (- :range))
            do (setf (gethash (symbol-name in) h) out))
      h))

  (defparameter *regex-combo-symbol-abbrev* (make-hash-table :test 'equal))

  ;; Take the cartesian product of these regex classes
  ;; and repetition modifiers, and generate shortcut symbols
  ;; for them
  (loop for class in '(d w s !d !w !s any)
        do (loop for modifier in '(? + +? * *?)
                 for syn = (format nil "~A~A"
                                   (symbol-name class)
                                   (symbol-name modifier))
                 for replacement = (append (gethash (symbol-name modifier) *regex-car-abbrev*)
                                           (list (gethash (symbol-name class) *regex-symbol-abbrev*)))
                 do (setf (gethash syn *regex-combo-symbol-abbrev*) replacement)))


  (defparameter *on-switches* '((#\I :case-insensitive-p)
                                (#\S :single-line-mode-p)
                                (#\M :multi-line-mode-p)))

  (defparameter *off-switches* '((#\I :case-sensitive-p)
                                 (#\S :not-single-line-mode-p)
                                 (#\M :not-multi-line-mode-p)))

  (defun lookaround-form-p (form)
    (and (listp form)
         (or (member (symbol-name (car form))
                     '("?=" "?!" "?<=" "?<!" ) :test #'string=)
             (member (car form)
                     '(:positive-lookahead
                       :negative-lookahead
                       :positive-lookbehind
                       :negative-lookbehind)))))

  (defun ism-p (name)
    (and (char= (char name 0) #\?)
         (> (length name) 1)
         (every (lambda (c) (member c '(#\I #\S #\M #\-))) (subseq name 1))))


  (defun expand-ism-name (name)
    "Turn ?ism-ism and variations into their symbols"
    (remove-duplicates
     (loop with lookup = *on-switches*
           for char across (subseq name 1)
           when (char= char #\-) do (setf lookup *off-switches*)
           else collecting (second (assoc char lookup)))
     :from-end t))


  (defun get-rep-term (sym)
    (when (symbolp sym)
      (let ((name (symbol-name sym)))
        (if (string= name "}")
            :greedy-repetition
            (when (string= name "}?")
              :non-greedy-repetition)))))

  (defun expand-repeat-form (form)
    "Expand a form like ({ 5 10 } d)"
    ;; y or z must be a terminator: '} or '}?
    (flet ((expand (repetition bounds-index form)
             "Assumes that the terminator is already found and in the correct place."
             (let ((bounds (mapcar #'(lambda (x) (nth (1- x) form)) bounds-index))
                   (body (nthcdr (1+ (car (last bounds-index))) form)))
               (if (every #'integerp bounds)
                   (values (list repetition (first bounds) (second bounds)) body)
                   (error "Expected integer for ~{~:R ~#[~;and ~]~}element~P of ~S"
                          bounds-index
                          (length bounds)
                          form)))))
      (let* ((termpos (position-if #'get-rep-term (rest form)))
             (reptype (when termpos (get-rep-term (nth termpos (rest form))))))
        (case termpos
          ('nil (error "No terminator ( } or }? ) found for repetition group ~S" form))
          (0 (error "Expected one or two counts before terminator of repetition group ~S" form))
          (1 (expand reptype (list 2) form))
          (2 (expand reptype (list 2 3) form))
          (t (error "Repetition group ~S has two count boundaries, maximum is two." form))))))

  (defun expand-branch-form (form)
    (list :branch
          (let ((condition (second form)))
            (when (null (cddr form))
              (error "No clauses found for ~S" form))
            (when (> (length form) 4)
              (error "Too many clauses for ~S" form))
            (cond
              ((lookaround-form-p condition)
               (transform-regex-tree condition))
              ((integerp condition) condition)
              (t (error "~S - Condition should be either an integer or a lookaround-form" form))))
          (cons :alternation (mapcar #'transform-regex-tree (cddr form)))))


  (defun transform-regex-symbol (sym)
    (assert (symbolp sym))
    (let ((name (symbol-name sym)))
      (if (char= (char name 0) #\/)
          (list :back-reference (if (digit-char-p (char name 1))
                                    (parse-integer (subseq name 1))
                                    (subseq name 1)))
          (let ((match (or (gethash (symbol-name sym) *regex-symbol-abbrev*)
                           (gethash (symbol-name sym) *regex-combo-symbol-abbrev*))))
            (if match
                match
                sym)))))


  (defun transform-regex-combination (c)
    (assert (listp c))
    (if (symbolp (car c))
        (let ((cname (symbol-name (car c))))
          (cond
            ((string= cname "R")
             `(:register (:sequence ,@(mapcar #'transform-regex-tree (cdr c)))))
            ((string= cname "NR") `(:named-register
                                    ,(if (symbolp (second c))
                                         (symbol-name (second c))
                                         (second c))
                                    (:sequence ,@(mapcar #'transform-regex-tree (cddr c)))))
            ((string= cname "IF") (expand-branch-form c))
            ((ism-p cname)
             (values (cons :group (expand-ism-name cname)) (rest c)))
            ((string= cname "{")
             (expand-repeat-form c))
            (t (let ((match (gethash cname *regex-car-abbrev*)))
                 (if match
                     (values match (rest c))
                     (values (list :sequence) c))))))
        (values (list :sequence) c)))


  (defun transform-regex-tree (tree)
    "Compile an abridged format ppcre regex tree into the
canonical format."
    (typecase tree
      (symbol (transform-regex-symbol tree))
      (list   (multiple-value-bind (modified leftover)
                  (transform-regex-combination tree)
                (append modified (when leftover
                                   (mapcar #'transform-regex-tree leftover)))))
      (t      tree)))


);; end the eval-when (...)


;;; EXPORTED MACROS

(defmacro re (&body regex)
  "Transform a terse regex parse tree into one that CL-PPCRE can
understand. Note that you can enclose the regex as a list or not, either way
should work.

usage: (re [REGEX TREE])

Examples:

;;;  (re * d) ; same as \"\\\\d*\"
;;; (re (^ (or (+ #\\A) (+ #\\B)) $)) ; same as \"^(?:A+|B+)$\"

Symbolic substitutions:
;;;  any => :everything
;;; b   => :word-boundary
;;; !b  => :non-word-boundary
;;; d   => :digit-class
;;; !d  => :non-digit-class
;;; w   => :word-char-class
;;; !w  => :non-word-char-class
;;; s   => :whitespace-char-class
;;; !s  => :non-whitespace-char-class
;;; ^   => :start-anchor
;;; $   => :end-anchor
;;; a   => :modeless-start-anchor
;;; z   => :modeless-end-anchor
;;; zn  => :modeless-end-anchor-no-newline
;;; /1  => (:back-reference 1)
;;; /hi => (:back-reference \"HI\")

Combinations of (any d !d w !w s !s) and (? * *? + +?), such as
;;;  d+   => (:greedy-repetition 1 nil :digit-class)
;;; any? => (:greedy-repetition 0 1 :everything)
;;; s*?  => (:non-greedy-repetition 0 nil :whitespace-char-class)

List transformations:
;;;  (do ...)       => (:sequence ...)
;;; (all ...)      => (:group ...)
;;; (g ...)        => (:group ...)
;;; (or ...)       => (:alternation ...)
;;; (?= ...)       => (:positive-lookahead ...)
;;; (?! ...)       => (:negative-lookahead ...)
;;; (?<= ...)      => (:positive-lookbehind ...)
;;; (?<! ...)      => (:negative-lookbehind ...)
;;; (? ...)        => (:greedy-repetition 0 1 ...)
;;; (* ...)        => (:greedy-repetition 0 nil ...)
;;; (+ ...)        => (:greedy-repetition 1 nil ...)
;;; (*? ...)       => (:non-greedy-repetition 0 nil ...)
;;; (+? ...)       => (:non-greedy-repetition 1 nil ...)
;;; ({ X } ...)    => (:greedy-repetition X nil ...)
;;; ({ X }? ...)   => (:non-greedy-repetition X nil ...)
;;; ({ X Y } ...)  => (:greedy-repetition X Y ...)
;;; ({ X Y }? ...) => (:non-greedy-repetition X Y ...)
;;; (/ ...)        => (:regex ...)
;;; ([] ...)       => (:char-class ...)
;;; ([^] ...)      => (:inverted-char-class ...)
;;; (- ...)        => (:range ...))
;;; ;; any combiniation of i/s/m/- works
;;; (?ism-ism ...) => (:group :case-insensitive-p :single-line-mode-p     :multi-line-mode-p
;;;                           :case-sensitive-p   :not-single-line-mode-p :not-multi-line-mode-p ...)
;;; (r ...)               => (:register (:sequence ...))
;;; (nr foo ...)          => (:name-register \"FOO\" (:sequence ...))
;;; (if 1 ...)            => (:branch 1 (:alternation ...))
;;; (if (?= AAA) BBB CCC) => (:branch (:positive-lookahead AAA) (:alternation BBB CCC))
;;; (...)          => (:sequence ...) *only when nothing else matches
"
  (list 'quote
        (let ((final (terse-ppcre::transform-regex-tree
                      (if (null (cdr regex))
                          (car regex)
                          regex))))
          (if (stringp final)
              (list :sequence final)
              final))))

(defmacro defre (name &rest parse-tree)
  "Define a parse tree synonym using the RE macro.

Example:

;;;  (defre anyjunk ?s (* any)) ; matches a series of any number of
;;;                            ; any characters, including newlines
;;; (scan (re \"A\" anyjunk \"Z\") \"A @#$&^[& Z\")
"
  `(setf (ppcre:parse-tree-synonym ',name) (re ,parse-tree)))

