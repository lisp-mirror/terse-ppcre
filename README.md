# terse-ppcre

Automatically exported from code.google.com/p/terse-ppcre

TERSE-PPCRE allows you to use create CL-PPCRE parse trees using a simpler syntax that is still S-expression based. It tries to mimic perl string regular expression syntax as closely as possible, although in some cases the lisp reader prevents us from doing that (for example, we can't use a period to represent any character, so ANY is used instead).

Hopefully using the RE macro shouldn't be too much more verbose than straight perl regex strings, and much easier to combine and manipulate.

Unfortunately I noticed DEFPATT (http://www.cl-user.net/asp/libs/DEFPATT) only after writing this. Oh well, having too many libraries is not exactly Common Lisp's problem.

Author: Aaron Sokoloski (email to asokoloski (but no spam!) at gmail . com)

# TERSE-PPCRE Documentation

### MACRO: (RE terse-regex-tree)

Transform a terse regex parse tree into one that CL-PPCRE can understand. Note that you can enclose the regex as a list or not, either way should work.

Examples:

``` 
(re * d) ; same as "\d*"

(re (^ (or (+ "A") (+ "B")) $)) ; same as "^(?:A+|B+)$" ```

Symbolic substitutions: | any | :everything | |:----|:------------| | b | :word-boundary | | !b | :non-word-boundary | | d | :digit-class | | !d | :non-digit-class | | w | :word-char-class | | !w | :non-word-char-class | | s | :whitespace-char-class | | !s | :non-whitespace-char-class | | ^ | :start-anchor | | $ | :end-anchor | | a | :modeless-start-anchor | | z | :modeless-end-anchor | | zn | :modeless-end-anchor-no-newline | | /1 | (:back-reference 1) | | /hi | (:back-reference \"HI\") |

Combinations of (any d !d w !w s !s) and (? ? + +?), such as | d+ | (:greedy-repetition 1 nil :digit-class) | |:------|:----------------------------------------| | any? | (:greedy-repetition 0 1 :everything) | | s*? | (:non-greedy-repetition 0 nil :whitespace-char-class) |

List transformations: | (do ...) | (:sequence ...) | |:---------------|:----------------| | (all ...) | (:group ...) | | (g ...) | (:group ...) | | (or ...) | (:alternation ...) |

| (?= ...) | (:positive-lookahead ...) | |:---------------|:--------------------------| | (?! ...) | (:negative-lookahead ...) | | (?<= ...) | (:positive-lookbehind ...) | | (?

| (? ...) | (:greedy-repetition 0 1 ...) | |:---------------|:-----------------------------| | (* ...) | (:greedy-repetition 0 nil ...) | | (+ ...) | (:greedy-repetition 1 nil ...) | | (*? ...) | (:non-greedy-repetition 0 nil ...) | | (+? ...) | (:non-greedy-repetition 1 nil ...) | | ({ X } ...) | (:greedy-repetition X nil ...) | | ({ X }? ...) | (:non-greedy-repetition X nil ...) | | ({ X Y } ...) | (:greedy-repetition X Y ...) | | ({ X Y }? ...) | (:non-greedy-repetition X Y ...) |

| (/ ...) | (:regex ...) | |:---------------|:-------------|

| (.md ...) | (:char-class ...) | |:-----------------------|:------------------| | ([^] ...) | (:inverted-char-class ...) | | (- ...) | (:range ...)) |

;; any combiniation of ism- after a question mark (yes we actually parse the symbol name) | (?ism-ism ...) | (:group :case-insensitive-p :single-line-mode-p :multi-line-mode-p :case-sensitive-p :not-single-line-mode-p :not-multi-line-mode-p ...) | |:---------------|:--------------------------------------------------------------------------------------------------------------------------------------------|

| (r ...) | (:register (:sequence ...)) | |:----------------------|:----------------------------| | (nr foo ...) | (:name-register \"FOO\" (:sequence ...)) | | (if 1 ...) | (:branch 1 (:alternation ...)) | | (if (?= AAA) BBB CCC) | (:branch (:positive-lookahead AAA) (:alternation BBB CCC)) | | (...) | (:sequence ...) ;; only when nothing else matches |
```

### MACRO: (DEFRE name &body regex)

Define a parse tree synonym using the RE macro. Acts like the body regex is wrapped in (RE ...). Other than that, pretty much the same as #'ppcre:define-parse-tree-synonym.

Example:

``` 
(defre anyjunk ?s (* any)) ; matches a series of any number of ; any characters, including newlines

(scan (re \"A\" anyjunk \"Z\") \"A @#$&^[& Z\") 
```

# See also

https://common-lisp.net/project/cl-irregsexp/
