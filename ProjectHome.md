TERSE-PPCRE allows you to use create CL-PPCRE parse trees using a simpler syntax that is still S-expression based.  It tries to mimic perl string regular expression syntax as closely as possible, although in some cases the lisp reader prevents us from doing that (for example, we can't use a period to represent any character, so ANY is used instead).

Hopefully using the RE macro shouldn't be too much more verbose than straight perl regex strings, and much easier to combine and manipulate.

Unfortunately I noticed DEFPATT (http://www.cl-user.net/asp/libs/DEFPATT) only after writing this.  Oh well, having too many libraries is not exactly Common Lisp's problem.

Author: Aaron Sokoloski (email to asokoloski (but no spam!) at gmail . com)