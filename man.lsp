;; 
;; A db with the definitions of newLisp functions from newLisp User Manual and Reference v.10.4.5 rev 2012-12-11 
;;
;; Copyright of the content from newLisp Manual and Reference 
;; © 2012 Lutz Mueller www.nuevatec.com. All rights reserved.
;; Permission is granted to copy, distribute and/or modify this document under the
;; terms of the GNU Free Documentation License,
;; Version 1.2 or any later version published by the Free Software Foundation;
;; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. 
;; The accompanying software is protected by the GNU General Public License V.3, June 2007.
;; newLISP is a registered trademark of Lutz Mueller.
;;
;; man.db created with manGen.lsp by Sven Oliver Severini (2013) 
;; Contact: http://www.contactify.com/26a0f
;;

;; the entry for replace is missing, couldn't load it with that entry inside.

;; function for the output
(define (man input)
  (println (man-content input))
  (setq output-man ""))

;; db in it's own context  
(context 'man-content)

(set '_! [text]!

syntax: (! str-shell-command [int-flags])

Executes the command in str-command by shelling out to the operating system and
executing. This function returns a different value depending on the host
operating system.

(! "vi")
(! "ls -ltr")

Use the exec function to execute a shell command and capture the standard
output or to feed standard input. The process function may be used to launch a
non-blocking child process and redirect std I/O and std error to pipes.

On Ms Windows the optional int-flags parameter takes process creation flags as
defined for the Windows CreateProcessA function to control various parameters
of process creation. The inclusion of this parameter – which also can be 0 –
forces a different creation of the process without a command shell window. This
parameter is ignored on Unix.

; on MS Windows
; close the console of the currently running newLISP process
(apply (import "kernel32" "FreeConsole"))

; start another process and wait for it to finish
(! "notepad.exe" 0)

(exit)

Without the additional parameter, the ! call would create a new command window
replacing the closed one.

Note that ! (exclamation mark) can be also be used as a command-line shell
operator by omitting the parenthesis and space after the !:

> !ls -ltr    ; executed in the newLISP shell window

Used in this way, the ! operator is not a newLISP function at all, but rather a
special feature of the newLISP command shell. The ! must be entered as the
first character on the command-line.[/text])

(set '_$ "{$\n\nsyntax: ($ int-idx)\n\nThe functions that use regular expressions (directory, ends-with, find, parse,\nregex, search, starts-with and replace) all bind their results to the\npredefined system variables $0, $1, $2\226\128\147$15 after or during the function's\nexecution. System variables can be treated the same as any other symbol. As an\nalternative, the contents of these variables may also be accessed by using ($\n0), ($ 1), ($ 2), etc. This method allows indexed access (i.e., ($ i), where i\nis an integer).\n\n(set 'str  \"http://newlisp.org:80\")\n(find \"http://(.*):(.*)\" str 0)  \226\134\146 0\n\n$0  \226\134\146 \"http://newlisp.org:80\"\n$1  \226\134\146 \"newlisp.org\"\n$2  \226\134\146 \"80\"\n\n($ 0)  \226\134\146 \"http://newlisp.org:80\"\n($ 1)  \226\134\146 \"newlisp.org\"\n($ 2)  \226\134\146 \"80\"")

(set '_& "{&\n\nsyntax: (& int-1 int-2 [int-3 ... ])\n\nA bitwise and operation is performed on the number in int-1 with the number in\nint-2, then successively with int-3, etc.\n\n(& 0xAABB 0x000F)  \226\134\146 11  ; which is 0xB")

(set '_++ "{++ !\n\nsyntax: (++ place [num ... ])\n\nThe ++ operator works like inc, but performs integer arithmetic. Without the\noptional argument in num, ++ increments the number in place by 1.\n\nIf floating point numbers are passed as arguments, their fractional part gets\ntruncated first.\n\nCalculations resulting in numbers greater than 9,223,372,036,854,775,807 wrap\naround to negative numbers. Results smaller than -9,223,372,036,854,775,808\nwrap around to positive numbers.\n\nplace is either a symbol or a place in a list structure holding a number, or a\nnumber returned by an expression.\n\n(set 'x 1)\n(++ x)        \226\134\146 2\n(set 'x 3.8)\n(++ x)        \226\134\146 4\n(++ x 1.3)    \226\134\146 5\n(set 'lst '(1 2 3))\n(++ (lst 1) 2))  \226\134\146 4\nlst              \226\134\146 (1 4 3)\n\nIf the symbol for place contains nil, it is treated as if containing 0.\n\nSee -- for decrementing numbers in integer mode. See inc for incrementing\nnumbers in floating point mode.")

(set '_+ "{+, -, *, / ,%\n\nsyntax: (+ int-1 [int-2 ... ])\n\nReturns the sum of all numbers in int-1 \226\128\148.\n\nsyntax: (- int-1 [int-2 ... ])\n\nSubtracts int-2 from int-1, then the next int-i from the previous result. If\nonly one argument is given, its sign is reversed.\n\nsyntax: (* int-1 [int-2 ... ])\n\nThe product is calculated for int-1 to int-i.\n\nsyntax: (/ int-1 [int-2 ... ])\n\nEach result is divided successively until the end of the list is reached.\nDivision by zero causes an error.\n\nsyntax: (% int-1 [int-2 ... ])\n\nEach result is divided successively by the next int, then the rest (modulo\noperation) is returned. Division by zero causes an error. For floating point\nnumbers, use the mod function.\n\n(+ 1 2 3 4 5)        \226\134\146 15\n(+ 1 2 (- 5 2) 8)    \226\134\146 14\n(- 10 3 2 1)         \226\134\146 4\n(- (* 3 4) 6 1 2)    \226\134\146 3\n(- 123)              \226\134\146 -123\n(map - '(10 20 30))  \226\134\146 (-10 -20 -30)\n(* 1 2 3)            \226\134\146 6\n(* 10 (- 8 2))       \226\134\146 60\n(/ 12 3)             \226\134\146 4\n(/ 120 3 20 2)       \226\134\146 1\n(% 10 3)             \226\134\146 1\n(% -10 3)            \226\134\146 -1\n(+ 1.2 3.9)          \226\134\146 4\n\nFloating point values in arguments to +, -, *, /, and % are truncated to the\ninteger value closest to 0 (zero).\n\nFloating point values larger or smaller than the maximum\n(9,223,372,036,854,775,807) or minimum (-9,223,372,036,854,775,808) integer\nvalues are truncated to those values. This includes the values for +Inf and\n-Inf.\n\nCalculations resulting in values larger than 9,223,372,036,854,775,807 or\nsmaller than -9,223,372,036,854,775,808 wrap around from positive to negative\nor negative to positive.\n\nFloating point values that evaluate to NaN (Not a Number), ar treated as 0\n(zero).")

(set '_- "{+, -, *, / ,%\n\nsyntax: (+ int-1 [int-2 ... ])\n\nReturns the sum of all numbers in int-1 \226\128\148.\n\nsyntax: (- int-1 [int-2 ... ])\n\nSubtracts int-2 from int-1, then the next int-i from the previous result. If\nonly one argument is given, its sign is reversed.\n\nsyntax: (* int-1 [int-2 ... ])\n\nThe product is calculated for int-1 to int-i.\n\nsyntax: (/ int-1 [int-2 ... ])\n\nEach result is divided successively until the end of the list is reached.\nDivision by zero causes an error.\n\nsyntax: (% int-1 [int-2 ... ])\n\nEach result is divided successively by the next int, then the rest (modulo\noperation) is returned. Division by zero causes an error. For floating point\nnumbers, use the mod function.\n\n(+ 1 2 3 4 5)        \226\134\146 15\n(+ 1 2 (- 5 2) 8)    \226\134\146 14\n(- 10 3 2 1)         \226\134\146 4\n(- (* 3 4) 6 1 2)    \226\134\146 3\n(- 123)              \226\134\146 -123\n(map - '(10 20 30))  \226\134\146 (-10 -20 -30)\n(* 1 2 3)            \226\134\146 6\n(* 10 (- 8 2))       \226\134\146 60\n(/ 12 3)             \226\134\146 4\n(/ 120 3 20 2)       \226\134\146 1\n(% 10 3)             \226\134\146 1\n(% -10 3)            \226\134\146 -1\n(+ 1.2 3.9)          \226\134\146 4\n\nFloating point values in arguments to +, -, *, /, and % are truncated to the\ninteger value closest to 0 (zero).\n\nFloating point values larger or smaller than the maximum\n(9,223,372,036,854,775,807) or minimum (-9,223,372,036,854,775,808) integer\nvalues are truncated to those values. This includes the values for +Inf and\n-Inf.\n\nCalculations resulting in values larger than 9,223,372,036,854,775,807 or\nsmaller than -9,223,372,036,854,775,808 wrap around from positive to negative\nor negative to positive.\n\nFloating point values that evaluate to NaN (Not a Number), ar treated as 0\n(zero).")

(set '_* "{+, -, *, / ,%\n\nsyntax: (+ int-1 [int-2 ... ])\n\nReturns the sum of all numbers in int-1 \226\128\148.\n\nsyntax: (- int-1 [int-2 ... ])\n\nSubtracts int-2 from int-1, then the next int-i from the previous result. If\nonly one argument is given, its sign is reversed.\n\nsyntax: (* int-1 [int-2 ... ])\n\nThe product is calculated for int-1 to int-i.\n\nsyntax: (/ int-1 [int-2 ... ])\n\nEach result is divided successively until the end of the list is reached.\nDivision by zero causes an error.\n\nsyntax: (% int-1 [int-2 ... ])\n\nEach result is divided successively by the next int, then the rest (modulo\noperation) is returned. Division by zero causes an error. For floating point\nnumbers, use the mod function.\n\n(+ 1 2 3 4 5)        \226\134\146 15\n(+ 1 2 (- 5 2) 8)    \226\134\146 14\n(- 10 3 2 1)         \226\134\146 4\n(- (* 3 4) 6 1 2)    \226\134\146 3\n(- 123)              \226\134\146 -123\n(map - '(10 20 30))  \226\134\146 (-10 -20 -30)\n(* 1 2 3)            \226\134\146 6\n(* 10 (- 8 2))       \226\134\146 60\n(/ 12 3)             \226\134\146 4\n(/ 120 3 20 2)       \226\134\146 1\n(% 10 3)             \226\134\146 1\n(% -10 3)            \226\134\146 -1\n(+ 1.2 3.9)          \226\134\146 4\n\nFloating point values in arguments to +, -, *, /, and % are truncated to the\ninteger value closest to 0 (zero).\n\nFloating point values larger or smaller than the maximum\n(9,223,372,036,854,775,807) or minimum (-9,223,372,036,854,775,808) integer\nvalues are truncated to those values. This includes the values for +Inf and\n-Inf.\n\nCalculations resulting in values larger than 9,223,372,036,854,775,807 or\nsmaller than -9,223,372,036,854,775,808 wrap around from positive to negative\nor negative to positive.\n\nFloating point values that evaluate to NaN (Not a Number), ar treated as 0\n(zero).")

(set '_/ "{+, -, *, / ,%\n\nsyntax: (+ int-1 [int-2 ... ])\n\nReturns the sum of all numbers in int-1 \226\128\148.\n\nsyntax: (- int-1 [int-2 ... ])\n\nSubtracts int-2 from int-1, then the next int-i from the previous result. If\nonly one argument is given, its sign is reversed.\n\nsyntax: (* int-1 [int-2 ... ])\n\nThe product is calculated for int-1 to int-i.\n\nsyntax: (/ int-1 [int-2 ... ])\n\nEach result is divided successively until the end of the list is reached.\nDivision by zero causes an error.\n\nsyntax: (% int-1 [int-2 ... ])\n\nEach result is divided successively by the next int, then the rest (modulo\noperation) is returned. Division by zero causes an error. For floating point\nnumbers, use the mod function.\n\n(+ 1 2 3 4 5)        \226\134\146 15\n(+ 1 2 (- 5 2) 8)    \226\134\146 14\n(- 10 3 2 1)         \226\134\146 4\n(- (* 3 4) 6 1 2)    \226\134\146 3\n(- 123)              \226\134\146 -123\n(map - '(10 20 30))  \226\134\146 (-10 -20 -30)\n(* 1 2 3)            \226\134\146 6\n(* 10 (- 8 2))       \226\134\146 60\n(/ 12 3)             \226\134\146 4\n(/ 120 3 20 2)       \226\134\146 1\n(% 10 3)             \226\134\146 1\n(% -10 3)            \226\134\146 -1\n(+ 1.2 3.9)          \226\134\146 4\n\nFloating point values in arguments to +, -, *, /, and % are truncated to the\ninteger value closest to 0 (zero).\n\nFloating point values larger or smaller than the maximum\n(9,223,372,036,854,775,807) or minimum (-9,223,372,036,854,775,808) integer\nvalues are truncated to those values. This includes the values for +Inf and\n-Inf.\n\nCalculations resulting in values larger than 9,223,372,036,854,775,807 or\nsmaller than -9,223,372,036,854,775,808 wrap around from positive to negative\nor negative to positive.\n\nFloating point values that evaluate to NaN (Not a Number), ar treated as 0\n(zero).")

(set '_% "{+, -, *, / ,%\n\nsyntax: (+ int-1 [int-2 ... ])\n\nReturns the sum of all numbers in int-1 \226\128\148.\n\nsyntax: (- int-1 [int-2 ... ])\n\nSubtracts int-2 from int-1, then the next int-i from the previous result. If\nonly one argument is given, its sign is reversed.\n\nsyntax: (* int-1 [int-2 ... ])\n\nThe product is calculated for int-1 to int-i.\n\nsyntax: (/ int-1 [int-2 ... ])\n\nEach result is divided successively until the end of the list is reached.\nDivision by zero causes an error.\n\nsyntax: (% int-1 [int-2 ... ])\n\nEach result is divided successively by the next int, then the rest (modulo\noperation) is returned. Division by zero causes an error. For floating point\nnumbers, use the mod function.\n\n(+ 1 2 3 4 5)        \226\134\146 15\n(+ 1 2 (- 5 2) 8)    \226\134\146 14\n(- 10 3 2 1)         \226\134\146 4\n(- (* 3 4) 6 1 2)    \226\134\146 3\n(- 123)              \226\134\146 -123\n(map - '(10 20 30))  \226\134\146 (-10 -20 -30)\n(* 1 2 3)            \226\134\146 6\n(* 10 (- 8 2))       \226\134\146 60\n(/ 12 3)             \226\134\146 4\n(/ 120 3 20 2)       \226\134\146 1\n(% 10 3)             \226\134\146 1\n(% -10 3)            \226\134\146 -1\n(+ 1.2 3.9)          \226\134\146 4\n\nFloating point values in arguments to +, -, *, /, and % are truncated to the\ninteger value closest to 0 (zero).\n\nFloating point values larger or smaller than the maximum\n(9,223,372,036,854,775,807) or minimum (-9,223,372,036,854,775,808) integer\nvalues are truncated to those values. This includes the values for +Inf and\n-Inf.\n\nCalculations resulting in values larger than 9,223,372,036,854,775,807 or\nsmaller than -9,223,372,036,854,775,808 wrap around from positive to negative\nor negative to positive.\n\nFloating point values that evaluate to NaN (Not a Number), ar treated as 0\n(zero).")

(set '_-- "{-- !\n\nsyntax: (-- place [num ... ])\n\nThe -- operator works like dec, but performs integer arithmetic. Without the\noptional argument in num-2, -- decrements the number in place by 1.\n\nIf floating point numbers are passed as arguments, their fractional part gets\ntruncated first.\n\nCalculations resulting in numbers greater than 9,223,372,036,854,775,807 wrap\naround to negative numbers. Results smaller than -9,223,372,036,854,775,808\nwrap around to positive numbers.\n\nplace is either a symbol or a place in a list structure holding a number, or a\nnumber returned by an expression.\n\n(set 'x 1)\n(-- x)        \226\134\146 0\n(set 'x 3.8)\n(-- x)        \226\134\146 2\n(-- x 1.3)    \226\134\146 1\n\n(set 'lst '(1 2 3))\n(-- (lst 1) 2))  \226\134\146 0\nlst              \226\134\146 (1 0 3)\n\nIf the symbol for place contains nil, it is treated as if containing 0.\n\nSee ++ for incrementing numbers in integer mode. See dec for decrementing\nnumbers in floating point mode.")

(set '_!= [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])

(set '_>= [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])

(set '_<= [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])

(set '_= [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])

(set '_> [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])

(set '_< [text]<, >, =, <=, >=, !=

syntax: (< exp-1 [exp-2 ... ])
syntax: (> exp-1 [exp-2 ... ])
syntax: (= exp-1 [exp-2 ... ])
syntax: (<= exp-1 [exp-2 ... ])
syntax: (>= exp-1 [exp-2 ... ])
syntax: (!= exp-1 [exp-2 ... ])

Expressions are evaluated and the results are compared successively. As long as
the comparisons conform to the comparison operators, evaluation and comparison
will continue until all arguments are tested and the result is true. As soon as
one comparison fails, nil is returned.

If only one argument is supplied, all comparison operators assume 0 (zero) as a
second argument. This can be used to check if a numer is negative, positive,
zero or not zero.

All types of expressions can be compared: atoms, numbers, symbols, and strings.
List expressions can also be compared (list elements are compared recursively).

When comparing lists, elements at the beginning of the list are considered more
significant than the elements following (similar to characters in a string).
When comparing lists of different lengths but equal elements, the longer list
is considered greater (see examples).

In mixed-type expressions, the types are compared from lowest to highest.
Floats and integers are compared by first converting them to the needed type,
then comparing them as numbers.

    Atoms: nil, true, integer or float, string, symbol, primitive
    Lists: quoted list/expression, list/expression, lambda, lambda-macro

(< 3 5 8 9)                     → true
(> 4 2 3 6)                     → nil
(< "a" "c" "d")                 → true
(>= duba aba)                   → true
(< '(3 4) '(1 5))               → nil
(> '(1 2 3) '(1 2))             → true
(= '(5 7 8) '(5 7 8))           → true
(!= 1 4 3 7 3)                  → true
(< 1.2 6 "Hello" 'any '(1 2 3))           → true
(< nil true)                              → true
(< '(((a b))) '(((b c))))                 → true
(< '((a (b c)) '(a (b d)) '(a (b (d)))))  → true

; with single argument compares against 0

(> 1)    → true ; checks for positive
(> -1)   → nil ; checks for negative
(= 123)  → nil ; checks for zero

(map > '(1 3 -4 -3 1 2))   → (true true nil nil true true)[/text])


(set  (sym "_:" MAIN:man-content)  "{:\n\nsyntax: (: sym-function list-object [ ... ])\n\nThe colon is used not only as a syntactic separator between between namespace\nprefix and the term inside but also as an operator. When used as an operator,\nthe colon : constructs a context symbol from the context name in the object\nlist and the symbol following the colon. The object list in list-object can be\nfollowed by other parameters.\n\nThe : operator implements polymorphism of object methods, which are part of\ndifferent object classes represented by contexts (namespaces). In newLISP, an\nobject is represented by a list, the first element of which is the symbol\n(name) of its class context. The class context implements the functions\napplicable to the object. No space is required between the colon and the symbol\nfollowing it.\n\n(define (Rectangle:area)\n    (mul (self 3) (self 4)))\n\n(define (Circle:area)\n    (mul (pow (self 3) 2) (acos 0) 2))\n\n(define (Rectangle:move dx dy)\n    (inc (self 1) dx)\n        (inc (self 2) dy))\n\n(define (Circle:move p dx dy)\n    (inc (self 1) dx) (inc (self 2) dy))\n\n(set 'myrect '(Rectangle 5 5 10 20)) ; x y width height\n(set 'mycircle '(Circle 1 2 10)) ; x y radius\n\n;; using the : (colon) operator to resolve to a specific context\n\n(:area myrect)     \226\134\146 200\n(:area mycircle)   \226\134\146 314.1592654\n\n;; map class methods uses curry to enclose the colon operator and class function\n\n(map (curry :area) (list myrect mycircle)) \226\134\146 (200 314.1592654)\n\n(map (curry :area) '((Rectangle 5 5 10 20) (Circle 1 2 10))) \226\134\146 (200 314.1592654)\n\n;; change object attributes using a function and re-assigning\n;; to the objects name\n\n(:move myrect 2 3)\nmyrect   \226\134\146 (Rectangle 7 8 10 20)\n\n(:move mycircle 4 5)\nmycircle \226\134\146 (Circle 5 7 10)\n\nInside the FOOP methods the self function is used to access the target object\nof the method.")

(set  (sym "_<<" MAIN:man-content)  "{<<, >>\n\nsyntax: (<< int-1 int-2 [int-3 ... ])\nsyntax: (>> int-1 int-2 [int-3 ... ])\nsyntax: (<< int-1)\nsyntax: (>> int-1)\n\nThe number int-1 is arithmetically shifted to the left or right by the number\nof bits given as int-2, then shifted by int-3 and so on. For example, 64-bit\nintegers may be shifted up to 63 positions. When shifting right, the most\nsignificant bit is duplicated (arithmetic shift):\n\n(>> 0x8000000000000000 1)  \226\134\146 0xC000000000000000  ; not 0x0400000000000000!\n\n\n(<< 1 3)      \226\134\146  8\n(<< 1 2 1)    \226\134\146  8\n(>> 1024 10)  \226\134\146  1\n(>> 160 2 2)  \226\134\146 10\n\n(<< 3)        \226\134\146  6\n(>> 8)        \226\134\146  4\n\nWhen int-1 is the only argument << and >> shift by one bit.")

(set  (sym "_>>" MAIN:man-content)  "{<<, >>\n\nsyntax: (<< int-1 int-2 [int-3 ... ])\nsyntax: (>> int-1 int-2 [int-3 ... ])\nsyntax: (<< int-1)\nsyntax: (>> int-1)\n\nThe number int-1 is arithmetically shifted to the left or right by the number\nof bits given as int-2, then shifted by int-3 and so on. For example, 64-bit\nintegers may be shifted up to 63 positions. When shifting right, the most\nsignificant bit is duplicated (arithmetic shift):\n\n(>> 0x8000000000000000 1)  \226\134\146 0xC000000000000000  ; not 0x0400000000000000!\n\n\n(<< 1 3)      \226\134\146  8\n(<< 1 2 1)    \226\134\146  8\n(>> 1024 10)  \226\134\146  1\n(>> 160 2 2)  \226\134\146 10\n\n(<< 3)        \226\134\146  6\n(>> 8)        \226\134\146  4\n\nWhen int-1 is the only argument << and >> shift by one bit.")

(set '_NaN? "{NaN?\n\nsyntax: (NaN? float)\n\nTests if the result of a floating point math operation is a NaN. Certain\nfloating point operations return a special IEEE 754 number format called a NaN\nfor 'Not a Number'.\n\n; floating point operation on NaN yield NaN\n(set 'x (sqrt -1))  \226\134\146 NaN\n(NaN? x)            \226\134\146 true\n(add x 123)         \226\134\146 NaN\n(mul x 123)         \226\134\146 NaN\n\n; integer operations treat NaN as zero\n(+ x 123)  \226\134\146 123\n(* x 123)  \226\134\146 0\n\n; comparisons with NaN values yield nil\n(> x 0)   \226\134\146 nil\n(<= x 0)  \226\134\146 nil\n(= x x)   \226\134\146 nil\n\n(set 'infinity (mul 1.0e200 1.0e200)) \226\134\146 inf\n(NaN? (sub infinity infinity)) \226\134\146 true\n\nNote that all floating point arithmetic operations with a NaN yield a NaN. All\ncomparisons with NaN return nil, but true when comparing to itself. Comparison\nwith itself, however, would result in not true when using ANSI C. Integer\noperations treat NaN as 0 (zero) values.\n\nSee also inf? for testing a floating point value for infinity.")

(set '_^ "{^\n\nsyntax: (^ int-1 int-2 [int-3 ... ])\n\nA bitwise xor operation is performed on the number in int-1 with the number in\nint-2, then successively with int-3, etc.\n\n(^ 0xAA 0x55)  \226\134\146 255")

(set '_abort "{abort\n\nsyntax: (abort int-pid)\nsyntax: (abort)\n\nIn the first form, abort aborts a specific child process of the current parent\nprocess giving the process id in int-pid. The process must have been started\nusing spawn. For processes started using fork, use destroy instead.\n\nThe function abort is not available on Win32.\n\n(abort 2245)  \226\134\146 true\n\nTo abort all child processes spawned from the current process use abort without\nany parameters:\n\n(abort)  \226\134\146 true ; abort all\n\nThe function abort is part of the Cilk API for synchronizing child processes\nand process parallelization. See the reference for the function spawn for a\nfull discussion of the Cilk API.")

(set '_abs "{abs\n\nsyntax: (abs num)\n\nReturns the absolute value of the number in num.\n\n(abs -3.5)  \226\134\146 3.5")

(set '_acos "{acos\n\nsyntax: (acos num-radians)\n\nThe arc-cosine function is calculated from the number in num-radians.\n\n(acos 1)  \226\134\146 0\n(cos (acos 1)) \226\134\146 1")

(set '_acosh "{acosh\n\nsyntax: (acosh num-radians)\n\nCalculates the inverse hyperbolic cosine of num-radians, the value whose\nhyperbolic cosine is num-radians. If num-radians is less than 1, acosh returns\nNaN.\n\n(acosh 2)  \226\134\146 1.316957897\n(cosh (acosh 2)) \226\134\146 2\n(acosh 0.5) \226\134\146 NaN")

(set '_add "{add\n\nsyntax: (add num-1 [num-2 ... ])\n\nAll of the numbers in num-1, num-2, and on are summed. add accepts float or\ninteger operands, but it always returns a floating point number. Any floating\npoint calculation with NaN also returns NaN.\n\n(add 2 3.25 9)   \226\134\146 14.25\n(add 1 2 3 4 5)  \226\134\146 15")

(set '_address "{address\n\nsyntax: (address int)\nsyntax: (address float)\nsyntax: (address str)\n\nReturns the memory address of the integer in int, the double floating point\nnumber in float, or the string in str. This function is used for passing\nparameters to library functions that have been imported using the import\nfunction.\n\n(set 's \"\\001\\002\\003\\004\")\n\n(get-char (+ (address s) 3))   \226\134\146 4\n\n(set 'x 12345) ; x is a 64-bit long int\n\n; on a big-endian CPU, i.e. PPC or SPARC\n(get-long (address x))         \226\134\146 12345\n; the 32-bit int is in high 32-bit part of the long int\n(get-int (+ (address x) 4))    \226\134\146 12345\n\n; on a little-endian CPU, i.e. Intel i386\n; the 32-bit int is in the low 32-bit part of the long int\n(get-int (address x))          \226\134\146 12345\n\n; on both architectures (integers are 64 bit in newLISP)\n(set 'x 1234567890)\n(get-long (address x))         \226\134\146  1234567890\n\n\nWhen a string is passed to C library function the address of the string is used\nautomatically, and it is not necessary to use the address function in that\ncase. As the example shows, address can be used to do pointer arithmetic on the\nstring's address.\n\naddress should only be used on persistent addresses from data objects referred\nto by a variable symbol, not from volatile intermediate expression objects.\n\nSee also the get-char, get-int, get-long and get-float functions.")

(set '_amb "{amb\n\nsyntax: (amb exp-1 [exp-2 ... ])\n\nOne of the expressions exp-1 ... n is selected at random, and the evaluation\nresult is returned.\n\n(amb 'a 'b 'c 'd 'e)  \226\134\146 one of: a, b, c, d, or e at random\n\n(dotimes (x 10) (print (amb 3 5 7)))  \226\134\146 35777535755\n\nInternally, newLISP uses the same function as rand to pick a random number. To\ngenerate random floating point numbers, use random, randomize, or normal. To\ninitialize the pseudo random number generating process at a specific starting\npoint, use the seed function.")

(set '_and "{and\n\nsyntax: (and exp-1 [exp-2 ... ])\n\nThe expressions exp-1, exp-2, etc. are evaluated in order, returning the result\nof the last expression. If any of the expressions yield nil or the empty list\n(), evaluation is terminated and nil or the empty list () is returned.\n\n(set 'x 10)                       \226\134\146 10\n(and (< x 100) (> x 2))           \226\134\146 true\n(and (< x 100) (> x 2) \"passed\")  \226\134\146 \"passed\"\n(and '())                         \226\134\146 ()\n(and true)                        \226\134\146 true\n(and)                             \226\134\146 true")

(set '_append "{append\n\nsyntax: (append list-1 [list-2 ... ])\nsyntax: (append array-1 [array-2 ... ])\nsyntax: (append str-1 [str-2 ... ])\n\nIn the first form, append works with lists, appending list-1 through list-n to\nform a new list. The original lists are left unchanged.\n\n(append '(1 2 3) '(4 5 6) '(a b))  \226\134\146 (1 2 3 4 5 6 a b)\n\n(set 'aList '(\"hello\" \"world\"))    \226\134\146 (\"hello\" \"world\")\n\n(append aList '(\"here\" \"I am\"))    \226\134\146 (\"hello\" \"world\" \"here\" \"I am\")\n\nIn the second form append works on arrays:\n\n(set 'A (array 3 2 (sequence 1 6)))\n\226\134\146 ((1 2) (3 4) (5 6))\n(set 'B (array 2 2 (sequence 7 10)))\n\226\134\146 ((7 8) (9 10))\n\n(append A B)\n\226\134\146 ((1 2) (3 4) (5 6) (7 8) (9 10))\n\n(append B B B)\n\226\134\146 ((7 8) (9 10) (7 8) (9 10) (7 8) (9 10))\n\n\nIn the third form, append works on strings. The strings in str-n are\nconcatenated into a new string and returned.\n\n(set 'more \" how are you\")       \226\134\146 \" how are you\"\n\n(append \"Hello \" \"world,\" more)  \226\134\146 \"Hello world, how are you\"\n\nappend is also suitable for processing binary strings containing zeroes. The\nstring function would cut of strings at zero bytes.\n\nLinkage characters or strings can be specified using the join function. Use the\nstring function to convert arguments to strings and append in one step.\n\nUse the functions extend and push to append to an existing list or string\nmodifying the target.")

(set '_append-file "{append-file\n\nsyntax: (append-file str-filename str-buffer)\n\nWorks similarly to write-file, but the content in str-buffer is appended if the\nfile in str-filename exists. If the file does not exist, it is created (in this\ncase, append-file works identically to write-file). This function returns the\nnumber of bytes written.\n\nOn failure the function returns nil. For error information, use sys-error when\nused on files. When used on URLs net-error gives more error information.\n\n(write-file \"myfile.txt\" \"ABC\")\n(append-file \"myfile.txt\" \"DEF\")\n\n(read-file \"myfile.txt\")  \226\134\146 \"ABCDEF\"\n\nappend-file can take a http:// or file:// URL in str-file-name. In case of the\nhttp:// prefix , append-file works exactly like put-url with \"Pragma: append\\r\\\nn\" in the header option and can take the same additional parameters. The\n\"Pragma: append\\r\\n\" option is supplied automatically.\n\n(append-file \"http://asite.com/message.txt\" \"More message text.\")\n\nThe file message.txt is appended at a remote location http://asite.com with the\ncontents of str-buffer. If the file does not yet exist, it will be created. In\nthis mode, append-file can also be used to transfer files to remote newLISP\nserver nodes.\n\nSee also read-file and write-file.")

(set '_apply "{apply\n\nsyntax: (apply func list [int-reduce])\n\nApplies the contents of func (primitive, user-defined function, or lambda\nexpression) to the arguments in list.\n\n(apply + '(1 2 3 4))                   \226\134\146 10\n(set 'aList '(3 4 5))                  \226\134\146 (3 4 5)\n(apply * aList)                        \226\134\146 60\n(apply sqrt '(25))                     \226\134\146 5\n(apply (lambda (x y) (* x y)) '(3 4))  \226\134\146 12\n\nThe int-reduce parameter can optionally contain the number of arguments taken\nby the function in func. In this case, func will be repeatedly applied using\nthe previous result as the first argument and taking the other arguments\nrequired successively from list (in left-associative order). For example, if op\ntakes two arguments, then:\n\n(apply op '(1 2 3 4 5) 2)\n\n;; is equivalent to\n\n(op (op (op (op 1 2) 3) 4) 5)\n\n;; find the greatest common divisor\n;; of two or more integers\n;; note that newLISP already has a gcd function\n\n(define (gcd_ a b)\n    (let (r (% b a))\n        (if (= r 0) a (gcd_ r a))))\n\n(define-macro (my-gcd)\n    (apply gcd_ (args) 2))\n\n(my-gcd 12 18 6)    \226\134\146 6\n(my-gcd 12 18 6 4)  \226\134\146 2\n\nThe last example shows how apply's reduce functionality can be used to convert\na two-argument function into one that takes multiple arguments. Note, that a\nbuilt-in gcd is available.\n\napply should only be used on functions and operators that evaluate all of their\narguments, not on special forms like dotimes or case, which evaluate only some\nof their arguments. Doing so will cause the function to fail.")

(set '_args "{args\n\nsyntax: (args)\nsyntax: (args int-idx-1 [int-idx-2 ... ])\n\nAccesses a list of all unbound arguments passed to the currently evaluating\ndefine, define-macro lambda, or lambda-macro expression. Only the arguments of\nthe current function or macro that remain after local variable binding has\noccurred are available. The args function is useful for defining functions or\nmacros with a variable number of parameters.\n\nargs can be used to define hygienic macros that avoid the danger of variable\ncapture. See define-macro.\n\n(define-macro (print-line)\n    (dolist (x (args))\n        (print x \"\\n\")))\n\n(print-line \"hello\" \"World\")\n\nThis example prints a line-feed after each argument. The macro mimics the\neffect of the built-in function println.\n\nIn the second syntax, args can take one or more indices (int-idx-n).\n\n(define-macro (foo)\n    (print (args 2) (args 1) (args 0)))\n\n(foo x y z)\nzyx\n\n(define (bar)\n        (args 0 2 -1))\n\n(bar '(1 2 (3 4)))  \226\134\146 4\n\nThe function foo prints out the arguments in reverse order. The bar function\nshows args being used with multiple indices to access nested lists.\n\nRemember that (args) only contains the arguments not already bound to local\nvariables of the current function or macro:\n\n(define (foo a b) (args))\n\n(foo 1 2)        \226\134\146 ()\n\n(foo 1 2 3 4 5)  \226\134\146 (3 4 5)\n\nIn the first example, an empty list is returned because the arguments are bound\nto the two local symbols, a and b. The second example demonstrates that, after\nthe first two arguments are bound (as in the first example), three arguments\nremain and are then returned by args.\n\n(args) can be used as an argument to a built-in or user-defined function call,\nbut it should not be used as an argument to another macro, in which case (args)\nwould not be evaluated and would therefore have the wrong contents in the new\nmacro environment.")

(set '_array [text]{array

syntax: (array int-n1 [int-n2 ... ] [list-init])

Creates an array with int-n1 elements, optionally initializing it with the
contents of list-init. Up to sixteen dimensions may be specified for
multidimensional arrays.

Internally, newLISP builds multidimensional arrays by using arrays as the
elements of an array. newLISP arrays should be used whenever random indexing
into a large list becomes too slow. Not all list functions may be used on
arrays. For a more detailed discussion, see the chapter on arrays.

(array 5)                  → (nil nil nil nil nil)

(array 5 (sequence 1  5))  → (1 2 3 4 5)

(array 10 '(1 2))          → (1 2 1 2 1 2 1 2 1 2)

Arrays can be initialized with objects of any type. If fewer initializers than
elements are provided, the list is repeated until all elements of the array are
initialized.


(set 'myarray (array 3 4 (sequence 1 12)))
→ ((1 2 3 4) (5 6 7 8) (9 10 11 12))

Arrays are modified and accessed using most of the same functions used for
modifying lists:

(setf (myarray 2 3) 99) → 99)
myarray → ((1 2 3 4) (5 6 7 8) (9 10 11 99))

(setf (myarray 1 1) "hello")  → "hello"

myarray → ((1 2 3 4) (5 "hello" 7 8) (9 10 11 99))

(setf (myarray 1) '(a b c d)) → (a b c d)
myarray → ((1 2 3 4) (a b c d) (9 10 11 99))

(nth 1 myarray)     → (a b c d)  ; access a whole row

(myarray 0 -1)      → 4

;; use implicit indexing and slicing on arrays

(myarray 1)     → (a b c d)

(myarray 0 -1)  → 4

(2 myarray)     → (c d)

(-3 2 myarray)  → (b c)

Care must be taken to use an array when replacing a whole row.

array-list can be used to convert arrays back into lists:

(array-list myarray)  → ((1 2 3 4) (a b c d) (1 2 3 99))

To convert a list back into an array, apply flat to the list:

(set 'aList '((1 2) (3 4)))             → ((1 2) (3 4))

(set 'aArray (array 2 2 (flat aList)))  → ((1 2) (3 4))

The array? function can be used to check if an expression is an array:

(array? myarray)               → true

(array? (array-list myarray))  → nil

When serializing arrays using the function source or save, the generated code
includes the array statement necessary to create them. This way, variables
containing arrays are correctly serialized when saving with save or creating
source strings using source.

(set 'myarray (array 3 4 (sequence 1 12)))

(save "array.lsp" 'myarray)

;; contents of file arraylsp ;;

(set 'myarray (array 3 4 (flat '(
  (1 2 3 4)
  (5 6 7 8)
  (9 10 11 12)))))[/text])

(set '_array-list "{array-list\n\nsyntax: (array-list array)\n\nReturns a list conversion from array, leaving the original array unchanged:\n\n(set 'myarray (array 3 4 (sequence 1 12)))\n\226\134\146 ((1 2 3 4) (5 6 7 8) (9 10 11 12))\n\n(set 'mylist (array-list myarray))\n\226\134\146 ((1 2 3 4) (5 6 7 8) (9 10 11 12))\n\n(list (array? myarray) (list? mylist))\n\226\134\146 (true true)")

(set '_array? "{array?\n\nsyntax: (array? exp)\n\nChecks if exp is an array:\n\n(set 'M (array 3 4 (sequence 1 4)))\n\226\134\146 ((1 2 3 4) (1 2 3 4) (1 2 3 4)))\n\n\n(array? M)               \226\134\146 true\n\n(array? (array-list M))  \226\134\146 nil")

(set '_asin "{asin\n\nsyntax: (asin num-radians)\n\nCalculates the arcsine function from the number in num-radians and returns the\nresult.\n\n(asin 1)  \226\134\146 1.570796327\n(sin (asin 1)) \226\134\146 1")

(set '_asinh "{asinh\n\nsyntax: (asinh num-radians)\n\nCalculates the inverse hyperbolic sine of num-radians, the value whose\nhyperbolic sine is num-radians.\n\n(asinh 2)         \226\134\146 1.443635475\n(sinh (asinh 2))  \226\134\146 2")

(set '_assoc "{assoc\n\nsyntax: (assoc exp-key list-alist)\nsyntax: (assoc list-exp-key list-alist)\n\nIn the first syntax the value of exp-key is used to search list-alist for a\nmember-list whose first element matches the key value. If found, the\nmember-list is returned; otherwise, the result will be nil.\n\n(assoc 1 '((3 4) (1 2)))  \226\134\146 (1 2)\n\n(set 'data '((apples 123) (bananas 123 45) (pears 7)))\n\n(assoc 'bananas data)  \226\134\146 (bananas 123 45)\n(assoc 'oranges data)  \226\134\146 nil\n\nTogether with setf assoc can be used to change an association.\n\n(setf (assoc 'pears data) '(pears 8))\n\ndata  \226\134\146 ((apples 123) (bananas 123 45) (pears 8))\n\nIn the second syntax more then one key expressions can be specified to search\nin nested, multilevel association lists:\n\n(set 'persons '(\n    (id001 (name \"Anne\") (address (country \"USA\") (city \"New York\")))\n    (id002 (name \"Jean\") (address (country \"France\") (city \"Paris\")))\n))\n\n(assoc '(id001 address) persons) \226\134\146 (address (country \"USA\") (city \"New York\"))\n(assoc '(id001 address city) persons) \226\134\146 (city \"New York\")\n\nThe list in list-aList can be a context which will be interpreted as its\ndefault functor. This way very big lists can be passed by reference for\nspeedier access and less memory usage:\n\n(set 'persons:persons '(\n    (id001 (name \"Anne\") (address (country \"USA\") (city \"New York\")))\n    (id002 (name \"Jean\") (address (country \"France\") (city \"Paris\")))\n))\n\n(define (get-city db id)\n    (last (assoc (list id 'address 'city) db ))\n)\n\n(get-city persons 'id001) \226\134\146 \"New York\"\n\nFor making replacements in association lists, use the setf together with the\nassoc function. The lookup function is used to perform association lookup and\nelement extraction in one step.")

(set '_atan "{atan\n\nsyntax: (atan num-radians)\n\nThe arctangent of num-radians is calculated and returned.\n\n(atan 1)        \226\134\146 0.7853981634\n(tan (atan 1))  \226\134\146 1")

(set '_atan2 "{atan2\n\nsyntax: (atan2 num-Y-radians num-X-radians)\n\nThe atan2 function computes the principal value of the arctangent of Y / X in\nradians. It uses the signs of both arguments to determine the quadrant of the\nreturn value. atan2 is useful for converting Cartesian coordinates into polar\ncoordinates.\n\n(atan2 1 1)                       \226\134\146 0.7853981634\n(div (acos 0) (atan2 1 1))        \226\134\146 2\n(atan2 0 -1)                      \226\134\146 3.141592654\n(= (atan2 1 2) (atan (div 1 2)))  \226\134\146 true")

(set '_atanh "{atanh\n\nsyntax: (atanh num-radians)\n\nCalculates the inverse hyperbolic tangent of num-radians, the value whose\nhyperbolic tangent is num-radians. If the absolute value of num-radians is\ngreater than 1, atanh returns NaN; if it is equal to 1, atanh returns infinity.\n\n(atanh 0.5) \226\134\146 0.5493061443\n(tanh (atanh 0.5)) \226\134\146 0.5\n(atanh 1.1) \226\134\146 NaN\n(atanh 1) \226\134\146 inf")

(set '_atom? "{atom?\n\nsyntax: (atom? exp)\n\nReturns true if the value of exp is an atom, otherwise nil. An expression is an\natom if it evaluates to nil, true, an integer, a float, a string, a symbol or a\nprimitive. Lists, lambda or lambda-macro expressions, and quoted expressions\nare not atoms.\n\n(atom? '(1 2 3))      \226\134\146 nil\n(and (atom? 123)\n     (atom? \"hello\")\n     (atom? 'foo))    \226\134\146 true\n(atom? ''foo)         \226\134\146 nil")

(set '_base64-dec "{base64-dec\n\nsyntax: (base64-dec str)\n\nThe BASE64 string in str is decoded. Note that str is not verified to be a\nvalid BASE64 string. The decoded string is returned.\n\n(base64-dec \"SGVsbG8gV29ybGQ=\")  \226\134\146 \"Hello World\"\n\nFor encoding, use the base64-enc function.\n\nnewLISP's BASE64 handling is derived from routines found in the Unix curl\nutility and conforms to the RFC 4648 standard.")

(set '_base64-enc "{base64-enc\n\nsyntax: (base64-enc str [bool-flag])\n\nThe string in str is encoded into BASE64 format. This format encodes groups of\n3 * 8 = 24 input bits into 4 * 8 = 32 output bits, where each 8-bit output\ngroup represents 6 bits from the input string. The 6 bits are encoded into 64\npossibilities from the letters A\226\128\147Z and a\226\128\147z; the numbers 0\226\128\1479; and the characters\n+ (plus sign) and / (slash). The = (equals sign) is used as a filler in unused\n3- to 4-byte translations. This function is helpful for converting binary\ncontent into printable characters.\n\nWithout the optional bool-flag parameter the empty string \"\" is encoded into \"=\n===\". If bool-flag evaluates to true, the the empty string \"\" is translated\ninto \"\". Both translations result in \"\" when using base64-dec.\n\nThe encoded string is returned.\n\nBASE64 encoding is used with many Internet protocols to encode binary data for\ninclusion in text-based messages (e.g., XML-RPC).\n\n(base64-enc \"Hello World\")  \226\134\146 \"SGVsbG8gV29ybGQ=\"\n\n(base64-enc \"\")             \226\134\146 \"====\"\n(base64-enc \"\" true)        \226\134\146 \"\"\n\nNote that base64-enc does not insert carriage-return/line-feed pairs in longer\nBASE64 sequences but instead returns a pure BASE64-encoded string.\n\nFor decoding, use the base64-dec function.\n\nnewLISP's BASE64 handling is derived from routines found in the Unix curl\nutility and conforms to the RFC 4648 standard.")

(set '_bayes-query [text]{bayes-query

syntax: (bayes-query list-L context-D [bool-chain [bool-probs]])

Takes a list of tokens (list-L) and a trained dictionary (context-D) and
returns a list of the combined probabilities of the tokens in one category (A
or Mc) versus a category (B) against all other categories (Mi). All tokens in
list-L should occur in context-D. When using the default R.A. Fisher Chi² mode,
nonexistent tokens will skew results toward equal probability in all
categories.

Non-existing tokens will not have any influence on the result when using the
true Chain Bayesian mode with bool-chain set to true. The optional last flag,
bool-probs, indicates whether frequencies or probability values are used in the
data set. The bayes-train function is typically used to generate a data set's
frequencies.

Tokens can be strings or symbols. If strings are used, they are prepended with
an underscore before being looked up in context-D. If bayes-train was used to
generate context-D's frequencies, the underscore was automatically prepended
during the learning process.

Depending on the flag specified in bool-probs, bayes-query employs either the
R. A. Fisher Chi² method of compounding probabilities or the Chain Bayesian
method. By default, when no flag or nil is specified in bool-probs, the Chi²
method of compounding probabilities is used. When specifying true in bool-probs
, the Chain Bayesian method is used.

If the R.A. Fisher Chi² method is used, the total number of tokens in the
different training set's categories should be equal or similar. Uneven
frequencies in categories will skew the results.

For two categories A and B, bayes-query uses the following formula:

p(A|tkn) = p(tkn|A) * p(A) / p(tkn|A) * p(A) + p(tkn|B) * p(B)

For N categories, this formula is used:

p(Mc|tkn) = p(tkn|Mc) * p(Mc) / sum-i-N( p(tkn|Mi) * p(Mi) )

The probabilities (p(Mi) or p(A), along with p(B)) represent the Bayesian prior
probabilities. p(Mc|tkn) and p(A|tkn) are the posterior Bayesian probabilities
of a category or model.

Priors are handled differently, depending on whether the R.A. Fisher Chi² or
the Chain Bayesian method is used. In Chain Bayesian mode, posteriors from one
token calculation get the priors in the next calculation. In the default R.A.
Fisher method, priors are not passed on via chaining, but probabilities are
compounded using the Chi² method.

In Chain Bayes mode, tokens with zero frequency in one category will
effectively put the probability of that category to 0 (zero). This also causes
all posterior priors to be set to 0 and the category to be completely
suppressed in the result. Queries resulting in zero probabilities for all
categories yield NaN values.

The default R.A. Fisher Chi² method is less sensitive about zero frequencies
and still maintains a low probability for that token. This may be an important
feature in natural language processing when using Bayesian statistics. Imagine
that five different language corpus categories have been trained, but some
words occurring in one category are not present in another. When the pure Chain
Bayesian method is used, a sentence could never be classified into its correct
category because the zero-count of just one word token could effectively
exclude it from the category to which it belongs.

On the other hand, the Chain Bayesian method offers exact results for specific
proportions in the data. When using Chain Bayesian mode for natural language
data, all zero frequencies should be removed from the trained dictionary first.

The return value of bayes-query is a list of probability values, one for each
category. Following are two examples: the first for the default R.A. Fisher
mode, the second for a data set processed with the Chain Bayesian method.

Previous to version 10.3.0 the list of probability values returned in Fisher
Chi² mode was normalized by dividing each value by the sum of the whole list.
This normalization has been dropped in version 10.3.0.


R.A. Fisher Chi² method

In the following example, the two data sets are books from Project Gutenberg.
We assume that different authors use certain words with different frequencies
and want to determine if a sentence is more likely to occur in one or the other
author's writing. A similar method is frequently used to differentiate between
spam and legitimate email.

;; from Project Gutenberg: http://www.gutenberg.org/catalog/
;; The Adventures of Sherlock Holmes - Sir Arthur Conan Doyle

(bayes-train (parse (lower-case (read-file "Doyle.txt"))
                    "[^a-z]+" 0) '() 'DoyleDowson)

;; A Comedy of Masks - Ernest Dowson and Arthur Moore

(bayes-train '() (parse (lower-case (read-file "Dowson.txt"))
                    "[^a-z]+" 0) 'DoyleDowson)

(save "DoyleDowson.lsp" 'DoyleDowson)

The two training sets are loaded, split into tokens, and processed by the
bayes-train function. In the end, the DoyleDowson dictionary is saved to a
file, which will be used later with the bayes-query function.

The following code illustrates how bayes-query is used to classify a sentence
as Doyle or Dowson:

(load "DoyleDowson.lsp")
(bayes-query (parse "he was putting the last touches to a picture")
    'DoyleDowson)
→ (0.03802079132 0.9619792087)

(bayes-query (parse "immense faculties and extraordinary powers of observation")
    'DoyleDowson)
→ (0.985108793 0.01489120699)

The queries correctly identify the first sentence as a Dowson sentence, and the
second one as a Doyle sentence.


Chain Bayesian method

The second example is frequently found in introductory literature on Bayesian
statistics. It shows the Chain Bayesian method of using bayes-query on the data
of a previously processed data set:

(set 'Data:test-positive '(8 18))
(set 'Data:test-negative '(2 72))
(set 'Data:total '(10 90))

A disease occurs in 10 percent of the population. A blood test developed to
detect this disease produces a false positive rate of 20 percent in the healthy
population and a false negative rate of 20 percent in the sick. What is the
probability of a person carrying the disease after testing positive?

(bayes-query '(test-positive) Data true)
→ (0.3076923077 0.6923076923)

(bayes-query '(test-positive test-positive) Data true)
→ (0.64 0.36)

(bayes-query '(test-positive test-positive test-positive) Data true)
→ (0.8767123288 0.1232876712)

Note that the Bayesian formulas used assume statistical independence of events
for the bayes-query to work correctly.

The example shows that a person must test positive several times before they
can be confidently classified as sick.

Calculating the same example using the R.A. Fisher Chi² method will give
less-distinguished results.


Specifying probabilities instead of counts

Often, data is already available as probability values and would require
additional work to reverse them into frequencies. In the last example, the data
were originally defined as percentages. The additional optional bool-probs flag
allows probabilities to be entered directly and should be used together with
the Chain Bayesian mode for maximum performance:

(set 'Data:test-positive '(0.8 0.2))
(set 'Data:test-negative '(0.2 0.8))
(set 'Data:total '(0.1 0.9))

(bayes-query '(test-positive) Data true true)
→ (0.3076923077 0.6923076923)

(bayes-query '(test-positive test-positive) Data true true)
→ (0.64 0.36)

(bayes-query '(test-positive test-positive test-positive) Data true true)
→ (0.8767123288 0.1232876712)

As expected, the results are the same for probabilities as they are for
frequencies.[/text])

(set '_bayes-train [text]{bayes-train

syntax: (bayes-train list-M1 [list-M2 ... ] sym-context-D)

Takes one or more lists of tokens (M1, M2—) from a joint set of tokens. In
newLISP, tokens can be symbols or strings (other data types are ignored).
Tokens are placed in a common dictionary in sym-context-D, and the frequency is
counted for each token in each category Mi. If the context does not yet exist,
it must be quoted.

The M categories represent data models for which sequences of tokens can be
classified (see bayes-query). Each token in D is a content-addressable symbol
containing a list of the frequencies for this token within each category.
String tokens are prepended with an _ (underscore) before being converted into
symbols. A symbol named total is created containing the total of each category.
The total symbol cannot be part of the symbols passed as an Mi category.

The function returns a list of token frequencies found in the different
categories or models.

(bayes-train '(A A B C C) '(A B B C C C) 'L)  → (5 6)

L:A      → (2 1)
L:B      → (1 2)
L:C      → (2 3)
L:total  → (5 6)

(bayes-train '("one" "two" "two" "three")
             '("three" "one" "three")
             '("one" "two" "three") 'S)
→ (4 3 3)

S:_one    → (1 1 1)
S:_two    → (2 0 1)
S:_three  → (1 2 1)
S:total   → (4 3 3)

The first example shows training with two lists of symbols. The second example
illustrates how an _ is prepended when training with strings.

bayes-train creates symbols from strings prepending an underscore character.
This is the same way hashes are created and contexts populates with symbols by
bayes-train can be used like hashes:

; use a bayes-trained context namespace like a hash dictionary

(S "two")   → (2 0 1)
(S "three") → (1 2 1)

(S) → (("one" (1 1 1)) ("three" (1 2 1)) ("two" (2 0 1)))

Note that these examples are just for demonstration purposes. In reality,
training sets may contain thousands or millions of words, especially when
training natural language models. But small data sets may be used when the
frequency of symbols just describe already-known proportions. In this case, it
may be better to describe the model data set explicitly, without the
bayes-train function:

(set 'Data:tested-positive '(8 18))
(set 'Data:tested-negative '(2 72))
(set 'Data:total '(10 90))

The last data are from a popular example used to describe the bayes-query
function in introductory papers and books about bayesian networks.

Training can be done in different stages by using bayes-train on an existing
trained context with the same number of categories. The new symbols will be
added, then counts and totals will be correctly updated.

Training in multiple batches may be necessary on big text corpora or documents
that must be tokenized first. These corpora can be tokenized in small portions,
then fed into bayes-train in multiple stages. Categories can also be singularly
trained by specifying an empty list for the absent corpus:

(bayes-train shakespeare1 '() 'data)
(bayes-train shakespeare2 '() 'data)
(bayes-train '() hemingway1 'data)
(bayes-train '() hemingway2 'data)
(bayes-train shakepeare-rest hemingway-rest 'data)

bayes-train will correctly update word counts and totals.

Using bayes-train inside a context other than MAIN requires the training
contexts to have been created previously within the MAIN context via the
context function.

bayes-train is not only useful with the bayes-query function, but also as a
function for counting in general. For instance, the resulting frequencies could
be analyzed using prob-chi2 against a null hypothesis of proportional
distribution of items across categories.[/text])

(set '_begin "{begin\n\nsyntax: (begin body)\n\nThe begin function is used to group a block of expressions. The expressions in\nbody are evaluated in sequence, and the value of the last expression in body is\nreturned.\n\n(begin\n  (print \"This is a block of 2 expressions\\n\")\n  (print \"================================\"))\n\nSome built-in functions like cond, define, doargs, dolist, dostring, dotimes,\nwhen and while already allow multiple expressions in their bodies, but begin is\noften used in an if expression.\n\nThe silent function works like begin, but suppresses console output on return.")

(set '_beta "{beta\n\nsyntax: (beta cum-a num-b)\n\nThe Beta function, beta, is derived from the log Gamma gammaln function as\nfollows:\n\nbeta = exp(gammaln(a) + gammaln(b) - gammaln(a + b))\n\n(beta 1 2)  \226\134\146 0.5")

(set '_betai "{betai\n\nsyntax: (betai num-x num-a num-b)\n\nThe Incomplete Beta function, betai, equals the cumulative probability of the\nBeta distribution, betai, at x in num-x. The cumulative binomial distribution\nis defined as the probability of an event, pev, with probability p to occur k\nor more times in N trials:\n\npev = Betai(p, k, N - k + 1)\n\n(betai 0.5 3 8)  \226\134\146 0.9453125\n\nThe example calculates the probability for an event with a probability of 0.5\nto occur 3 or more times in 10 trials (8 = 10 - 3 + 1). The incomplete Beta\ndistribution can be used to derive a variety of other functions in mathematics\nand statistics. See also the binomial function.")

(set '_bind "{bind !\n\nsyntax: (bind list-variable-associations [bool-eval])\n\nlist-variable-associations contains an association list of symbols and their\nvalues. bind sets all symbols to their associated values.\n\nThe associated values are evaluated if the bool-eval flag is true:\n\n(set 'lst '((a (+ 3 4)) (b \"hello\")))\n\n(bind lst)         \226\134\146 \"hello\"\n\na    \226\134\146 (+ 3 4)\nb    \226\134\146 \"hello\"\n\n(bind lst true)    \226\134\146 \"hello\"\n\na    \226\134\146 7\n\nThe return value of bind is the value of the last association.\n\nbind is often used to bind association lists returned by unify.\n\n(bind (unify '(p X Y a) '(p Y X X)))    \226\134\146 a\n\nX    \226\134\146 a\nY    \226\134\146 a\n\nThis can be used for de-structuring:\n\n(set 'structure '((one \"two\") 3 (four (x y z))))\n(set 'pattern '((A B) C (D E)))\n(bind (unify pattern structure))\n\nA \226\134\146 one\nB \226\134\146 \"two\"\nC \226\134\146 3\nD \226\134\146 four\nE \226\134\146 (x y z)\n\nunify returns an association list and bind binds the associations.")

(set '_binomial "{binomial\n\nsyntax: (binomial int-n int-k float-p)\n\nThe binomial distribution function is defined as the probability for an event\nto occur int-k times in int-n trials if that event has a probability of float-p\nand all trials are independent of one another:\n\nbinomial = pow(p, k) * pow(1.0 - p, n - k) * n! / (k! * (n - k)!)\n\nwhere x! is the factorial of x and pow(x, y) is x raised to the power of y.\n\n\n(binomial 10 3 0.5)  \226\134\146 0.1171875\n\nThe example calculates the probability for an event with a probability of 0.5\nto occur 3 times in 10 trials. For a cumulated distribution, see the betai\nfunction.")

(set '_bits "{bits\n\nsyntax: (bits int [bool])\n\nTransforms a number in int to a string of 1's and 0's or a list, if bool\nevaluates to anything not nil.\n\nIn string representation bits are in high to low order. In list presentation\n1's and 0's are represented as true and nil and in order from the lowest to the\nhighest bit. This allows direct indexing and program control switching on the\nresult.\n\n(bits 1234)      \226\134\146 \"10011010010\"\n\n(int (bits 1234) 0 2) \226\134\146 1234\n\n(bits 1234 true)     \226\134\146 (nil true nil nil true nil true true nil nil true)\n\n((bits 1234 true) 0) \226\134\146 nil ; indexing of the result\n\nint with a base of 2 is the inverse function to bits.")

(set '_callback [text]{callback

syntax: (callback int-index sym-function)
syntax: (callback sym-function str-return-type [str_param_type ...])
stntax: (callback sym-function)

In the first simple callback syntax up to sixteen (0 to 15) callback functions
for up to eight parameters can be registered with imported libraries. The
callback function returns a procedure address that invokes a user-defined
function in sym-function. The following example shows the usage of callback
functions when importing the OpenGL graphics library:

If more than sixteen callback functions are required, slots must be reassigned
to a different callback function.

...
(define (draw)
    (glClear GL_COLOR_BUFFER_BIT )
    (glRotated rotx 0.0 1.0 0.0)
    (glRotated roty 1.0 0.0 0.0)
    (glutWireTeapot 0.5)
    (glutSwapBuffers))

(define (keyboard key x y)
    (if (= (& key 0xFF) 27) (exit)) ; exit program with ESC
    (println "key:" (& key 0xFF) " x:" x  " y:" y))

(define (mouse button state x y)
    (if (= state 0)
        (glutIdleFunc 0) ; stop rotation on button press
        (glutIdleFunc (callback 4 'rotation)))
    (println "button: " button " state:" state " x:" x " y:" y))

(glutDisplayFunc (callback 0 'draw))
(glutKeyboardFunc (callback 1 'keyboard))
(glutMouseFunc (callback 2 'mouse))
...

The address returned by callback is registered with the Glut library. The above
code is a snippet from the file opengl-demo.lsp, in the examples/ directory of
the source distribution of newLISP.

In the second extended callback syntax type specifiers are used to describe the
functions return and parameter value types when the function is called. An
unlimited number of callback functions can be registered with the second
syntax, and return values are passed back to the calling function. The symbol
in sym-function contains a newLISP defined function used as a callback function
callable from a C program.

In the third syntax callback returns a previously returned C-callable address
for that symbol.

While the first simple callback syntax only handles integers and pointer
values, callback in the expanded syntax can also handle simple and double
precision floationg point numbers passed in an out of the callback function.

Both the simple and extended syntax can be mixed inside the same program.

The following example shows the import of the qsort C library function, which
takes as one of it's arguments the address of a comparison function. The
comparison function in this case is written in newLISP and called into by the
imported qsort function:

; C void qsort(...) takes an integer array with number and width
; of array elements and a pointer to the comparison function
(import "libc.dylib" "qsort" "void" "void*" "int" "int" "void*")

(set 'rlist '(2 3 1 2 4 4 3 3 0 3))
; pack the list into an C readable 32-bit integer array
(set 'carray (pack (dup "ld " 10) rlist))

; the comparison callback function receives pointers to integers
(define (cmp a b)
    (- (get-int a) (get-int b)))

; generate a C callable address for cmp
(set 'func (callback 'cmp "int" "void*" "void*"))

; sort the carray
(qsort carray 10 4 func)

; unpack the sorted array into a LISP list
(unpack (dup "ld" 10) carray)  →  (0 1 2 2 3 3 3 3 4 4)

As type specifiers the same string tags can be used as in the import function.
All pointer types are passed as numbers in and out of the callback function.
The functions get-char, get-int, get-long and get-string can be used to extract
numbers of different precision from parameters. Use pack and unpack to extract
data from binary buffers and structures.

Note that newLISP has already a fast built-in sort function.[/text])

(set '_case "{case\n\nsyntax: (case exp-switch (exp-1 body-1) [(exp-2 body-2) ... ])\n\nThe result of evaluating exp-switch is compared to each of the unevaluated\nexpressions exp-1, exp-2, \226\128\148. If a match is found, the corresponding expressions\nin body are evaluated. The result of the last body expression is returned as\nthe result for the entire case expression.\n\n(define (translate n)\n  (case n\n    (1 \"one\")\n    (2 \"two\")\n    (3 \"three\")\n    (4 \"four\")\n    (true \"Can't translate this\")))\n\n(translate 3)   \226\134\146 \"three\"\n(translate 10)  \226\134\146 \"Can't translate this\"\n\nThe example shows how, if no match is found, the last expression in the body of\na case function can be evaluated.")

(set '_catch "{catch\n\nsyntax: (catch exp)\nsyntax: (catch exp symbol)\n\nIn the first syntax, catch will return the result of the evaluation of exp or\nthe evaluated argument of a throw executed during the evaluation of exp:\n\n(catch (dotimes (x 1000)\n  (if (= x 500) (throw x))))  \226\134\146 500\n\nThis form is useful for breaking out of iteration loops and for forcing an\nearly return from a function or expression block:\n\n(define (foo x)\n   \226\128\166\n  (if condition (throw 123))\n    \226\128\166\n  456)\n\n;; if condition is true\n\n(catch (foo p))  \226\134\146 123\n\n;; if condition is not true\n\n(catch (foo p))  \226\134\146 456\n\nIn the second syntax, catch evaluates the expression exp, stores the result in\nsymbol, and returns true. If an error occurs during evaluation, catch returns\nnil and stores the error message in symbol. This form can be useful when errors\nare expected as a normal potential outcome of a function and are dealt with\nduring program execution.\n\n(catch (func 3 4) 'result)  \226\134\146 nil\nresult\n\226\134\146 \"ERR: invalid function in function catch : (func 3 4)\"\n\n(constant 'func +)          \226\134\146 + <4068A6>\n(catch (func 3 4) 'result)  \226\134\146 true\nresult                      \226\134\146 7\n\nWhen a throw is executed during the evaluation of exp, catch will return true,\nand the throw argument will be stored in symbol:\n\n(catch (dotimes (x 100)\n  (if (= x 50) (throw \"fin\")) 'result)  \226\134\146 true\n\nresult  \226\134\146 \"fin\"\n\nAs well as being used for early returns from functions and for breaking out of\niteration loops (as in the first syntax), the second syntax of catch can also\nbe used to catch errors. The throw-error function may be used to throw\nuser-defined errors.")

(set '_ceil "{ceil\n\nsyntax: (ceil number)\n\nReturns the next highest integer above number as a floating point.\n\n(ceil -1.5)  \226\134\146 -1\n(ceil 3.4)   \226\134\146 4\n\nSee also the floor function.")

(set '_change-dir "{change-dir\n\nsyntax: (change-dir str-path)\n\nChanges the current directory to be the one given in str-path. If successful,\ntrue is returned; otherwise nil is returned.\n\n(change-dir \"/etc\")\n\nMakes /etc the current directory.")

(set '_char "{char utf8\n\nsyntax: (char str [int-index [true]])\nsyntax: (char int)\n\nGiven a string argument, extracts the character at int-index from str,\nreturning either the ASCII value of that character or the Unicode value on\nUTF-8 enabled versions of newLISP.\n\nIf int-index is omitted, 0 (zero) is assumed. If int-idx is followed by a\nboolean true value, than the index treats str as an 8-bit byte array instead of\nan array of multi-byte UTF-8 characters.\n\nThe empty string returns nil. Both (char 0) and (char nil) will return \"\\000\".\n\nSee Indexing elements of strings and lists.\n\nGiven an integer argument, char returns a string containing the ASCII character\nwith value int.\n\nOn UTF-8\226\128\147enabled versions of newLISP, the value in int is taken as Unicode and\na UTF-8 character is returned.\n\n(char \"ABC\")          \226\134\146 65  ; ASCII code for \"A\"\n(char \"ABC\" 1)        \226\134\146 66  ; ASCII code for \"B\"\n(char \"ABC\" -1)       \226\134\146 67  ; ASCII code for \"C\"\n(char \"B\")            \226\134\146 66  ; ASCII code for \"B\"\n(char \"\206\169\")            \226\134\146 937 ; UTF-8 code for \"\206\169\"\n(char \"\206\169\" 1 true)     \226\134\146 169 ; byte value at offset 1\n\n(char 65)  \226\134\146 \"A\"\n(char 66)  \226\134\146 \"B\"\n\n(char (char 65))  \226\134\146 65      ; two inverse applications\n\n(map char (sequence 1 255))  ; returns current character set\n\n; The Zen of UTF-8\n(char (& (char \"\231\148\159\") (char \"\230\173\187\"))) \226\134\146 \230\132\155 ; by @kosh_bot")

(set '_chop "{chop utf8\n\nsyntax: (chop str [int-chars])\nsyntax: (chop list [int-elements])\n\nIf the first argument evaluates to a string, chop returns a copy of str with\nthe last int-char characters omitted. If the int-char argument is absent, one\ncharacter is omitted. chop does not alter str.\n\nIf the first argument evaluates to a list, a copy of list is returned with\nint-elements omitted (same as for strings).\n\n(set 'str \"newLISP\")  \226\134\146 \"newLISP\"\n\n(chop str)    \226\134\146 \"newLIS\"\n(chop str 2)  \226\134\146 \"newLI\"\n\nstr  \226\134\146 \"newLISP\"\n\n(set 'lst '(a b (c d) e))\n\n(chop lst)    \226\134\146 (a b (c d))\n(chop lst 2)  \226\134\146 (a b)\n\nlst  \226\134\146 (a b (c d) e)")

(set '_clean "{clean\n\nsyntax: (clean exp-predicate list)\n\nThe predicate exp-predicate is applied to each element of list. In the returned\nlist, all elements for which exp-predicate is true are eliminated.\n\nclean works like filter with a negated predicate.\n\n(clean symbol? '(1 2 d 4 f g 5 h))   \226\134\146 (1 2 4 5)\n\n(filter symbol? '(1 2 d 4 f g 5 h))  \226\134\146 (d f g h)\n\n(define (big? x) (> x 5))        \226\134\146 (lambda (x) (> x 5))\n\n(clean big? '(1 10 3 6 4 5 11))  \226\134\146 (1 3 4 5)\n\n(clean <= '(3 4 -6 0 2 -3 0))  \226\134\146 (3 4 2)\n\n(clean (curry match '(a *)) '((a 10) (b 5) (a 3) (c 8) (a 9)))\n\226\134\146  ((b 5) (c 8))\n\nThe predicate may be a built-in predicate or a user-defined function or lambda\nexpression.\n\nFor cleaning numbers from one list using numbers from another, use difference\nor intersect (with the list mode option).\n\nSee also the related function index, which returns the indices of the remaining\nelements, and filter, which returns all elements for which a predicate returns\ntrue.")

(set '_close "{close\n\nsyntax: (close int-file)\n\nCloses the file specified by the file handle in int-file. The handle would have\nbeen obtained from a previous open operation. If successful, close returns\ntrue; otherwise nil is returned.\n\n(close (device))  \226\134\146 true\n(close 7)         \226\134\146 true\n(close aHandle)   \226\134\146 true\n\nNote that using close on device automatically resets it to 0 (zero, the screen\ndevice).")

(set '_command-event [text]{command-event

syntax: (command-event sym-event-handler | func-event-handler)

Specifies a user defined function for pre-processing the newLISP command-line
before it gets evaluated. This can be used to write customized interactive
newLISP shells and to transform HTTP requests when running in server mode.

command-event takes either a symbol of a user-defined function or a lambda
function. The event-handler function must return a string or the command-line
will be passed untranslated to newLISP.

To only force a prompt, the function should return the empty string "".

The following example makes the newLISP shell work like a normal Unix shell
when the command starts with a letter. But starting the line with an open
parenthesis or a space initiates a newLISP evaluation.

(command-event (fn (s)
        (if (starts-with s "[a-zA-Z]" 0) (append "!" s) s)))

See also the related prompt-event which can be used for further customizing
interactive mode by modifying the newLISP prompt.

The following program can be used either stand-alone or included in newLISP's
init.lsp startup file:

#!/usr/bin/newlisp

; set the prompt to the current directory name
(prompt-event (fn (ctx) (append (real-path) "> ")))

; pre-process the command-line
(command-event (fn (s)
    (if
        (starts-with s "cd")
        (string " " (true? (change-dir (last (parse s " ")))))

        (starts-with s "[a-zA-Z]" 0)
        (append "!" s)

        true s)))

In the definition of the command-line translation function the Unix command cd
gets a special treatment, to make sure that the directory is changed for
newLISP process too. This way when shelling out with ! and coming back, newLISP
will maintain the changed directory.

Command lines for newLISP must start either with a space or an opening
parenthesis. Unix commands must start at the beginning of the line.

When newLISP is running in server mode either using the -c or -http option, it
receives HTTP requests similar to the following:

GET /index.html

Or if a query is involved:

GET /index.cgi?userid=joe&password=secret

A function specified by command-event could filter and transform these request
lines, e.g.: discovering all queries trying to perform CGI using a file ending
in .exe.  Such a request would be translated into a request for an error page:

;; httpd-conf.lsp
;;
;; filter and translate HTTP requests for newLISP
;; -c or -http server modes
;; reject query commands using CGI with .exe files

(command-event (fn (s)
    (let (request s)
        (when (find "?" s) ; is this a query
            (set 'request (first (parse s "?")))
            ; discover illegal extension in queries
            (when (ends-with request ".exe")
                (set 'request "GET /errorpage.html")) )
        request)
))

When starting the server mode with newlisp httpd-conf.lsp -c -d80 -w ./httpdoc
newLISP will load the definition for command-event for filtering incoming
requests, and the query:

GET /cmd.exe?dir

Would be translated into:

GET /errorpage.html

The example shows a technique frequently used in the past by spammers on Win32
based, bad configured web servers to gain control over servers.

httpd-conf.lsp files can easily be debugged loading the file into an
interactive newLISP session and entering the HTTP requests manually. newLISP
will translate the command line and dispatch it to the built-in web server. The
server output will appear in the shell window.

Note, that the command line length as well as the line length in HTTP headers
is limited to 512 characters for newLISP.[/text])

(set '_cond "{cond\n\nsyntax: (cond (exp-condition-1 body-1) [(exp-condition-2 body-2) ... ])\n\nLike if, cond conditionally evaluates the expressions within its body. The\nexp-conditions are evaluated in turn, until some exp-condition-i is found that\nevaluates to anything other than nil or an empty list (). The result of\nevaluating body-i is then returned as the result of the entire cond-expression.\nIf all conditions evaluate to nil or an empty list, cond returns the value of\nthe last cond-expression.\n\n(define (classify x)\n  (cond\n    ((< x 0) \"negative\")\n    ((< x 10) \"small\")\n    ((< x 20) \"medium\")\n    ((>= x 30) \"big\")))\n\n(classify 15)   \226\134\146 \"medium\"\n(classify 22)   \226\134\146 \"nil\"\n(classify 100)  \226\134\146 \"big\"\n(classify -10)  \226\134\146 \"negative\"\n\nWhen a body-n is missing, the value of the last cond-expression evaluated is\nreturned. If no condition evaluates to true, the value of the last conditional\nexpression is returned (i.e., nil or an empty list).\n\n(cond ((+ 3 4)))  \226\134\146 7\n\nWhen used with multiple arguments, the function if behaves like cond, except it\ndoes not need extra parentheses to enclose the condition-body pair of\nexpressions.")

(set '_cons "{cons\n\nsyntax: (cons exp-1 exp-2)\n\nIf exp-2 evaluates to a list, then a list is returned with the result of\nevaluating exp-1 inserted as the first element. If exp-2 evaluates to anything\nother than a list, the results of evaluating exp-1 and exp-2 are returned in a\nlist. Note that there is no dotted pair in newLISP: consing two atoms\nconstructs a list, not a dotted pair.\n\n(cons 'a 'b)            \226\134\146 (a b)\n(cons 'a '(b c))        \226\134\146 (a b c)\n(cons (+ 3 4) (* 5 5))  \226\134\146 (7 25)\n(cons '(1 2) '(3 4))    \226\134\146 ((1 2) 3 4)\n(cons nil 1)            \226\134\146 (nil 1)\n(cons 1 nil)            \226\134\146 (1 nil)\n(cons 1)                \226\134\146 (1)\n(cons)                  \226\134\146 ()\n\nUnlike other Lisps that return (s) as the result of the expression (cons 's\nnil), newLISP's cons returns (s nil). In newLISP, nil is a Boolean value and is\nnot equivalent to an empty list, and a newLISP cell holds only one value.\n\ncons behaves like the inverse operation of first and rest (or first and last if\nthe list is a pair):\n\n(cons (first '(a b c)) (rest '(a b c)))  \226\134\146 (a b c)\n\n(cons (first '(x y)) (last '(x y)))      \226\134\146 (x y)")

(set '_constant "{constant !\n\nsyntax: (constant sym-1 exp-1 [sym-2 exp-2] ...)\n\nIdentical to set in functionality, constant further protects the symbols from\nsubsequent modification. A symbol set with constant can only be modified using\nthe constant function again. When an attempt is made to modify the contents of\na symbol protected with constant, newLISP generates an error message. Only\nsymbols from the current context can be used with constant. This prevents the\noverwriting of symbols that have been protected in their home context. The last\nexp-n initializer is always optional.\n\nSymbols initialized with set, define, or define-macro can still be protected by\nusing the constant function:\n\n(constant 'aVar 123)  \226\134\146 123\n(set 'aVar 999)\nERR: symbol is protected in function set: aVar\n\n(define (double x) (+ x x))\n\n(constant 'double)\n\n;; equivalent to\n\n(constant 'double (fn (x) (+ x x)))\n\nThe first example defines a constant, aVar, which can only be changed by using\nanother constant statement. The second example protects double from being\nchanged (except by constant). Because a function definition in newLISP is\nequivalent to an assignment of a lambda function, both steps can be collapsed\ninto one, as shown in the last statement line. This could be an important\ntechnique for avoiding protection errors when a file is loaded multiple times.\n\nThe last value to be assigned can be omitted. constant returns the contents of\nthe last symbol set and protected.\n\nBuilt-in functions can be assigned to symbols or to the names of other built-in\nfunctions, effectively redefining them as different functions. There is no\nperformance loss when renaming functions.\n\n(constant 'squareroot sqrt)  \226\134\146 sqrt <406C2E>\n(constant '+ add)            \226\134\146 add <4068A6>\n\nsquareroot will behave like sqrt. The + (plus sign) is redefined to use the\nmixed type floating point mode of add. The hexadecimal number displayed in the\nresult is the binary address of the built-in function and varies on different\nplatforms and OSes.")

(set '_context [text]{context

syntax: (context [sym-context])
syntax: (context sym-context str | sym [exp-value])

In the first syntax, context is used to switch to a different context
namespace. Subsequent loads of newLISP source or functions like eval-string and
sym will put newly created symbols and function definitions in the new context.

If the context still needs to be created, the symbol for the new context should
be specified. When no argument is passed to context, then the symbol for the
current context is returned.

Because contexts evaluate to themselves, a quote is not necessary to switch to
a different context if that context already exists.

(context 'GRAPH)          ; create / switch context GRAPH

(define (foo-draw x y z)  ; function resides in GRAPH
  (…))

(set 'var 12345)
(symbols)  → (foo-draw var)  ; GRAPH has now two symbols

(context MAIN)               ; switch back to MAIN (quote not required)

(print GRAPH:var) → 12345    ; contents of symbol in GRAPH

(GRAPH:foo-draw 10 20 30)    ; execute function in GRAPH
(set 'GRAPH:var 6789)        ; assign to a symbol in GRAPH

If a context symbol is referred to before the context exists, the context will
be created implicitly.

(set 'person:age 0)       ; no need to create context first
(set 'person:address "")  ; useful for quickly defining data structures

Contexts can be copied:

(new person 'JohnDoe)  →  JohnDoe

(set 'JohnDoe:age 99)

Contexts can be referred to by a variable:

(set 'human JohnDoe)

human:age  → 99

(set 'human:address "1 Main Street")

JohnDoe:address  → "1 Main Street"

An evaluated context (no quote) can be given as an argument:

> (context 'FOO)
FOO
FOO> (context MAIN)
MAIN
> (set 'old FOO)
FOO
> (context 'BAR)
BAR
BAR> (context MAIN:old)
FOO
FOO>

If an identifier with the same symbol already exists, it is redefined to be a
context.

Symbols within the current context are referred to simply by their names, as
are built-in functions and special symbols like nil and true. Symbols outside
the current context are referenced by prefixing the symbol name with the
context name and a : (colon). To quote a symbol in a different context, prefix
the context name with a ' (single quote).

Within a given context, symbols may be created with the same name as built-in
functions or context symbols in MAIN. This overwrites the symbols in MAIN when
they are prefixed with a context:

(context 'CTX)
(define (CTX:new var)
    (…))

(context 'MAIN)

CTX:new will overwrite new in MAIN.

In the second syntax, context can be used to create symbols in a namespace.
Note that this should not be used for creating hashes or dictionaries. For a
shorter, more convenient method to use namespaces as hash-like dictionaries,
see the chapter Hash functions and dictionaries.

;; create a symbol and store data in it
(context 'Ctx "abc" 123)   → 123
(context 'Ctx 'xyz 999)    → 999

;; retrieve contents from  symbol
(context 'Ctx "abc")       → 123
(context 'Ctx 'xyz)        → 999
Ctx:abc                    → 123
Ctx:xyz                    → 999

The first three statements create a symbol and store a value of any data type
inside. The first statement also creates the context named Ctx. When a symbol
is specified for the name, the name is taken from the symbol and creates a
symbol with the same name in the context Ctx.

Symbols can contain spaces or any other special characters not typically
allowed in newLISP symbols being used as variable names. This second syntax of
context only creates the new symbol and returns the value contained in it. It
does not switch to the new namespace.[/text])

(set '_context? "{context?\n\nsyntax: (context? exp)\nsyntax: (context? exp str-sym)\n\nIn the first syntax, context? is a predicate that returns true only if exp\nevaluates to a context; otherwise, it returns nil.\n\n(context? MAIN)  \226\134\146 true\n(set 'x 123)\n(context? x)     \226\134\146 nil\n\n(set 'FOO:q \"hola\")  \226\134\146 \"hola\"\n(set 'ctx FOO)\n(context? ctx)       \226\134\146 true  ; ctx contains context foo\n\nThe second syntax checks for the existence of a symbol in a context. The symbol\nis specified by its name string in str-sym.\n\n(context? FOO \"q\")  \226\134\146 true\n(context? FOO \"p\")  \226\134\146 nil\n\nUse context to change and create namespaces and to create hash symbols in\ncontexts.")

(set '_copy "{copy\n\nsyntax: (copy exp)\n\nMake a copy from evaluating expression in exp. Some built-in functions are\ndestructive, changing the original contents of a list, array or string they are\nworking on. With copy their behavior can be made non-destructive.\n\n(set 'aList '(a b c d e f))\n\n(replace 'c (copy aList)) \226\134\146 (a b d e f)\n\naList \226\134\146 (a b c d e f)\n\n(set 'str \"newLISP\") \226\134\146 \"newLISP\"\n\n(rotate (copy str)) \226\134\146 \"PnewLIS\"\n\nstr \226\134\146 \"newLISP\"\n\nUsing copy the functions replace and rotate are prevented from changing the\ndata. A modified version of the data is returned.")

(set '_copy-file "{copy-file\n\nsyntax: (copy-file str-from-name str-to-name)\n\nCopies a file from a path-filename given in str-from-name to a path-filename\ngiven in str-to-name. Returns true if the copy was successful or nil, if the\ncopy was unsuccessful.\n\n(copy-file \"/home/me/newlisp/data.lsp\" \"/tmp/data.lsp\")")

(set '_corr "{corr\n\nsyntax: (corr list-vector-X list-vector-Y)\n\nCalculates the Pearson product-moment correlation coefficient as a measure of\nthe linear relationship between the two variables in list-vector-X and\nlist-vector-Y. Both lists must be of same length.\n\ncorr returns a list containing the following values:\n\nname description\nr    Correlation coefficient\nb0   Regression coefficient offset\nb1   Regression coefficient slope\nt    t - statistic for significance testing\ndf   Degrees of freedom for t\np    Two tailed probability of t under the null hyothesis\n\n\n(set 'study-time '(90 100 130 150 180 200 220 300 350 400))\n(set 'test-errors '(25 28 20 20 15 12 13 10 8 6))\n\n(corr study-time test-errors) \226\134\146 (-0.926 29.241 -0.064 -6.944 8 0.0001190)\n\nThe negative correlation of -0.926 between study time and test errors is highly\nsignficant with a two-tailed p of about 0.0001 under the null hypothesis.\n\nThe regression coefficients b0 = 29.241 and b1 = -0.064 can be used to estimate\nvalues of the Y variable (test errors) from values in X (study time) using the\nequation Y = b0 + b1 * X.")

(set '_cos "{cos\n\nsyntax: (cos num-radians)\n\nCalculates the cosine of num-radians and returns the result.\n\n(cos 1)                     \226\134\146 0.5403023059\n(set 'pi (mul 2 (acos 0)))  \226\134\146 3.141592654\n(cos pi)                    \226\134\146 -1")

(set '_cosh "{cosh\n\nsyntax: (cosh num-radians)\n\nCalculates the hyperbolic cosine of num-radians. The hyperbolic cosine is\ndefined mathematically as: (exp (x) + exp (-x)) / 2. An overflow to inf may\noccur if num-radians is too large.\n\n(cosh 1)     \226\134\146 1.543080635\n(cosh 10)    \226\134\146 11013.23292\n(cosh 1000)  \226\134\146 inf\n(= (cosh 1) (div (add (exp 1) (exp -1)) 2))  \226\134\146 true")

(set '_count "{count\n\nsyntax: (count list-1 list-2)\n\nCounts elements of list-1 in list-2 and returns a list of those counts.\n\n(count '(1 2 3) '(3 2 1 4 2 3 1 1 2 2))  \226\134\146 (3 4 2)\n(count '(z a) '(z d z b a z y a))        \226\134\146 (3 2)\n\n(set 'lst (explode (read-file \"myFile.txt\")))\n(set 'letter-counts (count (unique lst) lst))\n\nThe second example counts all occurrences of different letters in myFile.txt.\n\nThe first list in count, which specifies the items to be counted in the second\nlist, should be unique. For items that are not unique, only the first instance\nwill carry a count; all other instances will display 0 (zero).")

(set '_cpymem "{cpymem\n\nsyntax: (cpymem int-from-address int-to-address int-bytes)\n\nCopies int-bytes of memory from int-from-address to int-to-address. This\nfunction can be used for direct memory writing/reading or for hacking newLISP\ninternals (e.g., type bits in newLISP cells, or building functions with binary\nexecutable code on the fly).\n\nNote that this function should only be used when familiar with newLISP\ninternals. cpymem can crash the system or make it unstable if used incorrectly.\n\n(cpymem (pack \"c c\" 0 32) (last (dump 'sym)) 2)\n\n(set 's \"0123456789\")\n\n(cpymem \"xxx\" (+ (address s) 5) 3)\n\ns  \226\134\146 \"01234xxx89\")\n\nThe first example would remove the protection bit in symbol sym. The second\nexample copies a string directly into a string variable.\n\nThe following example creates a new function from scratch, runs a piece of\nbinary code, and adds up two numbers. This assembly language snippet shows the\nx86 (Intel CPU) code to add up two numbers and return the result:\n\n 55       push ebp\n 8B EC    mov  ebp, esp\n 8B 45 08 mov  eax, [ebp+08]\n 03 45 0C add  eax, [ebp+0c]\n 5D       pop  ebp\n C3       ret\n\n ; for Win32/stdcall change last line\n C2 08 00 ret\n\nThe binary representation is attached to a new function created in newLISP:\n\n(set 'foo-code (append\n     (pack \"bbbbbbbbbb\" 0x55 0x8B 0xEC 0x8B 0x45 0x08 0x03 0x45 0x0C 0x5D)\n     (if (= ostype \"Win32\") (pack \"bbb\" 0xC2 0x08 0x00) (pack \"b\" 0xC3))))\n(set 'foo print)\n(cpymem (pack \"ld\" (if (= ostype \"Win32\") 2312 1288)) (first (dump foo)) 4)\n(cpymem (pack \"ld\" (address foo-code)) (+ (first (dump foo)) 12) 4)\n(set 'foo-name \"foo\")\n(cpymem (pack \"ld\" foo-name) (+ (first (dump foo)) 8) 4)\n\n(foo 3 4)  \226\134\146  7\n\nThe last example will not work on all hardware platforms and OSs.\n\nUse the dump function to retrieve binary addresses and the contents from\nnewLISP cells.")

(set '_crc32 "{crc32\n\nsyntax: (crc32 str-data)\n\nCalculates a running 32-bit CRC (Circular Redundancy Check) sum from the buffer\nin str-data, starting with a CRC of 0xffffffff for the first byte. crc32 uses\nan algorithm published by www.w3.org.\n\n(crc32 \"abcdefghijklmnopqrstuvwxyz\")  \226\134\146 1277644989\n\ncrc32 is often used to verify data integrity in unsafe data transmissions.")

(set '_crit-chi2 "{crit-chi2\n\nsyntax: (crit-chi2 num-probability int-df)\n\nCalculates the critical minimum Chi\194\178  for a given confidence probability\nnum-probability under the null hypothesis and the degrees of freedom in int-df \nfor testing the significance of a statistical null hypothesis.\n\nNote that versions prior to 10.2.0 took (1.0 - p) for the probability instead\nof p.\n\n(crit-chi2 0.01 4)  \226\134\146 13.27670443\n\nSee also the inverse function prob-chi2.")

(set '_crit-f "{crit-f\n\nsyntax: (crit-f num-probability int-df1 int-df2)\n\nCalculates the critical minimum F  for a given confidence probability\nnum-probability under the null hypothesis and the degrees of freedmom given in\nint-df1 and int-df2 for testing the significance of a statistical null\nhypothesis using the F-test.\n\n(crit-f 0.05 10 12)  \226\134\146 2.753386727\n\nSee also the inverse function prob-f.")

(set '_crit-t "{crit-t\n\nsyntax: (crit-t num-probability int-df)\n\nCalculates the critical minimum Student's t for a given confidence probability\nnum-probability under the null hypothesis and the degrees of freedom in int-df \nfor testing the significance of a statistical null hypothesis.\n\n(crit-t 0.05 14)  \226\134\146 1.761310142\n\nSee also the inverse function prob-t.")

(set '_crit-z "{crit-z\n\nsyntax: (crit-z num-probability)\n\nCalculates the critical normal distributed Z value of a given cumulated\nprobability num-probability for testing of statistical significance and\nconfidence intervals.\n\n(crit-z 0.999)  \226\134\146 3.090232372\n\nSee also the inverse function prob-z.")

(set '_current-line "{current-line\n\nsyntax: (current-line)\n\nRetrieves the contents of the last read-line operation. current-line's contents\nare also implicitly used when write-line is called without a string parameter.\n\nThe following source shows the typical code pattern for creating a Unix\ncommand-line filter:\n\n#!/usr/bin/newlisp\n\n(set 'inFile (open (main-args 2) \"read\"))\n(while (read-line inFile)\n  (if (starts-with (current-line) \";;\")\n    (write-line)))\n(exit)\n\nThe program is invoked:\n\n./filter myfile.lsp\n\nThis displays all comment lines starting with ;; from a file given as a\ncommand-line argument when invoking the script filter.")

(set '_curry "{curry\n\nsyntax: (curry func exp)\n\nTransforms func from a function f(x, y) that takes two arguments into a\nfunction fx(y) that takes a single argument. curry works like a macro in that\nit does not evaluate its arguments. Instead, they are evaluated during the\napplication of func.\n\n(set 'f (curry + 10))  \226\134\146 (lambda ($x) (+ 10 $x))\n\n(f 7)  \226\134\146 17\n\n(filter (curry match '(a *)) '((a 10) (b 5) (a 3) (c 8) (a 9)))\n\226\134\146  ((a 10) (a 3) (a 9))\n\n(clean (curry match '(a *)) '((a 10) (b 5) (a 3) (c 8) (a 9)))\n\226\134\146  ((b 5) (c 8))\n\n(map (curry list 'x) (sequence 1 5))\n\226\134\146  ((x 1) (x 2) (x 3) (x 4) (x 5))\n\ncurry can be used on all functions taking two arguments.")

(set '_date [text]{date utf8

syntax: (date)
syntax: (date int-secs [int-offset])
syntax: (date int-secs int-offset str-format)

The first syntax returns the local time zone's current date and time as a
string representation. If int-secs is out of range, nil is returned.

In the second syntax, date translates the number of seconds in int-secs into
its date/time string representation for the local time zone. The number in
int-secs is usually retrieved from the system using date-value. Optionally, a
time-zone offset (in minutes) can be specified in int-offset, which is added or
subtracted before conversion of int-sec to a string. If int-secs is out of
range or an invalid str-format is specified, an empty string "" is returned.

(date)                   → "Fri Oct 29 09:56:58 2004"

(date (date-value))      → "Sat May 20 11:37:15 2006"
(date (date-value) 300)  → "Sat May 20 16:37:19 2006"  ; 5 hours offset
(date 0)                 → "Wed Dec 31 16:00:00 1969"
(date 0 (now 0 -2))      → "Thu Jan  1 00:00:00 1970"  ; Unix epoch

The way the date and time are presented in a string depends on the underlying
operating system.

The second example would show 1-1-1970 0:0 when in the Greenwich time zone, but
it displays a time lag of 8 hours when in Pacific Standard Time (PST). date
assumes the int-secs given are in Coordinated Universal Time (UTC; formerly
Greenwich Mean Time (GMT)) and converts it according to the local time-zone.

The third syntax makes the date string fully customizable by using a format
specified in str-format. This allows the day and month names to be translated
into results appropriate for the current locale:

(set-locale "german") → "de_DE"

; on Linux - no leading 0 on day with %-d
(date (date-value) 0 "%A %-d. %B %Y")  → "Montag  7. März 2005"

(set-locale "C")  ; default POSIX

(date (date-value) 0 "%A %B %d %Y")    → "Monday March 07 2005"

; suppressing leading 0 on Win32 using #
(date (date-value) 0 "%a %#d %b %Y")   → "Mon 7 Mar 2005"

(set-locale "german")

(date (date-value) 0 "%x") → "07.03.2005"   ; day month year

(set-locale "C")

(date (date-value) 0 "%x") → "03/07/05"     ; month day year

The following table summarizes all format specifiers available on both Win32
and Linux/Unix platforms. More format options are available on Linux/Unix. For
details, consult the manual page for the C function strftime() of the
individual platform's C library.

format description
%a     abbreviated weekday name according to the current locale
%A     full weekday name according to the current locale
%b     abbreviated month name according to the current locale
%B     full month name according to the current locale
%c     preferred date and time representation for the current locale
%d     day of the month as a decimal number (range 01–31)
%H     hour as a decimal number using a 24-hour clock (range 00–23)
%I     hour as a decimal number using a 12-hour clock (range 01–12)
%j     day of the year as a decimal number (range 001–366)
%m     month as a decimal number (range 01–12)
%M     minute as a decimal number
%p     either 'am' or 'pm' according to the given time value or the
       corresponding strings for the current locale
%S     second as a decimal number 0–61 (60 and 61 to account for occasional
       leap seconds)
%U     week number of the current year as a decimal number, starting with the
       first Sunday as the first day of the first week
%w     day of the week as a decimal, Sunday being 0
%W     week number of the current year as a decimal number, starting with the
       first Monday as the first day of the first week
%x     preferred date representation for the current locale without the time
%X     preferred time representation for the current locale without the date
%y     year as a decimal number without a century (range 00–99)
%Y     year as a decimal number including the century
%z     time zone or name or abbreviation (same as %Z on Win32, different on
       Unix)
%Z     time zone or name or abbreviation (same as %z on Win32, different on
       Unix)
%%     a literal '%' character


Leading zeroes in the display of decimal day numbers can be suppressed using
"%-d" on Linux and FreeBSD and using "%e" on OpenBSD, SunOS/Solaris and Mac OS
X. On Win32 use "%#d".

See also date-value, date-list, date-parse, time-of-day, time, and now.[/text])

(set '_date-list "{date-list\n\nsyntax: (date-list int-seconds [int-index])\n\nReturns a list of year, month, date, hours, minutes, seconds, day of year and\nday of week from a time value given in seconds after January 1st, 1970\n00:00:00. The date and time values aren given as UTC, which may differ from the\nlocal timezone.\n\nThe week-day value ranges from 1 to 7 for Monday thru Sunday.\n\n(date-list 1282479244)      \226\134\146 (2010 8 22 12 14 4 234 1)\n(date-list 1282479244 0)    \226\134\146 2010 ; year\n(date-list 1282479244 -2)   \226\134\146 234  ; day of year\n\n(apply date-value (date-list 1282479244)) \226\134\146 1282479244\n\n(date-list 0)   \226\134\146 (1970 1 1 0 0 0 1 4) ; Thursday 1st, Jan 1900\n\nA second optional int-index parameter can be used to return a specific member\nof the list.\n\ndate-list is the inverse operation of date-value.")

(set '_date-parse "{date-parse\n\nsyntax: (date-parse str-date str-format)\n\nParses a date from a text string in str-date using a format as defined in\nstr-format, which uses the same formatting rules found in date. The function\ndate-parse returns the number of UTC seconds passed since January 1st, 1970 UTC\nstarting with 0 and up to 2147472000 for a date of January 19th, 2038. For\ndates before January 1st 1970, negative values are returned down to a value of\n-2147472000 for December 14th, 1901.\n\nThis function is not available on Win32 platforms. The function was named\nparse-date in previous versions. The old form is deprecated.\n\n(date-parse \"2007.1.3\" \"%Y.%m.%d\")    \226\134\146 1167782400\n(date-parse \"January 10, 07\" \"%B %d, %y\")    \226\134\146 1168387200\n\n; output of date-parse as input value to date-list produces the same date\n\n(date-list (date-parse \"2010.10.18 7:00\" \"%Y.%m.%d %H:%M\"))\n\226\134\146 (2010 10 18 7 0 0 290 1)\n\nSee the date function for all possible format descriptors.")

(set '_date-value "{date-value\n\nsyntax: (date-value int-year int-month int-day [int-hour int-min int-sec])\nsyntax: (date-value)\n\nIn the first syntax, date-value returns the time in seconds since 1970-1-1\n00:00:00 for a given date and time. The parameters for the hour, minutes, and\nseconds are optional. The time is assumed to be Coordinated Universal Time\n(UTC), not adjusted for the current time zone.\n\nIn the second syntax, date-value returns the time value in seconds for the\ncurrent time.\n\n(date-value 2002 2 28)       \226\134\146 1014854400\n(date-value 1970 1 1 0 0 0)  \226\134\146 0\n\n(date (apply date-value (now)))  \226\134\146 \"Wed May 24 10:02:47 2006\"\n(date (date-value))              \226\134\146 \"Wed May 24 10:02:47 2006\"\n(date)                           \226\134\146 \"Wed May 24 10:02:47 2006\"\n\nThe function date-list can be used to transform a date-value back into a list:\n\n(date-list 1014854400)  \226\134\146 (2002 2 28 0 0 0)\n(apply date-value (date-list 1014854400)) \226\134\146 1014854400\n\nSee also date, date-list, date-parse, time-of-day, time, and now.")

(set '_debug "{debug\n\nsyntax: (debug func)\n\nCalls trace and begins evaluating the user-defined function in func. debug is a\nshortcut for executing (trace true), then entering the function to be debugged.\n\n;; instead of doing\n(trace true)\n(my-func a b c)\n(trace nil)\n\n;; use debug as a shortcut\n(debug (my-func a b c))\n\nWhen in debug or trace mode, error messages will be printed. The function\ncausing the exception will return either 0 or nil and processing will continue.\nThis way, variables and the current state of the program can still be inspected\nwhile debugging.\n\nSee also the trace function.")

(set '_dec "{dec !\n\nsyntax: (dec place [num])\n\nThe number in place is decremented by 1.0 or the optional number num and\nreturned. dec performs float arithmetic and converts integer numbers passed\ninto floating point type.\n\nplace is either a symbol or a place in a list structure holding a number, or a\nnumber returned by an expression.\n\n(set x 10)    \226\134\146 10\n(dec x)       \226\134\146 9\nx             \226\134\146 9\n(dec x 0.25)  \226\134\146 8.75\nx             \226\134\146 8.75\n\nIf the symbol for place contains nil, it is treated as if containing 0.0:\n\nz             \226\134\146 nil\n(dec z)       \226\134\146 -1\n\n(set z nil)\n(dec z 0.01)  \226\134\146 -0.01\n\nPlaces in a list structure or a number returned by another expression can be\nupdated too:\n\n(set 'l '(1 2 3 4))\n\n(dec (l 3) 0.1) \226\134\146 3.9\n\n(dec (first l)) \226\134\146 0\n\nl \226\134\146 (0 2 3 3.9)\n\n(dec (+ 3 4)) \226\134\146 6\n\nUse the -- function to decrement in integer mode. Use the inc function to\nincrement numbers floating point mode.")

(set '_def-new "{def-new\n\nsyntax: (def-new sym-source [sym-target])\n\nThis function works similarly to new, but it only creates a copy of one symbol\nand its contents from the symbol in sym-source. When sym-target is not given, a\nsymbol with the same name is created in the current context. All symbols\nreferenced inside sym-source will be translated into symbol references into the\ncurrent context, which must not be MAIN.\n\nIf an argument is present in sym-target, the copy will be made into a symbol\nand context as referenced by the symbol in sym-target. In addition to allowing\nrenaming of the function while copying, this also enables the copy to be placed\nin a different context. All symbol references in sym-source with the same\ncontext as sym-source will be translated into symbol references of the target\ncontext.\n\ndef-new returns the symbol created:\n\n> (set 'foo:var '(foo:x foo:y))\n(foo:x foo:y)\n\n> (def-new 'foo:var 'ct:myvar)\nct:myvar\n\n> ct:myvar\n(ct:x ct:y)\n\n> (context 'K)\n\nK> (def-new 'foo:var)\nvar\n\nK> var\n(x y)\n\nThe following example shows how a statically scoped function can be created by\nmoving it its own namespace:\n\n> (set 'temp (lambda (x) (+ x x)))\n(lambda (x) (+ x x))\n> (def-new 'temp 'double:double)\ndouble:double\n> (double 10)\n20\n> double:double\n(lambda (double:x) (+ double:x double:x))\n\nThe following definition of def-static can be used to create functions living\nin their own lexically protected name-space:\n\n(define (def-static s body)\n      (def-new 'body (sym s s)))\n\n(def-static 'acc (lambda (x)\n          (inc sum x)))\n\n> (acc 1)\n1\n> (acc 1)\n2\n> (acc 8)\n10\n>\n\nThe function def-new can also be used to configure contexts or context objects\nin a more granular fashion than is possible with new, which copies a whole\ncontext.")

(set '_default "{default\n\nsyntax: (default context)\n\nReturn the contents of the default functor in context.\n\n(define Foo:Foo 123)\n\n(default Foo) \226\134\146 123\n\n(setf (default Foo) 456)\n(set 'ctx Foo)\n\n(default ctx) \226\134\146 456\nFoo:Foo       \226\134\146 456\n\nIn many situations newLISP defaults automatically to the default functor when\nseeing a context name. In circumstances where this is not the case, the default\nfunction can be used.")

(set '_define [text]{define !

syntax: (define (sym-name [sym-param-1 ... ]) [body-1 ... ])
syntax: (define (sym-name [(sym-param-1 exp-default) ... ]) [body-1 ... ])
syntax: (define sym-name exp)

Defines the new function sym-name, with optional parameters sym-param-1—.
define is equivalent to assigning a lambda expression to sym-name. When calling
a defined function, all arguments are evaluated and assigned to the variables
in sym-param-1—, then the body-1— expressions are evaluated. When a function is
defined, the lambda expression bound to sym-name is returned.

All parameters defined are optional. When a user-defined function is called
without arguments, those parameters assume the value nil. If those parameters
have a default value specified in exp-default, they assume that value.

The return value of define is the assigned lambda expression. When calling a
user-defined function, the return value is the last expression evaluated in the
function body.

(define (area x y) (* x y))  → (lambda (x y) (* x y))
(area 2 3)                   → 6

As an alternative, area could be defined as a function without using define.

(set 'area (lambda (x y) (* x y))

lambda or fn expressions may be used by themselves as anonymous functions
without being defined as a symbol:

((lambda ( x y) (* x y)) 2 3)  → 6
((fn ( x y) (* x y)) 2 3)      → 6

fn is just a shorter form of writing lambda.

Parameters can have default values specified:

(define (foo (a 1) (b 2))
  (list a b))

(foo)      → (1 2)
(foo 3)    → (3 2)
(foo 3 4)  → (3 4)

Expressions in exp-default are evaluated in the function's current environment.

(define (foo (a 10) (b (div a 2)))
  (list a b))

(foo)      → (10 5)
(foo 30)   → (30 15)
(foo 3 4)  → (3 4)

The second version of define works like the set function.

(define x 123)  →   123
;; is equivalent to
(set 'x 123)    →   123

(define area (lambda ( x y) (* x y)))
;; is equivalent to
(set 'area (lambda ( x y) (* x y)))
;; is equivalent to
(define (area x y) (* x y))

Trying to redefine a protected symbol will cause an error message.[/text])

(set '_define-macro [text]{define-macro

syntax: (define-macro (sym-name [sym-param-1 ... ]) body)
syntax: (define-macro (sym-name [(sym-param-1 exp-default) ... ]) body)

Functions defined using define-macro are called fexprs in other LISPs as they
don't do variable expansion. In newLISP they are still called macros, because
they are written with the same purpose of creating special syntax forms with
non-standard evaluation patterns of arguments. Functions created using
define-macro can be combined with template expansion using expand or letex.

Defines the new fexpr sym-name, with optional arguments sym-param-1.
define-macro is equivalent to assigning a lambda-macro expression to a symbol.
When a define-macro function is called, unevaluated arguments are assigned to
the variables in sym-param-1 .... Then the body expressions are evaluated. When
evaluating the define-macro function, the lambda-macro expression is returned.

(define-macro (my-setq p1 p2) (set p1 (eval p2)))
→ (lambda-macro (p1 p2) (set p1 (eval p2)))

(my-setq x 123)  → 123
x                → 123

New functions can be created to behave like built-in functions that delay the
evaluation of certain arguments. Because fexprs can access the arguments inside
a parameter list, they can be used to create flow-control functions like those
already built-in to newLISP.

All parameters defined are optional. When a macro is called without arguments,
those parameters assume the value nil. If those parameters have a default value
specified in exp-default, they assume that default value.

(define-macro (foo (a 1) (b 2))
  (list a b))

(foo)      → (1 2)
(foo 3)    → (3 2)
(foo 3 4)  → (3 4)

Expressions in exp-default are evaluated in the function's current environment.

(define-macro (foo (a 10) (b (div a 2)))
  (list a b))

(foo)      → (10 5)
(foo 30)   → (30 15)
(foo 3 4)  → (3 4)

Note that in fexprs, the danger exists of passing a parameter with the same
variable name as used in the define-macro definition. In this case, the fexpr's
internal variable would end up receiving nil instead of the intended value:

;; not a good definition!

(define-macro (my-setq x y) (set x (eval y)))

;; symbol name clash for x

(my-setq x 123)  → 123
x                → nil

There are several methods that can be used to avoid this problem, known as
variable capture, by writing hygienic define-macros:

  • Put the definition into its own lexically closed namespace context. If the
    function has the same name as the context, it can be called by using the
    context name alone. A function with this characteristic is called a default
    function. This is the preferred method in newLISP to write define-macros.
  • Use args to access arguments passed by the function.

;; a define-macro as a lexically isolated function
;; avoiding variable capture in passed parameters

(context 'my-setq)

(define-macro (my-setq:my-setq x y) (set x (eval y)))

(context MAIN)

(my-setq x 123)  → 123  ; no symbol clash

The definition in the example is lexically isolated, and no variable capture
can occur. Instead of the function being called using (my-setq:my-setq …), it
can be called with just (my-setq …) because it is a default function.

The second possibility is to refer to passed parameters using args:

;; avoid variable capture in macros using the args function

(define-macro (my-setq) (set (args 0) (eval (args 1))))[/text])

(set '_delete "{delete\n\nsyntax: (delete symbol [bool])\nsyntax: (delete sym-context [bool])\n\nDeletes a symbol symbol, or a context in sym-context with all contained symbols\nfrom newLISP's symbol table. References to the symbol will be changed to nil.\n\nWhen the expression in bool evaluates to true, symbols are only deleted when\nthey are not referenced.\n\nWhen the expression in bool evaluates to nil, symbols will be deleted without\nany reference checking. Note that this mode should only be used, if no\nreferences to the symbol exist outside it's namespace. If external references\nexist, this mode can lead to system crashes, as the external reference is not\nset to nil when using this mode. This mode can be used to delete namespace\nhashes and to delete namespaces in object systems, where variables are strictly\ntreated as private.\n\nProtected symbols of built-in functions and special symbols like nil and true\ncannot be deleted.\n\ndelete returns true if the symbol was deleted successfully or nil if the symbol\nwas not deleted.\n\n(set 'lst '(a b aVar c d))\n\n(delete 'aVar)  ; aVar deleted, references marked nil\n\nlst  \226\134\146 (a b nil c d)\n\n(set 'lst '(a b aVar c d))\n\n(delete 'aVar true)\n\226\134\146 nil ; protect aVar if referenced\n\nlst  \226\134\146 (a b aVar c d)\n\n;; delete all symbols in a context\n(set 'foo:x 123)\n(set 'foo:y \"hello\")\n\n(delete 'foo)  \226\134\146 foo:x, foo:y deleted\n\nIn the last example only the symbols inside context foo will be deleted but not\nthe context symbol foo itself. It will be converted to a normal unprotected\nsymbol and contain nil.\n\nNote that deleting a symbol that is part of an expression which is currently\nexecuting can crash the system or have other unforeseen effects.")

(set '_delete-file "{delete-file\n\nsyntax: (delete-file str-file-name)\n\nDeletes a file given in str-file-name. Returns true if the file was deleted\nsuccessfully.\n\nOn failure the function returns nil. For error information, use sys-error when\nused on files. When used on URLs net-error gives more error information.\n\nThe file name can be given as a URL.\n\n(delete-file \"junk\")\n\n(delete-file \"http://asite.com/example.html\")\n\n(delete-file \"file://aFile.txt\")\n\nThe first example deletes the file junk in the current directory. The second\nexample shows how to use a URL to specify the file. In this form, additional\nparameters can be given. See delete-url for details.")

(set '_delete-url "{delete-url\n\nsyntax: (delete-file str-url)\n\nThis function deletes the file on a remote HTTP server specified in str-url.\nThe HTTP DELETE protocol must be enabled on the target web server, or an error\nmessage string may be returned. The target file must also have access\npermissions set accordingly. Additional parameters such as timeout and custom\nheaders are available exactly as in the get-url function.\n\nIf str-url starts with file:// a file on the local file system is deleted.\n\nThis feature is also available when the delete-file function is used and a URL\nis specified for the filename.\n\n(delete-url \"http://www.aserver.com/somefile.txt\")\n(delete-url \"http://site.org:8080/page.html\" 5000)\n\n; delete on the local file system\n(delete-url \"file:///home/joe/somefile.txt\")\n\nThe second example configures a timeout option of five seconds. Other options\nsuch as special HTTP protocol headers can be specified, as well. See the\nget-url function for details.")

(set '_destroy "{destroy\n\nsyntax: (destroy int-pid)\nsyntax: (destroy int-pid int-signal)\n\nDestroys a process with process id in int-pid and returns true on success or\nnil on failure. The process id is normally obtained from a previous call to\nfork on Mac OS X and other Unix or process on all platforms. On Unix, destroy\nworks like the system utility kill using the SIGKILL signal.\n\nCAUTION! If int-pid is 0 the signal is sent to all processes whose group ID is\nequal to the process group ID of the sender. If int-pid is -1 all processes\nwith the current user id will be killed, if newLISP is started with super user\nprivileges, all processes except system processes are destroyed.\n\nWhen specifying int-signal, destroy works like a Unix kill command sending the\nspecified Unix signal to the process in int-pid. This second syntax is not\navailable on Win32.\n\n(set 'pid (process \"/usr/bin/bc\" bcin bcout))\n(destroy pid)\n\n(set 'pid (fork (dotimes (i 1000) (println i) (sleep 10))))\n(sleep 100) (destroy pid)")

(set '_det "{det\n\nsyntax: (det matrix [float-pivot])\n\nReturns the determinant of a square matrix. A matrix can either be a nested\nlist or an array.\n\nOptionally 0.0 or a very small value can be specified in float-pivot. This\nvalue substitutes pivot elements in the LU-decomposition algorithm, which\nresult in zero when the algorithm deals with a singular matrix.\n\n(set 'A '((-1 1 1) (1 4 -5) (1 -2 0)))\n(det A)  \226\134\146 -1\n\n; treatment of singular matrices\n(det '((2 -1) (4 -2)))        \226\134\146 nil\n(det '((2 -1) (4 -2)) 0)      \226\134\146 -0\n(det '((2 -1) (4 -2)) 1e-20)  \226\134\146 -4e-20\n\nIf the matrix is singular and float-pivot is not specified, nil is returned.\n\nSee also the other matrix operations invert, mat, multiply and transpose.\n\n\ndevice\n\nsyntax: (device [int-handle])\n\nint-handle is an I/O device number, which is set to 0 (zero) for the default\nSTD I/O pair of handles, 0 for stdin and 1 for stdout. int-handle may also be a\nfile handle previously obtained using open. In this case both, input and output\nare channeled through this handle. When no argument is supplied, the current I/\nO device number is returned.\n\nThe I/O channel specified by device is used internally by the functions print\nand read-line. When the current I/O device is 0 or 1, print sends output to the\nconsole window and read-line accepts input from the keyboard. If the current I/\nO device has been set by opening a file, then print and read-line work on that\nfile.\n\n(device (open \"myfile\" \"write\"))  \226\134\146 5\n(print \"This goes in myfile\")     \226\134\146 \"This goes in myfile\"\n(close (device))                  \226\134\146 true\n\nNote that using close on device automatically resets device to 0 (zero).")

(set '_difference "{difference\n\nsyntax: (difference list-A list-B)\nsyntax: (difference list-A list-B bool)\n\nIn the first syntax, difference returns the set difference between list-A and\nlist-B. The resulting list only has elements occurring in list-A, but not in\nlist-B. All elements in the resulting list are unique, but list-A and list-B\nneed not be unique. Elements in the lists can be any type of Lisp expression.\n\n(difference '(2 5 6 0 3 5 0 2) '(1 2 3 3 2 1))  \226\134\146 (5 6 0)\n\nIn the second syntax, difference works in list mode. bool specifies true or an\nexpression not evaluating to nil. In the resulting list, all elements of list-B\nare eliminated in list-A, but duplicates of other elements in list-A are left.\n\n(difference '(2 5 6 0 3 5 0 2) '(1 2 3 3 2 1) true)  \226\134\146 (5 6 0 5 0)\n\nSee also the set functions intersect, unique and union.")

(set '_directory "{directory\n\nsyntax: (directory [str-path])\nsyntax: (directory str-path str-pattern [int-regex-option])\n\nA list of directory entry names is returned for the directory path given in\nstr-path. On failure, nil is returned. When str-path is omitted, the list of\nentries in the current directory is returned.\n\n(directory \"/bin\")\n\n(directory \"c:/\")\n\nThe first example returns the directory of /bin, the second line returns a list\nof directory entries in the root directory of drive C:. Note that on Win32\nsystems, a forward slash (/) can be included in path names. When used, a\nbackslash (\\) must be preceded by a second backslash.\n\nIn the second syntax, directory can take a regular expression pattern in\nstr-pattern. Only filenames matching the pattern will be returned in the list\nof directory entries. In int-regex-options, special regular expression options\ncan be specified; see regex for details.\n\n(directory \".\" \"\\\\.c\")  \226\134\146 (\"foo.c\" \"bar.c\")\n;; or using braces as string pattern delimiters\n(directory \".\" {\\.c})  \226\134\146 (\"foo.c\" \"bar.c\")\n\n; show only hidden files (starting with dot)\n(directory \".\" \"^[.]\")   \226\134\146 (\".\" \"..\" \".profile\" \".rnd\" \".ssh\")\n\nThe regular expression forces directory to return only file names containing\nthe string \".c\".\n\nOther functions that use regular expressions are find, find-all, parse, regex,\nreplace, and search.")

(set '_directory? "{directory?\n\nsyntax: (directory? str-path)\n\nChecks if str-path is a directory. Returns true or nil depending on the\noutcome.\n\n(directory? \"/etc\")             \226\134\146 true\n(directory? \"/usr/bin/emacs/\")  \226\134\146 nil")

(set '_div "{div\n\nsyntax: (div num-1 num-2 [num-3 ... ])\nsyntax: (div num-1)\n\nSuccessively divides num-1 by the number in num-2\226\128\148. div can perform mixed-type\narithmetic, but it always returns floating point numbers. Any floating point\ncalculation with NaN also returns NaN.\n\n(div 10 3)                 \226\134\146 3.333333333\n(div 120 (sub 9.0 6) 100)  \226\134\146 0.4\n\n(div 10)                   \226\134\146 0.1\n\nWhen num-1 is the only argument, div calculates the inverse of num-1.")

(set '_do-until "{do-until\n\nsyntax: (do-until exp-condition [body])\n\nThe expressions in body are evaluated before exp-condition is evaluated. If the\nevaluation of exp-condition is not nil, then the do-until expression is\nfinished; otherwise, the expressions in body get evaluated again. Note that\ndo-until evaluates the conditional expression after evaluating the body\nexpressions, whereas until checks the condition before evaluating the body. The\nreturn value of the do-until expression is the last evaluation of the body\nexpression. If body is empty, the last result of exp-condition is returned.\n\ndo-until also updates the system iterator symbol $idx.\n\n(set 'x 1)\n(do-until (> x 0) (inc x))\nx  \226\134\146 2\n\n(set 'x 1)\n(until (> x 0) (inc x))\nx  \226\134\146 1\n\nWhile do-until goes through the loop at least once, until never enters the\nloop.\n\nSee also the functions while and do-while.")

(set '_do-while "{do-while\n\nsyntax: (do-while exp-condition body)\n\nThe expressions in body are evaluated before exp-condition is evaluated. If the\nevaluation of exp-condition is nil, then the do-while expression is finished;\notherwise the expressions in body get evaluated again. Note that do-while\nevaluates the conditional expression after evaluating the body expressions,\nwhereas while checks the condition before evaluating the body. The return value\nof the do-while expression is the last evaluation of the body expression.\n\ndo-while also updates the system iterator symbol $idx.\n\n(set 'x 10)\n(do-while (< x 10) (inc x))\nx  \226\134\146 11\n\n(set 'x 10)\n(while (< x 10) (inc x))\nx  \226\134\146 10\n\nWhile do-while goes through the loop at least once, while never enters the\nloop.\n\nSee also the functions until and do-until.")

(set '_doargs "{doargs\n\nsyntax: (doargs (sym [exp-break]) body)\n\nIterates through all members of the argument list inside a user-defined\nfunction or macro. This function or macro can be defined using define,\ndefine-macro, lambda, or lambda-macro. The variable in sym is set sequentially\nto all members in the argument list until the list is exhausted or an optional\nbreak expression (defined in exp-break) evaluates to true or a logical true\nvalue. The doargs expression always returns the result of the last evaluation.\n\ndoargs also updates the system iterator symbol $idx.\n\n(define (foo)\n    (doargs (i) (println i)))\n\n> (foo 1 2 3 4)\n1\n2\n3\n4\n\nThe optional break expression causes doargs to interrupt processing of the\narguments:\n\n(define-macro (foo)\n    (doargs (i (= i 'x))\n        (println i)))\n\n> (foo a b x c d e)\na\nb\ntrue\n\nUse the args function to access the entire argument list at once.")

(set '_dolist "{dolist\n\nsyntax: (dolist (sym list [exp-break]) body)\n\nThe expressions in body are evaluated for each element in list. The variable in\nsym is set to each of the elements before evaluation of the body expressions.\nThe variable used as loop index is local and behaves according to the rules of\ndynamic scoping.\n\nOptionally, a condition for early loop exit may be defined in exp-break. If the\nbreak expression evaluates to any non-nil value, the dolist loop returns with\nthe value of exp-break. The break condition is tested before evaluating body.\n\n(set 'x 123)\n(dolist (x '(a b c d e f g))  ; prints: abcdefg\n    (print x))  \226\134\146 g          ; return value\n\n(dolist (x '(a b c d e f g) (= x 'e))  ; prints: abcd\n    (print x))\n\n;; x is local in dolist\n;; x has still its old value outside the loop\n\nx  \226\134\146 123  ; x has still its old value\n\nThis example prints abcdefg in the console window. After the execution of\ndolist, the value for x remains unchanged because the x in dolist has local\nscope. The return value of dolist is the result of the last evaluated\nexpression.\n\nThe internal system variable $idx keeps track of the current offset into the\nlist passed to dolist, and it can be accessed during its execution:\n\n(dolist (x '(a b d e f g))\n  (println $idx \":\" x))  \226\134\146 g\n\n0:a\n1:b\n2:d\n3:e\n4:f\n5:g\n\nThe console output is shown in boldface. $idx is protected and cannot be\nchanged by the user.")

(set '_dostring "{dostring utf8\n\nsyntax: (dostring (sym string [exp-break]) body)\n\nThe expressions in body are evaluated for each character in string. The\nvariable in sym is set to each ASCII or UTF-8 integer value of the characters\nbefore evaluation of the body expressions. The variable used as loop index is\nlocal and behaves according to the rules of dynamic scoping.\n\nOptionally, a condition for early loop exit may be defined in exp-break. If the\nbreak expression evaluates to any non-nil value, the dolist loop returns with\nthe value of exp-break. The break condition is tested before evaluating body.\n\n; ASCII example\n(set 'str \"abcdefg\")\n(dostring (c str) (println c \" - \" (char c)))\n\n97 - a\n98 - b\n99 - c\n100 - d\n101 - e\n102 - f\n103 - g\n\n; UTF8 example\n(set 'utf8str \"\230\136\145\232\131\189\229\144\158\228\184\139\231\142\187\231\146\131\232\128\140\228\184\141\228\188\164\232\186\171\228\189\147\227\128\130\")\n(dostring (c utf8str) (println c \" - \" (char c)))\n\n25105 - \230\136\145\n33021 - \232\131\189\n21534 - \229\144\158\n ...\n20307 - \228\189\147\n12290 - \227\128\130 \n\nThis example prints the value of each character in the console window. In UTF-8\nenabled versions of newLISP, individual characters may be longer than one byte\nand the number in the loop variable may exceed 255. The return value of\ndostring is the result of the last evaluated expression.\n\nThe internal system variable $idx keeps track of the current offset into the\nstring passed to dostring, and it can be accessed during its execution.")

(set '_dotimes "{dotimes\n\nsyntax: (dotimes (sym-var int-count [exp-break]) body)\n\nThe expressions in body are evaluated int times. The variable in sym is set\nfrom 0 (zero) to (int - 1) each time before evaluating the body expression(s).\nThe variable used as the loop index is local to the dotimes expression and\nbehaves according the rules of dynamic scoping. The loop index is of integer\ntype. dotimes returns the result of the last expression evaluated in body.\nAfter evaluation of the dotimes statement sym assumes its previous value.\n\nOptionally, a condition for early loop exit may be defined in exp-break. If the\nbreak expression evaluates to any non-nil value, the dotimes loop returns with\nthe value of exp-break. The break condition is tested before evaluating body.\n\n(dotimes (x 10)\n  (print x))  \226\134\146 9  ; return value\n\nThis prints 0123456789 to the console window.")

(set '_dotree "{dotree\n\nsyntax: (dotree (sym sym-context [bool]) body)\n\nThe expressions in body are evaluated for all symbols in sym-context. The\nsymbols are accessed in a sorted order. Before each evaluation of the body\nexpression(s), the variable in sym is set to the next symbol from sym-context.\nThe variable used as the loop index is local to the dotree expression and\nbehaves according the rules of dynamic scoping.\n\nWhen the optional bool expression evaluates to not nil, only symbols starting\nwith an underscore character _ are accessed. Symbol names starting with an _\nunderscore are used for hash keys and symbols created by bayes-train.\n\ndotree also updates the system iterator symbol $idx.\n\n;; faster and less memory overhead\n(dotree (s SomeCTX) (print s \" \"))\n\n;; slower and higher memory usage\n(dolist (s (symbols SomeCTX)) (print s \" \"))\n\nThis example prints the names of all symbols inside SomeCTX to the console\nwindow.")

(set '_dump "{dump\n\nsyntax: (dump [exp])\n\nShows the binary contents of a newLISP cell. Without an argument, this function\noutputs a listing of all Lisp cells to the console. When exp is given, it is\nevaluated and the contents of a Lisp cell are returned in a list.\n\n(dump 'a)   \226\134\146 (9586996 5 9578692 9578692 9759280)\n\n(dump 999)  \226\134\146 (9586996 130 9578692 9578692 999)\n\nThe list contains the following memory addresses and information:\n\noffset description\n0      memory address of the newLISP cell\n1      cell->type: major/minor type, see newlisp.h for details\n2      cell->next: linked list ptr\n       cell->aux:\n3          string length+1 or\n           low (little endian) or high (big endian) word of 64-bit integer or\n           low word of IEEE 754 double float\n       cell->contents:\n4          string/symbol address or\n           high (little endian) or low (big endian) word of 64-bit integer or\n           high word of IEEE 754 double float\n\n\nThis function is valuable for changing type bits in cells or hacking other\nparts of newLISP internals. See the function cpymem for a comprehensive\nexample.")

(set '_dup "{dup\n\nsyntax: (dup exp int-n [bool])\nsyntax: (dup exp)\n\nIf the expression in exp evaluates to a string, it will be replicated int-n\ntimes within a string and returned. When specifying an expression evaluating to\nanything other than nil in bool, the string will not be concatenated but\nreplicated in a list like any other data type.\n\nIf exp contains any data type other than string, the returned list will contain\nint-n evaluations of exp.\n\nWithout the repetition parameter, dup assumes 2.\n\n(dup \"A\" 6)       \226\134\146 \"AAAAAA\"\n(dup \"A\" 6 true)  \226\134\146 (\"A\" \"A\" \"A\" \"A\" \"A\" \"A\")\n(dup \"A\" 0)       \226\134\146 \"\"\n(dup \"AB\" 5)      \226\134\146 \"ABABABABAB\"\n(dup 9 7)         \226\134\146 (9 9 9 9 9 9 9)\n(dup 9 0)         \226\134\146 ()\n(dup 'x 8)        \226\134\146 (x x x x x x x x)\n(dup '(1 2) 3)    \226\134\146 ((1 2) (1 2) (1 2))\n(dup \"\\000\" 4)    \226\134\146 \"\\000\\000\\000\\000\"\n\n(dup \"*\")         \226\134\146 \"**\"\n\nThe last example shows handling of binary information, creating a string filled\nwith four binary zeroes.\n\nSee also the functions sequence and series.")

(set '_empty? "{empty?\n\nsyntax: (empty? exp)\nsyntax: (empty? str)\n\nexp is tested for an empty list (or str for an empty string). Depending on\nwhether the argument contains elements, true or nil is returned.\n\n(set 'var '())\n(empty? var)         \226\134\146 true\n(empty? '(1 2 3 4))  \226\134\146 nil\n(empty? \"hello\")     \226\134\146 nil\n(empty? \"\")          \226\134\146 true\n\nThe first example checks a list, while the second two examples check a string.")

(set '_encrypt "{encrypt\n\nsyntax: (encrypt str-source str-pad)\n\nPerforms a one-time\226\128\147pad encryption of str-source using the encryption pad in\nstr-pad. The longer str-pad is and the more random the bytes are, the safer the\nencryption. If the pad is as long as the source text, is fully random, and is\nused only once, then one-time\226\128\147pad encryption is virtually impossible to break,\nsince the encryption seems to contain only random data. To retrieve the\noriginal, the same function and pad are applied again to the encrypted text:\n\n(set 'secret\n  (encrypt \"A secret message\" \"my secret key\"))\n\226\134\146 \",YS\\022\\006\\017\\023\\017TM\\014\\022\\n\\012\\030E\"\n\n(encrypt secret \"my secret key\")  \226\134\146 \"A secret message\"\n\nThe second example encrypts a whole file:\n\n(write-file \"myfile.enc\"\n  (encrypt (read-file \"myfile\") \"29kH67*\"))")

(set '_ends-with "{ends-with\n\nsyntax: (ends-with str-data str-key [num-option])\nsyntax: (ends-with list exp)\n\nIn the first syntax, ends-with tests the string in str-data to see if it ends\nwith the string specified in str-key. It returns true or nil depending on the\noutcome.\n\nIf a regular expression option number is specified, str-key contains a regular\nexpression pattern. See regex for valid numbers for option.\n\n(ends-with \"newLISP\" \"LISP\")         \226\134\146 true\n(ends-with \"newLISP\" \"lisp\")         \226\134\146 nil\n;; use regular expressions\n(ends-with \"newLISP\" \"lisp|york\" 1)  \226\134\146 true\n\nIn the second syntax, ends-with checks if a list ends with the list element in\nexp. true or nil is returned depending on outcome.\n\n(ends-with '(1 2 3 4 5) 5)             \226\134\146 true\n(ends-with '(a b c d e) 'b)            \226\134\146 nil\n(ends-with '(a b c (+ 3 4)) '(+ 3 4))  \226\134\146 true\n\nThe last example shows that exp could be a list by itself.\n\nSee also the starts-with function.")

(set '_env "{env\n\nsyntax: (env)\nsyntax: (env var-str)\nsyntax: (env var-str value-str)\n\nIn the first syntax (without arguments), the operating system's environment is\nretrieved as an association list in which each entry is a key-value pair of\nenvironment variable and value.\n\n(env)\n\226\134\146 ((\"PATH\" \"/bin:/usr/bin:/sbin\") (\"TERM\" \"xterm-color\") ... ))\n\nIn the second syntax, the name of an environment variable is given in var-str.\nenv returns the value of the variable or nil if the variable does not exist in\nthe environment.\n\n(env \"PATH\")  \226\134\146 \"/bin:/usr/bin:/usr/local/bin\"\n\nThe third syntax (variable name in var-str and value pair in value-str) sets or\ncreates an environment variable. If value-str is the empty string \"\", then the\nvariable is completely removed from the environment except when running on\nSolaris, where the variable stays with an empty string.\n\n(env \"NEWLISPBIN\" \"/usr/bin/\")  \226\134\146 true\n(env \"NEWLISPBIN\")              \226\134\146 \"/usr/bin/\"\n(env \"NEWLISPBIN\" \"\")           \226\134\146 true\n(env \"NEWLISPBIN\")              \226\134\146 nil")

(set '_erf "{erf\n\nsyntax: (erf num)\n\nerf calculates the error function of a number in num. The error function is\ndefined as:\n\nerf (x) = 2/sqrt(pi) * integral from 0 to x of exp(-t^2) dt\n\n(map erf (sequence 0.0 6.0 0.5))\n\226\134\146\n(0 0.5204998778 0.8427007929 0.9661051465 0.995322265 0.999593048\n 0.9999779095 0.9999992569 0.9999999846 0.9999999998 1 1 1)")

(set '_error-event "{error-event\n\nsyntax: (error-event sym-event-handler | func-event-handler)\n\nsym-event-handler contains a user-defined function for handling errors.\nWhenever an error occurs, the system performs a reset and executes the\nuser-defined error handler. The error handler can use the built-in function\nlast-error to retrieve the number and text of the error. The event handler is\nspecified as either a quoted symbol or a lambda function.\n\n(define (my-handler)\n  (print \"error # \" (first (last-error)) \" has occurred\\n\") )\n\n(error-event 'my-handler)  \226\134\146 my-handler\n\n;; specify a function directly\n\n(error-event my-handler)  \226\134\146 $error-event\n\n(error-event\n  (fn () (print \"error # \" (first (last-error)) \" has occurred\\n\")))\n\n(error-event exit)  \226\134\146 $error-event\n\nFor a different way of handling errors, see the catch function. Use throw-error\nto throw user-defined errors.")

(set '_eval "{eval\n\nsyntax: (eval exp)\n\neval evaluates the result of evaluating exp in the current variable\nenvironment.\n\n(set 'expr '(+ 3 4))  \226\134\146 (+ 3 4)\n(eval expr)           \226\134\146 7\n(eval (list + 3 4))   \226\134\146 7\n(eval ''x)            \226\134\146 x\n(set 'y 123)\n(set 'x 'y)\nx            \226\134\146 y\n(eval x)     \226\134\146 123\n\nAs usual, evaluation of variables happens in the current variable environment:\n\n; eval in global (top level) environment\n(set 'x 3 'y 4)\n(eval '(+ x y))          \226\134\146 7\n\n; eval in local environment\n(let ( (x 33) (y 44) )\n    (eval '(+ x y)))     \226\134\146 77\n\n; old environment after leaving local let environment\n(eval '(+ x y))          \226\134\146 7\n\nnewLISP passes all arguments by value. Using a quoted symbol, expressions can\nbe passed by reference through the symbol. eval can be used to access the\noriginal contents of the symbol:\n\n(define (change-list aList) (push 999 (eval aList)))\n\n(set 'data '(1 2 3 4 5))\n\n(change-list 'data)  \226\134\146 (999 1 2 3 4 5)\n\nIn the example, the parameter 'data is quoted, so push can work on the original\nlist.\n\nThere is a safer method to pass arguments by reference in newLISP by enclosing\nthe data inside context objects. See the chapter Passing data by reference.\nPassing references into user defined function using namespace ids avoids\nvariable capture of the passed symbol, in case the symbol passed is the same\nused as a parameter in the function.")

(set '_eval-string "{eval-string\n\nsyntax: (eval-string str-source [sym-context [exp-error [int-offset]]])\n\nThe string in str-source is compiled into newLISP's internal format and then\nevaluated. The evaluation result is returned. If the string contains more than\none expression, the result of the last evaluation is returned.\n\nAn optional second argument can be used to specify the context to which the\nstring should be parsed and translated.\n\nIf an error occurs while parsing and evaluating str-source then exp-error will\nbe evaluated and the result returned.\n\nint-offset specifies an optional offset into str-source, where to start\nevaluation.\n\n(eval-string \"(+ 3 4)\")  \226\134\146 7\n(set 'X 123)             \226\134\146 123\n(eval-string \"X\")        \226\134\146 123\n\n(define (repl) ; read print eval loop\n  (while true\n    (println \"=> \" (eval-string (read-line) MAIN (last-error)))\n  )\n)\n\n(set 'a 10)\n(set 'b 20)\n(set 'foo:a 11)\n(set 'foo:b 22)\n\n(eval-string \"(+ a b)\")       \226\134\146 30\n(eval-string \"(+ a b)\" 'foo)  \226\134\146 33\n\nThe second example shows a simple newLISP interpreter eval loop.\n\nThe last example shows how to specify a target context for translation. The\nsymbols a and b now refer to symbols and their values in context foo instead of\nMAIN.\n\nSee also the function read-expr which translates a string without evaluating\nit.")

(set '_even? "{even?\n\nsyntax: (even? int-number)\n\nChecks if an integer number is even divisable by 2, without remainder. When a\nfloating point number is passed for int-number, it will be converted to an\ninteger by cutting off its fractional part.\n\n(even? 123)  \226\134\146 nil\n(even? 8)    \226\134\146 true\n(even? 8.7)  \226\134\146 true\n\nUse odd? to check if an integer is not divisable by 2.")

(set '_exec "{exec\n\nsyntax: (exec str-process)\nsyntax: (exec str-process [str-stdin])\n\nIn the first form, exec launches a process described in str-process and returns\nall standard output as a list of strings (one for each line in standard out\n(STDOUT)). exec returns nil if the process could not be launched. If the\nprocess could be launched but only returns and error and no valid output, the\nempty list will be returned.\n\n(exec \"ls *.c\")  \226\134\146 (\"newlisp.c\" \"nl-math.c\" \"nl-string.c\")\n\nThe example starts a process and performs the shell command ls, capturing the\noutput in an array of strings.\n\nIn the second form, exec creates a process pipe, starts the process in\nstr-process, and receives from str-stdin standard input for this process. The\nreturn value is true if the process was successfully launched; otherwise it is\nnil.\n\n(exec \"cgiProc\" query)\n\nIn this example, cgiProc could be a cgi processor (e.g., Perl or newLISP) that\nreceives and processes standard input supplied by a string contained in the\nvariable query.")

(set '_exists "{exists\n\nsyntax: (exists func-condition list)\n\nSuccessively applies func-condition to the elements of list and returns the\nfirst element that meets the condition in func-condition. If no element meets\nthe condition, nil is returned.\n\n(exists string? '(2 3 4 6 \"hello\" 7))       \226\134\146 \"hello\"\n\n(exists string? '(3 4 2 -7 3 0))            \226\134\146 nil\n\n(exists zero? '(3 4 2 -7 3 0))              \226\134\146 0 ; check for 0 or 0.0\n\n(exists < '(3 4 2 -7 3 0))                  \226\134\146 -7 ; check for negative\n\n(exists (fn (x) (> x 3)) '(3 4 2 -7 3 0))   \226\134\146 4\n\n(exists (fn (x) (= x 10)) '(3 4 2 -7 3 0))  \226\134\146 nil\n\nIf func-condition is nil?, the result nil is ambiguous. In this case index or\nfind are the better method when looking for nil.\n\nUse the for-all function to check if a condition is met for all elements in a\nlist.")

(set '_exit "{exit\n\nsyntax: (exit [int])\n\nExits newLISP. An optional exit code, int, may be supplied. This code can be\ntested by the host operating system. When newLISP is run in daemon server mode\nusing -d as a command-line option, only the network connection is closed, while\nnewLISP stays resident, listening for a new connection.\n\n(exit 5)")

(set '_exp "{exp\n\nsyntax: (exp num)\n\nThe expression in num is evaluated, and the exponential function is calculated\nbased on the result. exp is the inverse function of log.\n\n(exp 1)        \226\134\146 2.718281828\n(exp (log 1))  \226\134\146 1")

(set '_expand [text]{expand

syntax: (expand exp sym-1 [sym-2 ... ])
syntax: (expand exp list-assoc [bool])
syntax: (expand exp)

In the first syntax, one symbol in sym (or more in sym-2 through sym-n) is
looked up in a simple or nested expression exp. They are then expanded to the
current binding of the symbol and the expanded expression is returned. The
original list remains unchanged.

(set 'x 2 'a '(d e))
(set 'foo 'a)
(expand foo 'a)               → (d e)
(expand '(a x b) 'x)           → (a 2 b)
(expand '(a x (b c x)) 'x)     → (a 2 (b c 2))
(expand '(a x (b c x)) 'x 'a)  → ((d e) 2 (b c 2))

expand is useful when composing lambda expressions or doing variable expansion
inside macros.

(define (raise-to power)
  (expand (fn (base) (pow base power)) 'power))

(define square (raise-to 2))
(define cube (raise-to 3))

(square 5)  → 25
(cube 5)    → 125

If more than one symbol is present, expand will work in an incremental fashion:

(set 'a '(b c))
(set 'b 1)

(expand '(a b c) 'a 'b)  → ((1 c) 1 c)

Like the apply function, expand reduces its argument list.

syntax: (expand list list-assoc [bool])

The second syntax of expand allows expansion bindings to be specified on the
fly, without performing a set on the participating variables:

If the bool evaluates to true, the value parts in the association list are
evaluated.

(expand '(a b c) '((a 1) (b 2)))                → (1 2 c)
(expand '(a b c) '((a 1) (b 2) (c (x y z))))    → (1 2 (x y z))
(expand '(a b) '((a (+ 1 2)) (b (+ 3 4))))      → ((+ 1 2) (+ 3 4))
(expand '(a b) '((a (+ 1 2)) (b (+ 3 4))) true) → (3 7)

Note that the contents of the variables in the association list will not
change. This is different from the letex function, where variables are set by
evaluating and assigning their association parts.

This form of expand is frequently used in logic programming, together with the
unify function.

syntax: (expand list)

A third syntax is used to expand only the contents of variables starting with
an uppercase character. This PROLOG mode may also be used in the context of
logic programming. As in the first syntax of expand, symbols must be preset.
Only uppercase variables and those bound to anything other than nil will be
expanded:

(set 'A 1 'Bvar 2 'C nil 'd 5 'e 6)
(expand '(A (Bvar) C d e f))  → (1 (2) C d e f)

Only the symbols A and Bvar are expanded because they have capitalized names
and non-nil contents.

The currying function in the example demonstrating the first syntax of expand
can now be written even more simply using an uppercase variable:

(define (raise-to Power)
  (expand (fn (base) (pow base Power))))

> (define cube (raise-to 3))
(lambda (base) (pow base 3))

> (cube 4)
64

> _

See the letex function, which also provides an expansion mechanism, and the
function unify, which is frequently used together with expand.[/text])

(set '_explode [text]{explode utf8

syntax: (explode str [int-chunk [bool]])
syntax: (explode list [int-chunk [bool]])

In the first syntax, explode transforms the string (str) into a list of
single-character strings. Optionally, a chunk size can be specified in
int-chunk to break the string into multi-character chunks. When specifying a
value for bool other than nil, the last chunk will be omitted if it does not
have the full length specified in int-chunk.

(explode "newLISP")  → ("n" "e" "w" "L" "I" "S" "P")

(join (explode "keep it together"))  → "keep it together"

(explode "newLISP" 2)    → ("ne" "wL" "IS" "P")

(explode "newLISP" 3)    → ("new" "LIS" "P")

; omit last chunk if too short
(explode "newLISP" 3 true)    → ("new" "LIS")

Only on non UTF8– enabled versions, explode also works on binary content:

(explode "\000\001\002\003")
→ ("\000" "\001" "\002" "\003")

When called in UTF-8–enabled versions of newLISP, explode will work on
character boundaries rather than byte boundaries. In UTF-8–encoded strings,
characters may contain more than one byte. Processing will stop when a zero
byte character is found.

To explode binary contents on UTF-8–enabled versions of newLISP use unpack as
shown in the following example:

(set 'str "\001\002\003\004") → "\001\002\003\004"

(unpack (dup "c" (length str)) str) → (1 2 3 4)
(unpack (dup "s" (length str)) str) → ("\001" "\002" "\003" "\004")

In the second syntax, explode explodes a list (list) into sublists of chunk
size int-chunk, which is 1 (one) by default.

The following shows an example of the last chunk being omitted when the value
for bool is other than nil, and the chunk does not have the full length
specified in int-chunk.

(explode '(a b c d e f g h))    → ((a) (b) (c) (d) (e) (f) (g) (h))
(explode '(a b c d e f g) 2)  → ((a b) (c d) (e f) (g))

; omit last chunk if too short
(explode '(a b c d e f g) 2 true)  → ((a b) (c d) (e f))

(transpose (explode '(a b c d e f g h) 2))
→ ((a c e g) (b d f h))

The join and append functions are inverse operations of explode.[/text])

(set '_extend "{extend !\n\nsyntax: (extend list-1 [list-2 ... ])\nsyntax: (extend string-1 [string-2 ... ])\n\nThe list in list-1 is extended by appending list-2. More than one list may be\nappended.\n\nThe string in string-1 is extended by appending string-2. More than one string\nmay be appended. The string can contain binary 0 (zero) characters.\n\nThe first parameter can be an un-initialized variable.\n\nThe extended list or string is returned.\n\n; extending lists\n\n(extend lst '(a b) '(c d)) \226\134\146 (a b c d)\n(extend lst '(e f g)) \226\134\146 (a b c d e f)\nlst \226\134\146 (a b c d e f g)\n\n; extending strings\n\n(extend str \"ab\" \"cd\") \226\134\146 \"abcd\"\n(extend str \"efg\") \226\134\146 \"abcdefg\"\nstr \226\134\146 \"abcdefg\"\n\n; extending in place\n\n(set 'L '(a b \"CD\" (e f)))\n(extend (L 2) \"E\")\nL \226\134\146 (a b \"CDE\" (e f))\n\n(extend (L 3) '(g))\nL \226\134\146 (a b \"CDE\" (e f g))\n\nFor a non-destructive list or string extension see append.")

(set '_factor "{factor\n\nsyntax: (factor int)\n\nFactors the number in int into its prime components. When floating point\nnumbers are passed, they are truncated to their integer part first.\n\n(factor 123456789123456789)  \226\134\146 (3 3 7 11 13 19 3607 3803 52579)\n\n;; check correctness of factoring\n(= (apply * (factor 123456789123456789)) 123456789123456789)\n\226\134\146 true\n\n;; factor the biggest integer\n(factor 9223372036854775807)  \226\134\146 (7 7 73 127 337 92737 649657)\n\n;; primes.lsp - return all primes in a list, up to n\n\n(define (primes n , p)\n  (dotimes (e n)\n    (if (= (length (factor e)) 1)\n      (push e p -1))) p)\n\n(primes 20)  \226\134\146 (2 3 5 7 11 13 17 19)\n\nfactor returns nil for numbers smaller than 2. For numbers larger than\n9,223,372,036,854,775,807 (the largest 64-bit integer) converted from floating\npoint numbers, the largest integer is factored.")

(set '_fft "{fft\n\nsyntax: (fft list-num)\n\nCalculates the discrete Fourier transform on the list of complex numbers in\nlist-num using the FFT method (Fast Fourier Transform). Each complex number is\nspecified by its real part followed by its imaginary part. If only real numbers\nare used, the imaginary part is set to 0.0 (zero). When the number of elements\nin list-num is not a power of 2, fft increases the number of elements by\npadding the list with zeroes. When the imaginary part of a complex number is 0,\nsimple numbers can be used instead.\n\n(ifft (fft '((1 0) (2 0) (3 0) (4 0))))\n\226\134\146 ((1 0) (2 0) (3 0) (4 0))\n\n;; when imaginary part is 0, plain numbers work, too\n;; complex numbers can be intermixed\n\n(fft '(1 2 3 4))      \226\134\146 ((10 0) (-2 -2) (-2 0) (-2 2))\n(fft '(1 2 (3 0) 4))  \226\134\146 ((10 0) (-2 -2) (-2 0) (-2 2))\n\nThe inverse operation of fft is the ifft function.")

(set '_file-info "{file-info\n\nsyntax: (file-info str-name [int-index [bool-flag]])\n\nReturns a list of information about the file or directory in str_name. The\noptional index specifies the list member to return. When no bool-flag is\nspecified or when bool-flag evaluates to nil information about the link is\nreturned if the file is a link to an original file. If bool-flag evaluates to\nanything else than nil, information about the original file referenced by the\nlink is returned.\n\noffset contents\n0      size\n1      mode (differs with true flag)\n2      device mode\n3      user ID\n4      group ID\n5      access time\n6      modification time\n7      status change time\n\n\nDepending on bool-flag set, the function reports on either the link (no flag or\nnil flag) or on the original linked file (true flag).\n\n(file-info \".bashrc\")\n\226\134\146 (124 33188 0 500 0 920951022 920951022 920953074)\n\n(file-info \".bashrc\" 0)  \226\134\146 124\n\n(date (file-info \"/etc\" -1))  \226\134\146 \"Mon Mar 8 18:23:17 2005\"\n\nIn the second example, the last status change date for the directory /etc is\nretrieved.\n\nfile-info gives file statistics (size) for a linked file, not the link, except\nfor the mode field.")

(set '_file? "{file?\n\nsyntax: (file? str-path-name [bool])\n\nChecks for the existence of a file in str-name. Returns true if the file\nexists; otherwise, it returns nil. This function will also return true for\ndirectories. If the optional bool value is true, str-path-name is returned. The\nexistence of a file does not imply anything about its read or write\npermissions. A file may exist while not having the permissions to read from or\nwrite to it by the current user.\n\n(if (file? \"afile\") (set 'fileNo (open \"afile\" \"read\")))\n\n(file? \"/usr/bin/newlisp\" true) \226\134\146 \"/usr/bin/newlisp\"\n(file? \"/usr/bin/foo\" true)     \226\134\146 nil")

(set '_filter "{filter\n\nsyntax: (filter exp-predicate exp-list)\n\nThe predicate exp-predicate is applied to each element of the list exp-list. A\nlist is returned containing the elements for which exp-predicate is true.\nfilter works like clean, but with a negated predicate.\n\n(filter symbol? '(1 2 d 4 f g 5 h))  \226\134\146 (d f g h)\n\n(define (big? x) (> x 5))  \226\134\146 (lambda (x) (> x 5))\n\n(filter big? '(1 10 3 6 4 5 11))  \226\134\146 (10 6 11)\n\n; filter with comparison functor\n(set 'L '((a 10 2 7) (b 5) (a 8 3) (c 8) (a 9)))\n\n(filter (curry match '(a *)) L)   \226\134\146 ((a 10 2 7) (a 8 3) (a 9))\n\n(filter (curry match '(? ?)) L)   \226\134\146 ((b 5) (c 8) (a 9))\n\n(filter (curry match '(* 8 *)) L) \226\134\146 ((a 8 3) (c 8))\n\nThe predicate may be a built-in predicate, a user-defined function, or a lambda\nexpression.\n\nFor filtering a list of elements with the elements from another list, use the\ndifference function or intersect (with the list option).\n\nSee also the related function index, which returns the indices of the filtered\nelements and clean, which returns all elements of a list for which a predicate\nis false.")

(set '_find [text]{find

syntax: (find exp-key list [func-compare | int-regex-option])
syntax: (find str-key str-data [int-regex-option [int-offset]])

Find an expression in a list

If the second argument evaluates to a list, then find returns the index
position (offset) of the element derived from evaluating exp-key.

Optionally, an operator or user-defined function can be specified in
func-compare. If the exp-key is a string, a regular expression option can be
specified with the int-regex-option parameter.

When using regular expressions or comparison functors the system variable $0 is
set to the last element found.

; find an expression in a list
(find '(1 2) '((1 4) 5 6 (1 2) (8 9)))  → 3

(find "world" '("hello" "world"))       → 1
(find "hi" '("hello" "world"))          → nil

(find "newlisp" '("Perl" "Python" "newLISP") 1)  → 2

; use the comparison functor
(find 3 '(8 4 3  7 2 6) >)  → 4
$0 → 2

(find "newlisp" '("Perl" "Python" "newLISP")
                 (fn (x y) (regex x y 1))) → 2
$0 → "newLISP"

(find 5 '((l 3) (k 5) (a 10) (z 22))
         (fn (x y) (= x (last y))))  → 1
$0 → (k 5)

(find '(a ?) '((l 3) (k 5) (a 10) (z 22)) match)  → 2
$0 → (a 10)

(find '(X X) '((a b) (c d) (e e) (f g)) unify)  → 2
$0 → (e e)

; define the comparison functor first for better readability
(define (has-it-as-last x y) (= x (last y)))

(find 22 '((l 3) (k 5) (a 10) (z 22)) has-it-as-last)  → 3
$0 → (z 22)

Using match and unify, list searches can be formulated which are as powerful as
regular expression searches are for strings.

Find a string in a string

If the second argument, str-data, evaluates to a string, then the offset
position of the string str-key (found in the first argument, str-data) is
returned. In this case, find also works on binary str-data. The offset position
returned is always based on counting single byte characters even when running
the UTF-8 enabled version of newLISP.

The presence of a third parameter specifies a search using the regular
expression pattern specified in str-pattern, as well as an option number
specified in int-regex-option (i.e., 1 (one) for case-insensitive search or 0
(zero) for no special options). If int-regex-option is specified an optional
int-offset argument can be specified too to start the search not at the
beginning but at the offset given. In any case the position returned by find is
calculated relative to the beginning of the string.

To specify int-offset in a simple string search without regular expressions,
specify nil for int-regex-option.

In newLISP, regular expressions are standard Perl Compatible Regular Expression
(PCRE) searches. Found expressions or subexpressions are returned in the system
variables $0, $1, $2, etc., which can be used like any other symbol. As an
alternative, the contents of these variables can also be accessed by using ($
0), ($ 1), ($ 2), etc. This method allows indexed access (i.e., ($ i), where i
is an integer).

See regex for the meaning of the option numbers and more information on regular
expression searching.

; simple string search
(find "world" "Hello world")  → 6
(find "WORLD" "Hello woRLd")  → nil

; case-insensitive regex

(find "WorlD" "Hello woRLd" 1)  → 6

(find "hi" "hello world")       → nil
(find "Hello" "Hello world")    → 0

; regex with default options

(find "cat|dog" "I have a cat" 0)  → 9
$0                                 → "cat"
(find "cat|dog" "my dog" 0)        → 3
$0                                 → "dog"
(find "cat|dog" "MY DOG" 1)        → 3
$0                                 → "DOG"

; use an optional offset
(find "cat|dog" "I have a cat and a dog" 0)    → 9
(find "cat|dog" "I have a cat and a dog" 0 12) → 19

;; find with subexpressions in regular expression
;; and access with system variables

(set 'str  "http://nuevatec.com:80")

(find "http://(.*):(.*)" str 0)  → 0

$0  → "http://nuevatec.com:80"
$1  → "nuevatec.com"
$2  → "80"

;; system variables as an indexed expression (since 8.0.5)
($ 0)  → "http://nuevatec.com:80"
($ 1)  → "nuevatec.com"
($ 2)  → "80"

For other functions using regular expressions, see directory, find-all, parse,
regex, replace, and search.

To find expressions in nested or multidimensional lists, use the ref and
ref-all functions.[/text])

(set '_find-all [text]{find-all

syntax: (find-all str-regex-pattern str-text [exp [int-regex-option]])
syntax: (find-all list-match-pattern list-lists [exp])
syntax: (find-all exp-key list exp func-compare)

In the first syntax, find-all finds all occurrences of str-regex-pattern in the
text str-text, returning a list containing all matching strings. The empty list
() is returned if no matches are found.

Optionally, an expression can be specified to process the found string or
regular subexpressions before placing them into the returned list. An
additional option, int-option, specifies special regular expression options
(see regex for further details).

(find-all {\d+} "lkjhkljh34ghfdhgfd678gfdhfgd9")
→ ("34" "678" "9")

(find-all {(new)(lisp)} "newLISPisNEWLISP" (append $2 $1) 1)
→ ("LISPnew" "LISPNEW")

(unique (sort
    (find-all {[a-zA-Z]+}
        (replace "<[^>]+>" (get-url "http://newlisp.org") "" 0) )
))
→ ("A" "ACC" "AI" "API" "About" "All" "Amazing" "Apps"
...
"where" "whole" "width" "wiki" "will" "with" "work" "written")

The first example discovers all numbers in a text. The second example shows how
an optional expression in exp can work on subexpressions found by the regular
expression pattern in str-pattern. The last example retrieves a web page,
cleans out all HTML tags, and then collects all words into a unique and sorted
list.

Note that find-all with strings always performs a regular expression search,
even if the option in int-option is omitted.

In the second syntax, find-all searches for all list match patterns
list-match-pattern in list-lists. As in find-all for strings, an expression can
be specified in exp to process further the matched sublist:

(find-all '(? 2) '((a 1) (b 2) (a 2) (c 4))) → ((b 2) (a 2))

(find-all '(? 2) '((a 1) (b 2) (a 2) (c 4)) (first $it)) → (b a)

find-all for list matches always uses match to compare when searching for
sublists and always needs a list for the pattern expression.

In the third syntax, find-all can specify a built-in or user-defined function
used for comparing list elements with the key expression in exp-key:

(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) $it <) → (7 9 9 7 8)

; process the found element available in $it

(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) (* 3 $it) <) → (21 27 27 21 24)
; same as
(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) (* 3 $it) (fn (x y) (< x y))) → (21 27 27 21 24)


(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) ("abcdefghijk" $it) <) → ("h" "j" "j" "h" "i")

Any type of expression can be searched for or can be contained in the list.
find-all in this syntax works similar to filter but with the added benefit of
being able to define a processing expression for the found element.[/text])

(set '_first "{first utf8\n\nsyntax: (first list)\nsyntax: (first array)\nsyntax: (first str)\n\nReturns the first element of a list or the first character of a string. The\noperand is not changed. This function is equivalent to car or head in other\nLisp dialects.\n\n(first '(1 2 3 4 5))       \226\134\146 1\n(first '((a b) c d))       \226\134\146 (a b)\n(set 'aList '(a b c d e))  \226\134\146 (a b c d e)\n(first aList)              \226\134\146 a\naList                      \226\134\146 (a b c d e)\n(set 'A (array 3 2 (sequence 1 6)))\n\226\134\146  ((1 2) (3 4) (5 6))\n(first A)                  \226\134\146 (1 2)\n\n(first '())                \226\134\146 ERR: list is empty\n\nIn the third syntax, the first character is returned from the string in str as\na string.\n\n(first \"newLISP\")         \226\134\146 \"n\"\n(first (rest \"newLISP\"))  \226\134\146 \"e\"\n\nNote that first works on character boundaries rather than byte boundaries when\nthe UTF-8\226\128\147enabled version of newLISP is used. See also the functions last and\nrest.")

(set '_flat "{flat\n\nsyntax: (flat list)\n\nReturns a flattened list from a list:\n\n(set 'lst '(a (b (c d))))\n(flat lst)  \226\134\146 (a b c d)\n\n(map (fn (x) (ref x lst)) (flat lst))\n\226\134\146 ((0) (1 0) (1 1 0) (1 1 1))\n\nflat can be used to iterate through nested lists.")

(set '_float "{float\n\nsyntax: (float exp [exp-default])\n\nIf the expression in exp evaluates to a number or a string, the argument is\nconverted to a float and returned. If exp cannot be converted to a float then\nnil or, if specified, the evaluation of exp-default will be returned. This\nfunction is mostly used to convert strings from user input or when reading and\nparsing text. The string must start with a digit or the + (plus sign), - (minus\nsign), or . (period). If exp is invalid, float returns nil as a default value.\n\nFloats with exponents larger than 1e308 or smaller than -1e308 are converted to\n+INF or -INF, respectively. The display of +INF and -INF differs on different\nplatforms and compilers.\n\n(float \"1.23\")       \226\134\146 1.23\n(float \" 1.23\")      \226\134\146 1.23\n(float \".5\")         \226\134\146 0.50\n(float \"-1.23\")      \226\134\146 -1.23\n(float \"-.5\")        \226\134\146 nil\n(float \"#1.23\")      \226\134\146 nil\n(float \"#1.23\" 0.0)  \226\134\146 0\n\n(float? 123)          \226\134\146 nil\n(float? (float 123))  \226\134\146 true\n\n(float '(a b c))    \226\134\146 nil\n(float '(a b c) 0)  \226\134\146 0\n(float nil 0)       \226\134\146 0\n\n(float \"abc\" \"not a number\")  \226\134\146 \"not a number\"\n(float \"1e500\")               \226\134\146 inf\n(float \"-1e500\")              \226\134\146 -inf\n\n(print \"Enter a float num:\")\n(set 'f-num (float (read-line)))\n\nUse the int function to parse integer numbers.")

(set '_float? "{float?\n\nsyntax: (float? exp)\n\ntrue is returned only if exp evaluates to a floating point number; otherwise,\nnil is returned.\n\n(set 'num 1.23)\n(float? num)  \226\134\146 true")

(set '_floor "{floor\n\nsyntax: (floor number)\n\nReturns the next lowest integer below number as a floating point.\n\n(floor -1.5)  \226\134\146 -2\n(floor 3.4)   \226\134\146 3\n\nSee also the ceil function.")

(set '_flt "{flt\n\nsyntax: (flt number)\n\nConverts number to a 32-bit float represented by an integer. This function is\nused when passing 32-bit floats to library routines. newLISP floating point\nnumbers are 64-bit and are passed as 64-bit floats when calling imported C\nlibrary routines.\n\n(flt 1.23)  \226\134\146 1067282596\n\n;; pass 32-bit float to C-function: foo(float value)\n(import \"mylib.so\" \"foo\")\n(foo (flt 1.23))\n\n(get-int (pack \"f\" 1.23))  \226\134\146 1067282596\n\n(unpack \"f\" (pack \"ld\" (flt 1.2345)))  \226\134\146 (1.234500051)\n\nThe last two statements illustrate the inner workings of flt.\n\nUse the import function to import libraries.")

(set '_fn "{fn\n\nsyntax: (fn (list-parameters) exp-body)\n\nfn is used to define anonymous functions, which are frequently used in map,\nsort, and many other functions where functions can be used as arguments.\n\nUsing an anonymous function eliminates the need to define a new function with\ndefine. Instead, a function is defined on the fly:\n\n(map (fn (x) (+ x x)) '(1 2 3 4 5)) \226\134\146 (2 4 6 8 10)\n\n(sort '(\"..\" \"...\" \".\" \".....\") (fn (x y) (> (length x) (length y))))\n\226\134\146 (\".....\" \"...\" \"..\" \".\")\n\nThe example defines the function fn(x), which takes an integer (x) and doubles\nit. The function is mapped onto a list of arguments using map. The second\nexample shows strings being sorted by length.\n\nThe lambda function (the longer, traditional form) can be used in place of fn.")

(set '_for "{for\n\nsyntax: (for (sym num-from num-to [num-step [exp-break]]) body)\n\nRepeatedly evaluates the expressions in body for a range of values specified in\nnum-from and num-to, inclusive. A step size may be specified with num-step. If\nno step size is specified, 1 is assumed.\n\nOptionally, a condition for early loop exit may be defined in exp-break. If the\nbreak expression evaluates to any non-nil value, the for loop returns with the\nvalue of exp-break. The break condition is tested before evaluating body. If a\nbreak condition is defined, num-step must be defined, too.\n\nThe symbol sym is local in dynamic scope to the for expression. It takes on\neach value successively in the specified range as an integer value if no step\nsize is specified, or as a floating point value when a step size is present.\nAfter evaluation of the for statement sym assumes its previous value.\n\n> (for (x 1 10 2) (println x))\n1\n3\n5\n7\n9\n\n> (for (x 8 6 0.5) (println x))\n8\n7.5\n7\n6.5\n6\n\n> (for (x 1 100 2 (> (* x x) 30)) (println x))\n1\n3\n5\ntrue\n> _\n\nThe second example uses a range of numbers from highest to lowest. Note that\nthe step size is always a positive number. In the third example, a break\ncondition is tested.\n\nUse the sequence function to make a sequence of numbers.")

(set '_for-all "{for-all\n\nsyntax: (for-all func-condition list)\n\nApplies the function in func-condition to all elements in list. If all elements\nmeet the condition in func-condition, the result is true; otherwise, nil is\nreturned.\n\n(for-all number? '(2 3 4 6 7))                 \226\134\146 true\n\n(for-all number? '(2 3 4 6 \"hello\" 7))         \226\134\146 nil\n\n(for-all (fn (x) (= x 10)) '(10 10 10 10 10))  \226\134\146 true\n\nUse the exists function to check if at least one element in a list meets a\ncondition.")

(set '_fork [text]{fork

syntax: (fork exp)

The expression in exp is launched as a newLISP child process-thread of the
platforms OS. The new process inherits the entire address space, but runs
independently so symbol or variable contents changed in the child process will
not affect the parent process or vice versa. The child process ends when the
evaluation of exp finishes.

On success, fork returns with the child process ID; on failure, nil is
returned. See also the wait-pid function, which waits for a child process to
finish.

This function is only available on Linux/Unix versions of newLISP and is based
on the fork() implementation of the underlying OS.

A much simpler automated method to launch processes and collect results is
available with spawn and the Cilk API.

> (set 'x 0)
0
> (fork (while (< x 20) (println (inc x)) (sleep 1000)))
176

> 1
2
3
4
5
6

The example illustrates how the child process-thread inherits the symbol space
and how it is independent of the parent process. The fork statement returns
immediately with the process ID 176. The child process increments the variable
x by one each second and prints it to standard out (boldface). In the parent
process, commands can still be entered. Type x to see that the symbol x still
has the value 0 (zero) in the parent process. Although statements entered will
mix with the display of the child process output, they will be correctly input
to the parent process.

The second example illustrates how pipe can be used to communicate between
processes.

#!/usr/bin/newlisp

(define (count-down-proc x channel)
  (while (!= x 0)
      (write-line channel (string x))
      (dec x)))

(define (observer-proc channel)
  (do-until (= i "1")
    (println "process " (setq i (read-line channel)))))

(map set '(in out) (pipe))
(set 'observer (fork (observer-proc in)))
(set 'counter (fork (count-down-proc 5 out)))

; avoid zombies
(wait-pid observer)
(wait-pid counter)

(exit)

The following output is generated by observer-proc

process 5
process 4
process 3
process 2
process 1

The count-down-proc writes numbers to the communication pipe, where they are
picked up by the observer-process and displayed.

A forked process can either exit by itself or it can be destroyed using the
destroy function.

(define (fork-destroy-demo)
    (set 'pid (fork (dotimes (i 1000) (println i) (sleep 10))))
    (sleep 50)
    (destroy pid)
)

> (fork-destroy-demo)
0
1
2
3
4
true
>

The process started by fork-destroy-demo will not finish but is destroyed 50
milli-seconds after start by a call to destroy.

Use the semaphore function for synchronizing processes and share for sharing
memory between processes.

See spawn for a much simpler and automated way to synchronize processes and
collect results.[/text])

(set '_format [text]{format

syntax: (format str-format exp-data-1 [exp-data-2 ... ])
syntax: (format str-format list-data)

Constructs a formatted string from exp-data-1 using the format specified in the
evaluation of str-format. The format specified is identical to the format used
for the printf() function in the ANSI C language. Two or more exp-data
arguments can be specified for more than one format specifier in str-format.

In an alternative syntax, the data to be formatted can be passed inside a list
in list-data.

format checks for a valid format string, matching data type, and the correct
number of arguments. Wrong formats or data types result in error messages. int,
float, or string can be used to ensure correct data types and to avoid error
messages.

The format string has the following general format:

"%w.pf"

The % (percent sign) starts a format specification. To display a % inside a
format string, double it: %%

The w represents the width field. Data is right-aligned, except when preceded
by a minus sign, in which case it is left-aligned. If preceded by a + (plus
sign), positive numbers are displayed with a +. When preceded by a 0 (zero),
the unused space is filled with leading zeroes. The width field is optional and
serves all data types.

The p represents the precision number of decimals (floating point only) or
strings and is separated from the width field by a period. Precision is
optional. When using the precision field on strings, the number of characters
displayed is limited to the number in p.

The f represents a type flag and is essential; it cannot be omitted.

Below are the types in f:

format description
s      text string
c      character (value 1 - 255)
d      decimal (32-bit)
u      unsigned decimal (32-bit)
x      hexadecimal lowercase
X      hexadecimal uppercase
o      octal (32-bits) (not supported on all compilers)
f      floating point
e      scientific floating point
E      scientific floating point
g      general floating point


Formatting 64-bit numbers using 32-bit format specifiers will truncate and
format the lower 32 bits of the number.

For 64-bit numbers use the following format strings on Unix-like operating
systems:

format description
lld    decimal (64-bit)
llu    unsigned decimal (64-bit)
llx    hexadecimal (64-bit)
llX    hexadecimal uppercase(64-bit)


For 64-bit numbers (since version 8.9.7) use the following format strings on
Tru64 Unix:

format description
ld     decimal (64-bit)
lu     unsigned decimal (64-bit)
lx     hexadecimal (64-bit)
lX     hexadecimal uppercase(64-bit)


On Win32 platforms the following characters apply for 64 bit numbers:

format description
I64d   decimal (64-bit)
I64u   unsigned decimal (64-bit)
I64x   hexadecimal (64-bit)
I64X   hexadecimal uppercase(64-bit)


Other text may occur between, before, or after the format specs.

Note that on Tru64 Unix the format character i can be used instead of d.

(format ">>>%6.2f<<<" 1.2345)     → ">>>  1.23<<<"
(format ">>>%-6.2f<<<" 1.2345)    → ">>>1.23  <<<"
(format ">>>%+6.2f<<<" 1.2345)    → ">>> +1.23<<<"
(format ">>>%+6.2f<<<" -1.2345)   → ">>> -1.23<<<"
(format ">>>%-+6.2f<<<" -1.2345)  → ">>>-1.23 <<<"

(format "%e" 123456789)        → "1.234568e+08"
(format "%12.10E" 123456789)   → "1.2345678900E+08"

(format "%10g" 1.23)   → "      1.23"
(format "%10g" 1.234)  → "     1.234"

(format "Result = %05d" 2)  → "Result = 00002"

(format "%-15s" "hello")        → "hello          "
(format "%15s %d" "hello" 123)  → "          hello 123"
(format "%5.2s" "hello")        → "   he"
(format "%-5.2s" "hello")       → "he   "

(format "%o" 80)    → "120"

(format "%x %X" -1 -1)  → "ffffffff FFFFFFFF"

; 64 bit numbers on Windows
(format "%I64X" 123456789012345678)  → "1B69B4BA630F34E"

; 64 bit numbers on Unix (except TRU64)
(format "%llX" 123456789012345678)   → "1B69B4BA630F34E"

(format "%c" 65)  → "A"

The data to be formatted can be passed inside a list:

(set 'L '("hello" 123))
(format "%15s %d" L)  → "          hello 123"

If the format string requires it, newLISP's format will automatically convert
integers into floating points or floating points into integers:

(format "%f" 123)      → 123.000000

(format "%d" 123.456)  → 123[/text])

(set '_fv "{fv\n\nsyntax: (fv num-rate num-nper num-pmt num-pv [int-type])\n\nCalculates the future value of a loan with constant payment num-pmt and\nconstant interest rate num-rate after num-nper period of time and a beginning\nprincipal value of num-pv. If payment is at the end of the period, int-type is\n0 (zero) or int-type is omitted; for payment at the beginning of each period,\nint-type is 1.\n\n(fv (div 0.07 12) 240 775.30 -100000)  \226\134\146 -0.5544645052\n\nThe example illustrates how a loan of $100,000 is paid down to a residual of\n$0.55 after 240 monthly payments at a yearly interest rate of 7 percent.\n\nSee also the functions irr, nper, npv, pmt, and pv.")

(set '_gammai "{gammai\n\nsyntax: (gammai num-a num-b)\n\nCalculates the incomplete Gamma function of values a and b in num-a and num-b,\nrespectively.\n\n(gammai 4 5)  \226\134\146 0.7349740847\n\nThe incomplete Gamma function is used to derive the probability of Chi\194\178 to\nexceed a given value for a degree of freedom, df, as follows:\n\n    Q(Chi\194\178|df) = Q(df/2, Chi\194\178/2) = gammai(df/2, Chi\194\178/2)\n\nSee also the prob-chi2 function.")

(set '_gammaln "{gammaln\n\nsyntax: (gammaln num-x)\n\nCalculates the log Gamma function of the value x in num-x.\n\n(exp (gammaln 6))  \226\134\146 120\n\nThe example uses the equality of n! = gamma(n + 1) to calculate the factorial\nvalue of 5.\n\nThe log Gamma function is also related to the Beta function, which can be\nderived from it:\n\n    Beta(z,w) = Exp(Gammaln(z) + Gammaln(w) - Gammaln(z+w))")

(set '_gcd "{gcd\n\nsyntax: (gcd int-1 [int-2 ... ])\n\nCalculates the greatest common divisor of a group of integers. The greatest\ncommon divisor of two integers that are not both zero is the largest integer\nthat divides both numbers. gcd will calculate the greatest common divisor for\nthe first two integers in int-i and then further reduce the argument list by\ncalculating the greatest common divisor of the result and the next argument in\nthe parameter list.\n\n(gcd 0)        \226\134\146 0\n(gcd 0 0)      \226\134\146 0\n(gcd 10)       \226\134\146 10\n(gcd 12 36)    \226\134\146 12\n(gcd 15 36 6)  \226\134\146 3\n\nSee Wikipedia for details and theory about gcd numbers in mathematics.")

(set '_get-char "{get-char\n\nsyntax: (get-char int-address)\n\nGets an 8-bit character from an address specified in int-address. This function\nis useful when using imported shared library functions with import.\n\nchar * foo(void)\n        {\n        char * result;\n        result = \"ABCDEFG\";\n        return(result);\n        }\n\nConsider the above C function from a shared library, which returns a character\npointer (address to a string).\n\n(import \"mylib.so\" \"foo\")\n(print (get-char (foo) ))       \226\134\146  65 ; ASCII \"A\"\n(print (get-char (+ (foo) 1)))  \226\134\146  66 ; ASCII \"B\"\n\nNote that it is unsafe to use the get-char function with an incorrect address\nin int-address. Doing so could result in the system crashing or becoming\nunstable.\n\nSee also the address, get-int, get-long, get-float, get-string, pack, and\nunpack functions.")

(set '_get-float "{get-float\n\nsyntax: (get-float int-address)\n\nGets a 64-bit double float from an address specified in int-address. This\nfunction is helpful when using imported shared library functions (with import)\nthat return an address pointer to a double float or a pointer to a structure\ncontaining double floats.\n\ndouble float * foo(void)\n        {\n        double float * result;\n        \226\128\166\n        *result = 123.456;\n        return(result);\n        }\n\nThe previous C function is compiled into a shared library.\n\n(import \"mylib.so\" \"foo\")\n(get-float (foo))  \226\134\146 123.456\n\nfoo is imported and returns a pointer to a double float when called. Note that\nget-float is unsafe when used with an incorrect address in int-address and may\nresult in the system crashing or becoming unstable.\n\nSee also the address, get-int, get-long, get-char, get-string, pack, and unpack\nfunctions.")

(set '_get-int "{get-int\n\nsyntax: (get-int int-address)\n\nGets a 32-bit integer from the address specified in int-address. This function\nis handy when using imported shared library functions with import, a function\nreturning an address pointer to an integer, or a pointer to a structure\ncontaining integers.\n\nint * foo(void)\n        {\n        int * result;\n        \226\128\166\n        *result = 123;\n        return(result);\n        }\n\nint foo-b(void)\n        {\n        int result;\n        \226\128\166\n        result = 456;\n        return(result);\n        }\n\nConsider the C function foo (from a shared library), which returns an integer\npointer (address of an integer).\n\n(import \"mylib.so\" \"foo\")\n(get-int (foo))  \226\134\146 123\n(foo-b)          \226\134\146 456\n\nNote that using get-int with an incorrect address in int-address is unsafe and\ncould result in the system crashing or becoming unstable.\n\nSee also the address, get-char, get-float, get-long, get-string, pack, and\nunpack functions.")

(set '_get-long "{get-long\n\nsyntax: (get-long int-address)\n\nGets a 64-bit integer from the address specified in int-address. This function\nis handy when using import to import shared library functions, a function\nreturning an address pointer to a long integer, or a pointer to a structure\ncontaining long integers.\n\nlong long int * foo(void)\n        {\n        int * result;\n        \226\128\166\n        *result = 123;\n        return(result);\n        }\n\nlong long int foo-b(void)\n        {\n        int result;\n        \226\128\166\n        result = 456;\n        return(result);\n        }\n\nConsider the C function foo (from a shared library), which returns an integer\npointer (address of an integer).\n\n(import \"mylib.so\" \"foo\")\n(get-int (foo))  \226\134\146 123\n(foo-b)          \226\134\146 456\n\nNote that using get-long with an incorrect address in int-address is unsafe and\ncould result in the system crashing or becoming unstable.\n\nSee also the address, get-char, get-float, get-int, get-string, pack, and\nunpack functions.")

(set '_get-string "{get-string\n\nsyntax: (get-string int-address)\n\nGets a character string from the address specified in int-address. This\nfunction is helpful when using imported shared library functions with import.\n\nchar * foo(void)\n        {\n        char * result;\n        result = \"ABCDEFG\";\n        return(result);\n        }\n\nConsider the above C function from a shared library, which returns a character\npointer (address to a string).\n\n(import \"mylib.so\" \"foo\")\n(print (get-string (foo)))  \226\134\146 \"ABCDEFG\"\n\nWhen a string is passed as an argument, get-string will take its address as the\nargument. Because get-string always breaks off at the first first \\000 (null\ncharacter) it encounters, it can be used to retrieve a string from a buffer:\n\n(set 'buff \"ABC\\000\\000\\000DEF\")  \226\134\146 \"ABC\\000\\000\\000DEF\"\n\n(length buff)  \226\134\146 9\n\n(get-string buff)  \226\134\146 \"ABC\"\n\n(length (get-string buff))  \226\134\146 3\n\n; get a string from offset into a buffer\n(get-string (+ (address buff) 6)) \226\134\146 \"DEF\"\n\n; use unpack to get the whole buffer\n(unpack \"s9\" buff)  \226\134\146 (\"ABC\\000\\000\\000DEF\")\n\nSee also the get-char, get-int, get-float, pack, and unpack functions.\n\nNote that get-string can crash the system or make it unstable if the wrong\naddress is specified.")

(set '_get-url [text]{get-url

syntax: (get-url str-url [str-option] [int-timeout [str-header]])

Reads a web page or file specified by the URL in str-url using the HTTP GET
protocol. Both http:// and file:// URLs are handled. "header" can be specified
in the optional argument str-option to retrieve only the header. The option
"list" causes header and page information to be returned as separate strings in
a list.

A "debug" option can be specified either alone or after the "header" or "list"
option separated by one character, i.e. "header debug" or "list debug".
Including "debug" outputs all outgoing information to the console window.

The optional argument int-timeout can specify a value in milliseconds. If no
data is available from the host after the specified timeout, get-url returns
the string ERR: timeout. When other error conditions occur, get-url returns a
string starting with ERR: and the description of the error.

get-url handles redirection if it detects a Location: spec in the received
header and automatically does a second request. get-url also understands the
Transfer-Encoding: chunked format and will unpack data into an unchunked
format.

get-url requests are also understood by newLISP server nodes.

(get-url "http://www.nuevatec.com")
(get-url "http://www.nuevatec.com" 3000)
(get-url "http://www.nuevatec.com" "header")
(get-url "http://www.nuevatec.com" "header" 5000)
(get-url "http://www.nuevatec.com" "list")

(get-url "file:///home/db/data.txt") ; access local file system

(env "HTTP_PROXY" "http://ourproxy:8080")
(get-url "http://www.nuevatec.com/newlisp/")

The index page from the site specified in str-url is returned as a string. In
the third line, only the HTTP header is returned in a string. Lines 2 and 4
show a timeout value being used.

The second example shows usage of a file:// URL to access /home/db/data.txt on
the local file system.

The third example illustrates the use of a proxy server. The proxy server's URL
must be in the operating system's environment. As shown in the example, this
can be added using the env function.

The int-timeout can be followed by an optional custom header in str-header:

Custom header

The custom header may contain options for browser cookies or other directives
to the server. When no str-header is specified, newLISP sends certain header
information by default. After the following request:

(get-url "http://somehost.com" 5000)

newLISP will configure and send the request and header below:

GET / HTTP/1.1
Host: somehost.com
User-Agent: newLISP v8800
Connection: close

As an alternative, the str-header option could be used:

(get-url "http://somehost.com" 5000
    "User-Agent: Mozilla/4.0\r\nCookie: name=fred\r\n")

newLISP will now send the following request and header:

GET / HTTP/1.1
Host: somehost.com
User-Agent: Mozilla/4.o
Cookie: name=fred
Connection: close

Note that when using a custom header, newLISP will only supply the GET request
line, as well as the Host: and Connection: header entries. newLISP inserts all
other entries supplied in the custom header between the Host: and Connection:
entries. Each entry must end with a carriage return line-feed pair: \r\n.

See an HTTP transactions reference for valid header entries.

Custom headers can also be used in the put-url and post-url functions.[/text])

(set '_global "{global\n\nsyntax: (global sym-1 [sym-2 ... ])\n\nOne or more symbols in sym-1 [sym-2 ... ] can be made globally accessible from\ncontexts other than MAIN. The statement has to be executed in the MAIN context,\nand only symbols belonging to MAIN can be made global. global returns the last\nsymbol made global.\n\n(global 'aVar 'x 'y 'z)  \226\134\146 z\n\n(define (foo x)\n  (\226\128\166))\n\n(constant (global 'foo))\n\nThe second example shows how constant and global can be combined into one\nstatement, protecting and making a previous function definition global.")

(set '_global? "{global?\n\nsyntax: (global? sym)\n\nChecks if symbol in sym is global. Built-in functions, context symbols, and all\nsymbols made global using the function global are global:\n\nglobal? 'print)   \226\134\146 true\n(global 'var)     \226\134\146 var\n(global? 'var)    \226\134\146 true\n\n(constant (global 'foo))\n\n(global? 'foo)    \226\134\146 true")

(set '_if "{if\n\nsyntax: (if exp-condition exp-1 [exp-2])\nsyntax: (if exp-cond-1 exp-1 exp-cond-2 exp-2 [ ... ])\n\nIf the value of exp-condition is neither nil nor an empty list, the result of\nevaluating exp-1 is returned; otherwise, the value of exp-2 is returned. If\nexp-2 is absent, the value of exp-condition is returned.\n\n(set 'x 50)                   \226\134\146 50\n(if (< x 100) \"small\" \"big\")  \226\134\146 \"small\"\n(set 'x 1000)                 \226\134\146 1000\n(if (< x 100) \"small\" \"big\")  \226\134\146 \"big\"\n(if (> x 2000) \"big\")         \226\134\146 nil\n\nThe second form of if works similarly to cond, except it does not take\nparentheses around the condition-body pair of expressions. In this form, if can\nhave an unlimited number of arguments.\n\n(define (classify x)\n    (if\n        (< x 0) \"negative\"\n        (< x 10) \"small\"\n        (< x 20) \"medium\"\n        (>= x 30) \"big\"\n        \"n/a\"))\n\n(classify 15)   \226\134\146 \"medium\"\n(classify 100)  \226\134\146 \"big\"\n(classify 22)   \226\134\146 \"n/a\"\n(classify -10)  \226\134\146 \"negative\"\n\nThe last expression, \"n/a\", is optional. When this option is omitted, the\nevaluation of (>= x 30) is returned, behaving exactly like a traditional cond\nbut without requiring parentheses around the condition-expression pairs.\n\nIn any case, the whole if expression always returns the last expression or\ncondition evaluated.\n\nSee also the unless function.")

(set '_ifft "{ifft\n\nsyntax: (ifft list-num)\n\nCalculates the inverse discrete Fourier transform on a list of complex numbers\nin list-num using the FFT method (Fast Fourier Transform). Each complex number\nis specified by its real part, followed by its imaginary part. In case only\nreal numbers are used, the imaginary part is set to 0.0 (zero). When the number\nof elements in list-num is not an integer power of 2, ifft increases the number\nof elements by padding the list with zeroes. When complex numbers are 0 in the\nimaginary part, simple numbers can be used.\n\n(ifft (fft '((1 0) (2 0) (3 0) (4 0))))\n\226\134\146 ((1 0) (2 0) (3 0) (4 0))\n\n;; when imaginary part is 0, plain numbers work too\n\n(ifft (fft '(1 2 3 4)))\n\226\134\146 ((1 0) (2 0) (3 0) (4 0))\n\nThe inverse operation of ifft is the fft function.")

(set '_import [text]{import

syntax: (import str-lib-name str-function-name ["cdecl"])
syntax: (import str-lib-name str-function-name str-return-type [str-param-type
. . .])
syntax: (import str-lib-name)

Imports the function specified in str-function-name from a shared library named
in str-lib-name. Depending on the syntax used, string labels for return and
parameter types can be specified

If the libary in str-lib-name is not in the system's library path, the full
path name should be specified.

A function can be imported only once. A repeated import of the same function
will simply return the same - already allocated - function address.

On libffi enabled versions - capable of the second extended syntax - imported
symbols are protected against change and can only be modified using constant.

The third syntax - on OSX, Linux and other Unix only - allows loading libraries
without importing functions. This is necessary when other libraries need access
to functions they need internally.

The simple import syntax

Most library functions can be imported using the simpler first syntax. The API
expects all function arguments to be passed on the stack in either cdecl or
stdcall conventions. On 32-bit platforms, integers, pointers to strings and
buffers sometimes floating point values can be passed as parameters. On 64-bit
platforms only integers can be passed but no floating point values. As return
values only 32-bit or 64-bit values and pointers are allowed. No floating point
numbers can be returned. Strings must be retrieved with the get-string helper
function. Regardless of these limitations, most modules included in the
distribution use this simple import API.

If pointers are returned to strings or structures the following helper
functions can be used extract data: get-char, get-int, get-float, get-string,
unpack

To pass pointers for data structures the following functions help to pack data
and calculate addresses: address, pack.

To transform newLISP data types into the data types needed by the imported
function, use the functions float for 64-bit double floats, flt for 32-bit
floats, and int for 32-bit integers. By default, newLISP passes floating point
numbers as 64-bit double floats, integers as 32-bit integers, and strings as
32-bit integers for string addresses (pointers in C). Floats can only be used
with 32-bit versions of newLISP and libraries. To use floating point numbers in
a 64-bit environment use the extended import syntax.

;; define LIBC platform independent

(define LIBC (lookup ostype '(
    ("Win32" "msvcrt.dll")
    ("OSX" "libc.dylib")

(printf "%g %s %d %c\n" 1.23 "hello" 999 65)
1.23 hello 999 A
→ 17 ; return value

;; import Win32 DLLs in Win32 versions

(import "kernel32.dll" "GetTickCount")  → GetTickCount
(import "user32.dll" "MessageBoxA")     → MessageBoxA
(GetTickCount)                          → 3328896

In the first example, the string "1.23 hello 999 A" is printed as a side
effect, and the value 17 (number of characters printed) is returned. Any C
function can be imported from any shared library in this way.

The message box example pops up a Windows dialog box, which may be hidden
behind the console window. The console prompt does not return until the 'OK'
button is pressed in the message box.

;;this pops up a message box

(MessageBoxA 0 "This is the body" "Caption" 1)

The other examples show several imports of Win32 DLL functions and the details
of passing values by value or by reference. Whenever strings or numbers are
passed by reference, space must be reserved beforehand.

(import "kernel32.dll" "GetWindowsDirectoryA")

;; allocating space for a string return value
(set 'str (dup "\000" 64))  ; reserve space and initialize

(GetWindowsDirectoryA str (length str))

str  → "C:\\WINDOWS\000\000\000 ... "

;; use trim or get-string to cut of binary zeros
(get-string str)  → "C:\\WINDOWS"
(trim str)        → "C:\\WINDOWS"

(import "kernel32.dll" "GetComputerNameA")

;; allocate memory and initialize to zeros
(set 'str (dup "\000" 64))
(set 'len (length str)

;; call the function
;; the length of the string is passed as address reference
;; string str is automatically past by address (C pointer)
(GetComputerNameA str (address len))

str  → "LUTZ-PC\000\000 ... "

(trim str)  → "LUTZ-PC"

import returns the address of the function, which can be used to assign a
different name to the imported function.

(set 'imprime (import "libc.so.6" "printf"))
→ printf@400862A0

(imprime "%s %d" "hola" 123)
→ "hola 123"

The Win32 and Cygwin versions of newLISP uses standard call stdcall conventions
to call DLL library routines by default. This is necessary for calling DLLs
that belong to the Win32 operating system. Most third-party DLLs are compiled
for C declaration cdecl calling conventions and may need to specify the string
"cdecl" as an additional last argument when importing functions. newLISP
compiled for Mac OS X, Linux and other Unix systems uses the cdecl calling
conventions by default and ignores any additional string.

;; force cdecl calling conventions on Win32
(import "sqlite.dll" "sqlite_open" "cdecl")  → sqlite_open <673D4888>

Imported functions may take up to fourteen arguments. Note that floating point
arguments take up two spaces each (e.g., passing five floats takes up ten of
the fourteen parameters).

The extended import syntax

The extended import API works with the second syntax. It is based on the
popular libffi library and is pre-installed on most OS platforms. The API works
with all atomic C data types for passed parameters and return values. The
extended API requires that parameter types are specified in the import
statement as string type labels. Programs written with extended import API will
run without change on 32-bit and 64-bit newLISP and libraries. Integers,
floating point values and strings can be returned without using helper
functions.

The following types can be specified for the return value in str-return-type
and for function parameters in str-param-type:

label          C type for return value and      newLISP return and argument
               arguments                        type
"void"         void (only as return type)       nil is returned
"byte"         byte unsigned 8 bit              integer
"char"         char signed 8 bit                integer
"unsigned      unsigned short int 16 bit        integer
short int"
"short int"    short int signed 16 bit          integer
"unsigned int" unsigned int 32 bit              integer
"int"          int signed 32 bit                integer
"long"         long signed 32 or 64 bit         integer
               depending on platform
"long long"    long long signed 64 bit          integer
"float"        float 32 bit                     IEEE-754 64 bit float cut to
                                                32-bit precision
"double"       double 64 bit                    IEEE-754 64 bit float
                                                displayable string return (zero
"char*"        char* 32 or 64 bit ptr depending terminated)
               on platform                      string buffer arg (no addr.
                                                since 10.4.2)
               void* 32 or 64 bit ptr depending integer address return
"void*"        on platform                      either string buffer or integer
                                                address arg

The types "char*" and "void* can be interchanged and are treated identical
inside libffi. Depending on the type of arguments passed and the type of return
values, one or the other is used.

Aggregate types can be composed using the struct function.

The following examples show how the extended import syntax can handle return
values of floating point values and strings:

;; return a float value, LIBC was defined earlier
;             name   return   arg
(import LIBC "atof" "double" "char*")
(atof "3.141") → 3.141

;; return a copied string
;             name     return  arg-1   arg-2
(import LIBC "strcpy" "char*" "char*" "char*")
(set 'from "Hello World")

(set 'to (dup "\000" (length from))) ; reserve memory
(strcpy to from) → "Hello World"

The char* type takes a string buffer only. The "void* type can take either a
string buffer or a memory address number as input. When using "void*" as a
return type the address number of the result bufffer will be returned. This is
useful when returning pointers to data structures. These pointers can then be
used with unpack and struct for destructuring. In the following example the
return type is changed to void*:

(import LIBC "strcpy" "void*" "char*" "char*")
(set 'from "Hello World")
(set 'to (dup "\000" (length from)))

(strcpy to from)       → 2449424
(address to)           → 2449424
(unpack "s11" 2449424) → "Hello World"
(get-string 2449424)   → "Hello World"
to                     → "Hello World"

A newLISP string is always passed by it's address reference.

For a more complex example see this OpenGL demo.

Memory management

Any allocation performed by imported foreign functions has to be de-allocated
manually if there's no call in the imported API to do so. See the Code Patterns
in newLISP document for an example.

In case of calling foreign functions with passing by reference, memory for
variables needs to be allocated beforehand by newLISP — see import of
GetWindowsDirectoryA above — and hence, memory needs not be deallocated
manually, because it is managed automatically by newLISP.[/text])

(set '_inc "{inc !\n\nsyntax: (inc place [num])\n\nIncrements the number in place by 1.0 or by the optional number num and returns\nthe result. inc performs float arithmetic and converts integer numbers passed\ninto floating point type.\n\nplace is either a symbol or a place in a list structure holding a number, or a\nnumber returned by an expression.\n\n(set 'x 0)    \226\134\146 0\n(inc x)       \226\134\146 1\nx             \226\134\146 1\n(inc x 0.25)  \226\134\146 1.25\nx             \226\134\146 1.25\n(inc x)       \226\134\146 2.25\n\nIf a symbol for place contains nil, it is treated as if containing 0.0:\n\nz             \226\134\146 nil\n(inc z)       \226\134\146 1\n\n(set 'z nil)\n(inc z 0.01)  \226\134\146 0.01\n\nPlaces in a list structure or a number returned by another expression can be\nupdated too:\n\n(set 'l '(1 2 3 4))\n\n(inc (l 3) 0.1) \226\134\146 4.1\n\n(inc (first l)) \226\134\146 2\n\nl \226\134\146 (2 2 3 4.1)\n\n(inc (+ 3 4)) \226\134\146 8\n\nUse the ++ function for incrementing numbers in integer mode. Use dec to\ndecrement numbers in floating point mode.")

(set '_index "{index\n\nsyntax: (index exp-predicate exp-list)\n\nApplies the predicate exp-predicate to each element of the list exp-list and\nreturns a list containing the indices of the elements for which exp-predicate\nis true.\n\n(index symbol? '(1 2 d 4 f g 5 h))  \226\134\146 (2 4 5 7)\n\n(define (big? x) (> x 5))  \226\134\146 (lambda (x) (> x 5))\n\n(index big? '(1 10 3 6 4 5 11))  \226\134\146 (1 3 6)\n\n(select '(1 10 3 6 4 5 11) '(1 3 6)) \226\134\146 (1 3 6)\n\nThe predicate may be a built-in predicate, a user-defined function, or a lambda\nexpression.\n\nUse the filter function to return the elements themselves.")

(set '_inf? "{inf?\n\nsyntax: (inf? float)\n\nIf the value in float is infinite the function returns true else nil.\n\n(inf? (div 1 0)) \226\134\146 true\n\n(div 0 0) \226\134\146 NaN\n\nNote that an integer division by zero e.g. (/ 1 0) will throw an \"division by\nzero\" error and not yield infinity. See also NaN? to check if a floating point\nnumber is valid.")

(set '_int "{int\n\nsyntax: (int exp [exp-default [int-base]])\n\nIf the expression in exp evaluates to a number or a string, the result is\nconverted to an integer and returned. If exp cannot be converted to an integer,\nthen nil or the evaluation of exp-default will be returned. This function is\nmostly used when translating strings from user input or from parsing text. If\nexp evaluates to a string, the string must start with a digit; one or more\nspaces; or the + or - sign. The string must begin with '0x' for hexadecimal\nstrings or '0' (zero) for octal strings. If exp is invalid, int returns nil as\na default value if not otherwise specified.\n\nA second optional parameter can be used to force the number base of conversion\nto a specific value.\n\nIntegers larger than 9,223,372,036,854,775,807 are truncated to\n9,223,372,036,854,775,807. Integers smaller than -9,223,372,036,854,775,808 are\ntruncated to -9,223,372,036,854,775,808.\n\nWhen converting from a float (as in the second form of int), floating point\nvalues larger or smaller than the integer maximum or minimum are also\ntruncated. A floating point expression evaluating to NaN is converted to 0\n(zero).\n\n(int \"123\")          \226\134\146 123\n(int \" 123\")         \226\134\146 123\n(int \"a123\" 0)       \226\134\146 0\n(int (trim \" 123\"))  \226\134\146 123\n(int \"0xFF\")         \226\134\146 255\n(int \"055\")          \226\134\146 45\n(int \"1.567\")        \226\134\146 1\n(int 1.567)          \226\134\146 1\n\n(integer? 1.00)        \226\134\146 nil\n(integer? (int 1.00))  \226\134\146 true\n\n(int \"1111\" 0 2)  \226\134\146 15   ; base 2 conversion\n(int \"0FF\" 0 16)  \226\134\146 255  ; base 16 conversion\n\n(int 'xyz)     \226\134\146 nil\n(int 'xyz 0)   \226\134\146 0\n(int nil 123)  \226\134\146 123\n\n(int \"abc\" (throw-error \"not a number\"))\n\226\134\146 ERR: user error : not a number\n\n(print \"Enter a num:\")\n(set 'num (int (read-line)))\n\n(int (bits 12345) 0 2) \226\134\146 12345\n\nThe inverse function to int with base 2 is bits.\n\nUse the float function to convert arguments to floating point numbers.")

(set '_integer? "{integer?\n\nsyntax: (integer? exp)\n\nReturns true only if the value of exp is an integer; otherwise, it returns nil.\n\n(set 'num 123)  \226\134\146 123\n(integer? num)  \226\134\146 true")

(set '_intersect "{intersect\n\nsyntax: (intersect list-A list-B)\nsyntax: (intersect list-A list-B bool)\n\nIn the first syntax, intersect returns a list containing one copy of each\nelement found both in list-A and list-B.\n\n(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))\n\226\134\146 (2 4 1)\n\nIn the second syntax, intersect returns a list of all elements in list-A that\nare also in list-B, without eliminating duplicates in list-A. bool is an\nexpression evaluating to true or any other value not nil.\n\n(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5) true)\n\226\134\146 (1 2 4 2 1)\n\nSee also the set functions difference, unique and union.")

(set '_invert "{invert\n\nsyntax: (invert matrix [float-pivot])\n\nReturns the inversion of a two-dimensional matrix in matrix. The matrix must be\nsquare, with the same number of rows and columns, and non-singular\n(invertible). Matrix inversion can be used to solve systems of linear equations\n(e.g., multiple regression in statistics). newLISP uses LU-decomposition of the\nmatrix to find the inverse.\n\nOptionally 0.0 or a very small value can be specified in float-pivot. This\nvalue substitutes pivot elements in the LU-decomposition algorithm, which\nresult in zero when the algorithm deals with a singular matrix.\n\nThe dimensions of a matrix are defined by the number of rows times the number\nof elements in the first row. For missing elements in non-rectangular matrices,\n0.0 (zero) is assumed. A matrix can either be a nested list or an array.\n\n(set 'A '((-1 1 1) (1 4 -5) (1 -2 0)))\n(invert A)  \226\134\146 ((10 2 9) (5 1 4) (6 1 5))\n(invert (invert A)) \226\134\146 ((-1 1 1) (1 4 -5) (1 -2 0))\n\n; solve Ax = b for x\n(multiply (invert A) '((1) (2) (3))) \226\134\146 ((41) (19) (23))\n\n; treatment of singular matrices\n(invert '((2 -1) (4 -2)))        \226\134\146 nil\n(invert '((2 -1) (4 -2)) 0.0)    \226\134\146 ((inf -inf) (inf -inf))\n(invert '((2 -1) (4 -2)) 1e-20)  \226\134\146 ((5e+19 -2.5e+19) (1e+20 -5e+19))\n\ninvert will return nil if the matrix is singular and cannot be inverted, and\nfloat-pivot is not specified.\n\nAll operations shown here on lists can be performed on arrays, as well.\n\nSee also the matrix functions det, mat, multiply and transpose.")

(set '_irr "{irr\n\nsyntax: (irr list-amounts [list-times [num-guess]])\n\nCalculates the internal rate of return of a cash flow per time period. The\ninternal rate of return is the interest rate that makes the present value of a\ncash flow equal to 0.0 (zero). In-flowing (negative values) and out-flowing\n(positive values) amounts are specified in list-amounts. If no time periods are\nspecified in list-times, amounts in list-amounts correspond to consecutive time\nperiods increasing by 1 (1, 2, 3\226\128\148). The algorithm used is iterative, with an\ninitial guess of 0.5 (50 percent). Optionally, a different initial guess can be\nspecified. The algorithm returns when a precision of 0.000001 (0.0001 percent)\nis reached. nil is returned if the algorithm cannot converge after 50\niterations.\n\nirr is often used to decide between different types of investments.\n\n(irr '(-1000 500 400 300 200 100))\n\226\134\146 0.2027\n\n(npv 0.2027 '(500 400 300 200 100))\n\226\134\146 1000.033848 ; ~ 1000\n\n(irr '(-1000 500 400 300 200 100) '(0 3 4 5 6 7))\n\226\134\146 0.0998\n\n(irr '(-5000 -2000 5000 6000) '(0 3 12 18))\n\226\134\146 0.0321\n\nIf an initial investment of 1,000 yields 500 after the first year, 400 after\ntwo years, and so on, finally reaching 0.0 (zero) after five years, then that\ncorresponds to a yearly return of about 20.2 percent. The next line\ndemonstrates the relation between irr and npv. Only 9.9 percent returns are\nnecessary when making the first withdrawal after three years.\n\nIn the last example, securities were initially purchased for 5,000, then for\nanother 2,000 three months later. After a year, securities for 5,000 are sold.\nSelling the remaining securities after 18 months renders 6,000. The internal\nrate of return is 3.2 percent per month, or about 57 percent in 18 months.\n\nSee also the fv, nper, npv, pmt, and pv functions.")

(set '_join "{join\n\nsyntax: (join list-of-strings [str-joint [bool-trail-joint]])\n\nConcatenates the given list of strings in list-of-strings. If str-joint is\npresent, it is inserted between each string in the join. If bool-trail-joint is\ntrue then a joint string is also appended to the last string.\n\n(set 'lst '(\"this\" \"is\" \"a\" \"sentence\"))\n\n(join lst \" \")  \226\134\146 \"this is a sentence\"\n\n(join (map string (slice (now) 0 3)) \"-\")  \226\134\146 \"2003-11-26\"\n\n(join (explode \"keep it together\"))  \226\134\146 \"keep it together\"\n\n(join '(\"A\" \"B\" \"C\") \"-\")         \226\134\146 \"A-B-C\"\n(join '(\"A\" \"B\" \"C\") \"-\" true)    \226\134\146 \"A-B-C-\"\n\nSee also the append, string, and explode functions, which are the inverse of\nthe join operation.")

(set '_lambda "{lambda\n\nSee the description of fn, which is a shorter form of writing lambda.")

(set '_lambda-macro "{lambda-macro\n\nSee the description of define-macro.")

(set '_lambda? "{lambda?\n\nsyntax: (lambda? exp)\n\nReturns true only if the value of exp is a lambda expression; otherwise,\nreturns nil.\n\n(define (square x) (* x x)) \226\134\146 (lambda (x) (* x x))\n\nsquare \226\134\146 (lambda (x) (* x x))\n\n(lambda? square)  \226\134\146 true\n\nSee define and define-macro for more information about lambda expressions.")

(set '_last "{last utf8\n\nsyntax: (last list)\nsyntax: (last array)\nsyntax: (last str)\n\nReturns the last element of a list or a string.\n\n(last '(1 2 3 4 5))  \226\134\146 5\n(last '(a b (c d)))  \226\134\146 (c d)\n\n(set 'A (array 3 2 (sequence 1 6)))\n\226\134\146 ((1 2) (3 4) (5 6))\n(last A)             \226\134\146 (5 6)\n\n(last '())           \226\134\146 ERR: list is empty\n\nIn the second version the last character in the string str is returned as a\nstring.\n\n(last \"newLISP\")  \226\134\146 \"P\"\n\nNote that last works on character boundaries rather than byte boundaries when\nthe UTF-8\226\128\147enabled version of newLISP is used. See also first, rest and nth.")

(set '_last-error "{last-error\n\nsyntax: (last-error)\nsyntax: (last-error int-error)\n\nReports the last error generated by newLISP due to syntax errors or exhaustion\nof some resource. For a summary of all possible errors see the chapter Error\ncodes in the appendix.\n\nIf no error has occurred since the newLISP session was started, nil is\nreturned.\n\nWhen int-error is specified, a list of the number and the error text is\nreturned.\n\n(last-error)  \226\134\146 nil\n\n(abc)\n\nERR: invalid function : (abc)\n\n(last-error) \226\134\146 (24 \"ERR: invalid function : (abc)\")\n\n(last-error 24) \226\134\146 (24 \"invalid function\")\n(last-error 1) \226\134\146 (1 \"not enough memory\")\n(last-error 12345) \226\134\146 (12345 \"Unknown error\")\n\nFor error numbers out of range the string \"Unknown error\" is given for the\nerror text.\n\nErrors can be trapped by error-event and userdefined error handlers.\n\nSee also net-error for errors generated by networking conditions and sys-error\nfor errors generated by the operating system.")

(set '_legal? "{legal?\n\nsyntax: (legal? str)\n\nThe token in str is verified as a legal newLISP symbol. Non-legal symbols can\nbe created using the sym function (e.g. symbols containing spaces, quotes, or\nother characters not normally allowed). Non-legal symbols are created\nfrequently when using them for associative data access:\n\n(symbol? (sym \"one two\"))  \226\134\146 true\n\n(legal? \"one two\")         \226\134\146 nil  ; contains a space\n\n(set (sym \"one two\") 123)  \226\134\146 123\n\n(eval (sym \"one two\"))     \226\134\146 123\n\nThe example shows that the string \"one two\" does not contain a legal symbol\nalthough a symbol can be created from this string and treated like a variable.")

(set '_length "{length\n\nsyntax: (length exp)\n\nReturns the number of elements in a list, the number of rows in an array, or\nthe number of bytes in a string.\n\nlength applied to a symbol returns the length of the symbol name. Applied to a\nnumber, length returns the number of bytes needed in memory to store that\nnumber: 4 or 8 for integers and 8 for floating point numbers.\n\n(length '(a b (c d) e))         \226\134\146 4\n(length '())                    \226\134\146 0\n(set 'someList '(q w e r t y))  \226\134\146 (q w e r t y)\n(length someList)               \226\134\146 6\n\n(set 'ary (array 2 4 '(0)))  \226\134\146 ((1 2 3 4) (5 6 7 8))\n(length ary)                 \226\134\146 2\n\n(length \"Hello World\")  \226\134\146 11\n(length \"\")             \226\134\146 0\n\n(length 'someVar)  \226\134\146 7\n(length 123)       \226\134\146 8\n(length 1.23)      \226\134\146 8\n\n(length (get-int \"\\000\\000\\000\\001\")) \226\134\146 4\n\nUse utf8len to calculate the number of UTF-8 characters in a string.")

(set '_let "{let\n\nsyntax: (let ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)\nsyntax: (let (sym1 exp-init1 [sym2 exp-init2 ... ]) body)\n\nOne or more variables sym1, sym2, ... are declared locally and initialized with\nexpressions in exp-init1, exp-init2, etc. In the fully parenthesized first\nsyntax, initializers are optional and assumed nil if missing.\n\nWhen the local variables are initialized, the initializer expressions evaluate\nusing symbol bindings as before the let statement. To incrementally use symbol\nbindings as evaluated during the initialization of locals in let, use letn.\n\nOne or more expressions in exp-body are evaluated using the local definitions\nof sym1, sym2 etc. let is useful for breaking up complex expressions by\ndefining local variables close to the place where they are used. The second\nform omits the parentheses around the variable expression pairs but functions\nidentically.\n\n(define (sum-sq a b)\n    (let ((x (* a a)) (y (* b b)))\n        (+ x y)))\n\n(sum-sq 3 4) \226\134\146 25\n\n(define (sum-sq a b)           ; alternative syntax\n    (let (x (* a a) y (* b b))\n        (+ x y)))\n\nThe variables x and y are initialized, then the expression (+ x y) is\nevaluated. The let form is just an optimized version and syntactic convenience\nfor writing:\n\n((lambda (sym1 [sym2 ... ]) exp-body ) exp-init1 [ exp-init2 ])\n\nSee also letn for an incremental or nested form of let and local for\ninitializing to nil.")

(set '_letex "{letex\n\nsyntax: (letex ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)\nsyntax: (letex (sym1 exp-init1 [sym2 exp-init2 ... ]) body)\n\nThis function combines let and expand to expand local variables into an\nexpression before evaluating it. In the fully parenthesized first syntax\ninitializers are optional and assumed nil if missing.\n\nBoth forms provide the same functionality, but in the second form the\nparentheses around the initializers can be omitted:\n\n(letex (x 1 y 2 z 3) '(x y z))    \226\134\146 (1 2 3)\n\n(letex ( (x 1) (y '(a b c)) (z \"hello\") ) '(x y z))\n\n\226\134\146 (1 (a b c) \"hello\")\n\nBefore the expression '(x y z) gets evaluated, x, y and z are literally\nreplaced with the initializers from the letex initializer list. The final\nexpression which gets evaluated is '(1 2 3).\n\nIn the second example a function make-adder is defined for making adder\nfunctions:\n\n(define (make-adder n)\n    (letex (c n) (lambda (x) (+ x c))))\n\n(define add3 (make-adder 3)) \226\134\146 (lambda (x) (+ x 3))\n\n(add3 10) \226\134\146 13\n\nletex evaluates n to the constant 3 and replaces c with it in the lambda\nexpression.")

(set '_letn "{letn\n\nsyntax: (letn ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)\nsyntax: (letn (sym1 exp-init1 [sym2 exp-init2 ... ]) body)\n\nletn is like a nested let and works similarly to let, but will incrementally\nuse the new symbol bindings when evaluating the initializer expressions as if\nseveral let were nested. In the fully parenthesized first syntax, initializers\nare optional and assumed nil if missing.\n\nThe following comparison of let and letn show the difference:\n\n(set 'x 10)\n(let ((x 1) (y (+ x 1)))\n(list x y))           \226\134\146 (1 11)\n\n(letn ((x 1) (y (+ x 1)))\n(list x y))          \226\134\146 (1 2)\n\nWhile in the first example using let the variable y is calculated using the\nbinding of x before the let expression, in the second example using letn the\nvariable y is calculated using the new local binding of x.\n\n(letn  (x 1 y x)\n(+ x y))             \226\134\146  2\n\n;; same as nested let's\n\n(let (x 1)\n(let (y x)\n(+ x y)))       \226\134\146  2\n\nletn works like several nested let. The parentheses around the initializer\nexpressions can be omitted.")

(set '_list "{list\n\nsyntax: (list exp-1 [exp-2 ... ])\n\nThe exp are evaluated and the values used to construct a new list. Note that\narguments of array type are converted to lists.\n\n(list 1 2 3 4 5)                \226\134\146 (1 2 3 4 5)\n(list 'a '(b c) (+ 3 4) '() '*) \226\134\146 (a (b c) 7 () *)\n\nSee also cons and push for other forms of building lists.")

(set '_list? "{list?\n\nsyntax: (list? exp)\n\nReturns true only if the value of exp is a list; otherwise returns nil. Note\nthat lambda and lambda-macro expressions are also recognized as special\ninstances of a list expression.\n\n(set 'var '(1 2 3 4))    \226\134\146 (1 2 3 4)\n(list? var)              \226\134\146 true\n\n(define (double x) (+ x x))\n\n(list? double)           \226\134\146 true")

(set '_load "{load\n\nsyntax: (load str-file-name-1 [str-file-name-2 ... ] [sym-context])\n\nLoads and translates newLISP from a source file specified in one or more\nstr-file-name and evaluates the expressions contained in the file(s). When\nloading is successful, load returns the result of the last expression in the\nlast file evaluated. If a file cannot be loaded, load throws an error.\n\nAn optional sym-context can be specified, which becomes the context of\nevaluation, unless such a context switch is already present in the file being\nloaded. By default, files which do not contain context switches will be loaded\ninto the MAIN context.\n\nThe str-file-name specs can contain URLs. Both http:// and file:// URLs are\nsupported.\n\n(load \"myfile.lsp\")\n\n(load \"a-file.lsp\" \"b-file.lsp\")\n\n(load \"file.lsp\" \"http://mysite.org/mypro\")\n\n(load \"http://192.168.0.21:6000//home/test/program.lsp\")\n\n(load \"a-file.lsp\" \"b-file.lsp\" 'MyCTX)\n\n(load \"file:///usr/share/newlisp/mysql.lsp\")\n\nIn case expressions evaluated during the load are changing the context, this\nwill not influence the programming module doing the load.\n\nThe current context after the load statement will always be the same as before\nthe load.\n\nNormal file specs and URLs can be mixed in the same load command.\n\nload with HTTP URLs can also be used to load code remotely from newLISP server\nnodes running on a Unix-like operating system. In this mode, load will issue an\nHTTP GET request to the target URL. Note that a double backslash is required\nwhen path names are specified relative to the root directory. load in HTTP mode\nwill observe a 60-second timeout.\n\nThe second to last line causes the files to be loaded into the context MyCTX.\nThe quote forces the context to be created if it did not exist.\n\nThe file:// URL is followed by a third / for the directory spec.")

(set '_log "{log\n\nsyntax: (log num)\nsyntax: (log num num-base)\n\nIn the first syntax, the expression in num is evaluated and the natural\nlogarithmic function is calculated from the result.\n\n(log 1)         \226\134\146 0\n(log (exp 1))   \226\134\146 1\n\nIn the second syntax, an arbitrary base can be specified in num-base.\n\n(log 1024 2)             \226\134\146 10\n(log (exp 1) (exp 1))    \226\134\146  1\n\nSee also exp, which is the inverse function to log with base e (2.718281828).")

(set '_lookup "{lookup\n\nsyntax: (lookup exp-key list-assoc [int-index [exp-default]])\n\nFinds in list-assoc an association, the key element of which has the same value\nas exp-key, and returns the int-index element of association (or the last\nelement if int-index is absent).\n\nOptionally, exp-default can be specified, which is returned if an association\nmatching exp-key cannot be found. If the exp-default is absent and no\nassociation has been found, nil is returned.\n\nSee also Indexing elements of strings and lists.\n\nlookup is similar to assoc but goes one step further by extracting a specific\nelement found in the list.\n\n(set 'params '(\n    (name \"John Doe\")\n    (age 35)\n    (gender \"M\")\n    (balance 12.34)\n))\n\n(lookup 'age params)             \226\134\146 35\n\n; use together with setf to modify and association list\n(setf (lookup 'age params) 42)   \226\134\146 42\n(lookup 'age params)             \226\134\146 42\n\n(set 'persons '(\n    (\"John Doe\" 35 \"M\" 12.34)\n    (\"Mickey Mouse\" 65 \"N\" 12345678)\n))\n\n(lookup \"Mickey Mouse\" persons 2)    \226\134\146 \"N\"\n(lookup \"Mickey Mouse\" persons -3)   \226\134\146 65\n(lookup \"John Doe\" persons 1)        \226\134\146 35\n(lookup \"John Doe\" persons -2)       \226\134\146 \"M\"\n\n(lookup \"Jane Doe\" persons 1 \"N/A\")  \226\134\146 \"N/A\"\n\nSee also assoc")

(set '_lower-case "{lower-case utf8\n\nsyntax: (lower-case str)\n\nConverts the characters of the string in str to lowercase. A new string is\ncreated, and the original is left unaltered.\n\n(lower-case \"HELLO WORLD\")  \226\134\146 \"hello world\"\n(set 'Str \"ABC\")\n(lower-case Str)  \226\134\146 \"abc\"\nStr               \226\134\146 \"ABC\"\n\nSee also the upper-case and title-case functions.")

(set '_macro? "{macro?\n\nsyntax: (macro? exp)\n\nreturns true if exp evaluates to a lambda-macro expression; otherwise, nil is\nreturned.\n\n(define-macro (mysetq lv rv) (set lv (eval rv)))\n\n(macro? mysetq)  \226\134\146 true")

(set '_main-args "{main-args\n\nsyntax: (main-args)\nsyntax: (main-args int-index)\n\nmain-args returns a list with several string members, one for program\ninvocation and one for each of the command-line arguments.\n\nnewlisp 1 2 3\n\n> (main-args)\n(\"/usr/bin/newlisp\" \"1\" \"2\" \"3\")\n\nAfter newlisp 1 2 3 is executed at the command prompt, main-args returns a list\ncontaining the name of the invoking program and three command-line arguments.\n\nOptionally, main-args can take an int-index for indexing into the list. Note\nthat an index out of range will cause nil to be returned, not the last element\nof the list like in list-indexing.\n\nnewlisp a b c\n\n> (main-args 0)\n\"/usr/bin/newlisp\"\n> (main-args -1)\n\"c\"\n> (main-args 2)\n\"b\"\n> (main-args 10)\nnil\n\nNote that when newLISP is executed from a script, main-args also returns the\nname of the script as the second argument:\n\n#!/usr/bin/newlisp\n#\n# script to show the effect of 'main-args' in script file\n\n(print (main-args) \"\\n\")\n(exit)\n\n# end of script file\n\n;; execute script in the OS shell:\n\nscript 1 2 3\n\n(\"/usr/bin/newlisp\" \"./script\" \"1\" \"2\" \"3\")\n\nTry executing this script with different command-line parameters.")

(set '_make-dir "{make-dir\n\nsyntax: (make-dir str-dir-name [int-mode])\n\nCreates a directory as specified in str-dir-name, with the optional access mode\nint-mode. Returns true or nil depending on the outcome. If no access mode is\nspecified, most Unix systems default to drwxr-xr-x.\n\nOn Unix systems, the access mode specified will also be masked by the OS's\nuser-mask set by the system administrator. The user-mask can be retrieved on\nUnix systems using the command umask and is usually 0022 (octal), which masks\nwrite (and creation) permission for non-owners of the file.\n\n;; 0 (zero) in front of 750 makes it an octal number\n\n(make-dir \"adir\" 0750)\n\nThis example creates a directory named adir in the current directory with an\naccess mode of 0750 (octal 750 = drwxr-x---).")

(set '_map "{map\n\nsyntax: (map exp-functor list-args-1 [list-args-2 ... ])\n\nSuccessively applies the primitive function, defined function, or lambda\nexpression exp-functor to the arguments specified in list-args-1, list-args-2\226\128\148,\nreturning all results in a list.\n\n(map + '(1 2 3) '(50 60 70))  \226\134\146 (51 62 73)\n\n(map if '(true nil true nil true) '(1 2 3 4 5) '(6 7 8 9 10))\n\226\134\146 '(1 7 3 9 5)\n\n(map (fn (x y) (* x y)) '(3 4) '(20 10))\n\226\134\146 (60 40)\n\nThe second example shows how to dynamically create a function for map:\n\n(define (foo op p)\n    (append (lambda (x)) (list (list op p 'x))))\n\nWe can also use the shorter fn:\n\n(define (foo op p)\n    (append (fn (x)) (list (list op p 'x))))\n\nfoo now works like a function-maker:\n\n(foo 'add 2)  \226\134\146 (lambda (x) (add 2 x))\n\n(map (foo add 2) '(1 2 3 4 5))  \226\134\146 (3 4 5 6 7)\n\n(map (foo mul 3) '(1 2 3 4 5))  \226\134\146 (3 6 9 12 15)\n\nNote that the quote before the operand can be omitted because primitives\nevaluate to themselves in newLISP.\n\nBy incorporating map into the function definition, we can do the following:\n\n(define (list-map op p lst)\n    (map (lambda (x) (op p x)) lst))\n\n(list-map + 2 '(1 2 3 4))  \226\134\146 (3 4 5 6)\n\n(list-map mul 1.5 '(1 2 3 4))  \226\134\146 (1.5 3 4.5 6)\n\nmap also sets the internal list index $idx.\n\n(map (fn (x) (list $idx x)) '(a b c)) \226\134\146 ((0 a) (1 b) (2 c))\n\nThe number of arguments used is determined by the length of the first argument\nlist. Arguments missing in other argument lists cause map to stop collecting\nparameters for that level of arguments. This ensures that the nth parameter\nlist gets converted to the nth column during the transposition occurring. If an\nargument list contains too many elements, the extra ones will be ignored.\n\nSpecial forms which use parentheses as syntax cannot be mapped (i.e. case).")

(set '_mat "{mat\n\nsyntax: (mat + | - | * | / matrix-A matrix-B)\nsyntax: (mat + | - | * | / matrix-A number)\n\nUsing the first syntax, this function performs fast floating point scalar\noperations on two-dimensional matrices in matrix-A or matrix-B. The type of\noperation is specified by one of the four arithmetic operators +, -, *, or /.\nThis type of arithmetic operator is typically used for integer operations in\nnewLISP. In the case of mat, however, all operations will be performed as\nfloating point operations (add, sub, mul, div).\n\nMatrices in newLISP are two-dimensional lists or arrays. Internally, newLISP\ntranslates lists and arrays into fast, accessible C-language data objects. This\nmakes matrix operations in newLISP as fast as those coded directly in C. The\nsame is true for the matrix operations multiply and invert.\n\n(set 'A '((1 2 3) (4 5 6)))\n(set 'B A)\n\n(mat + A B)    \226\134\146 ((2 4 6) (8 10 12))\n(mat - A B)    \226\134\146 ((0 0 0) (0 0 0))\n(mat * A B)    \226\134\146 ((1 4 9) (16 25 36))\n(mat / A B)    \226\134\146 ((1 1 1) (1 1 1))\n\n; specify the operator in a variable\n\n(set 'op +)\n(mat op A B)    \226\134\146 ((2 4 6) (8 10 12))\n\nUsing the second syntax, all cells in matrix-A are operated on with a scalar in\nnumber:\n\n(mat + A 5)    \226\134\146 ((6 7 8) (9 10 11))\n(mat - A 2)    \226\134\146 ((-1 0 1) (2 3 4))\n(mat * A 3)    \226\134\146 ((3 6 9) (12 15 18))\n(mat / A 10)   \226\134\146 ((.1 .2 .3) (.4 .5 .6))\n\nSee also the other matrix operations det, invert, multiply, and transpose.")

(set '_match [text]{match

syntax: (match list-pattern list-match [bool])

The pattern in list-pattern is matched against the list in list-match, and the
matching expressions are returned in a list. The three wildcard characters ?,
+, and * can be used in list-pattern.

Wildcard characters may be nested. match returns a list of matched expressions.
For each ? (question mark), a matching expression element is returned. For each
+ (plus sign) or * (asterisk), a list containing the matched elements is
returned. If the pattern cannot be matched against the list in list-match,
match returns nil. If no wildcard characters are present in the pattern an
empty list is returned.

Optionally, the Boolean value true (or any other expression not evaluating to
nil) can be supplied as a third argument. This causes match to show all
elements in the returned result.

match is frequently employed as a functor parameter in find, ref, ref-all and
replace and is internally used by find-all for lists.

(match '(a ? c) '(a b c))  → (b)

(match '(a ? ?) '(a b c))  → (b c)

(match '(a ? c) '(a (x y z) c))  → ((x y z))

(match '(a ? c) '(a (x y z) c) true)  → (a (x y z) c)

(match '(a ? c) '(a x y z c))  → nil


(match '(a * c) '(a x y z c))  → ((x y z))

(match '(a (b c ?) x y z) '(a (b c d) x y z))  → (d)

(match '(a (*) x ? z) '(a (b c d) x y z))  → ((b c d) y)


(match '(+) '())  → nil

(match '(+) '(a))  → ((a))

(match '(+) '(a b))  → ((a b))

(match '(a (*) x ? z) '(a () x y z))  → (() y)

(match '(a (+) x ? z) '(a () x y z))  → nil

Note that the * operator tries to grab the fewest number of elements possible,
but match backtracks and grabs more elements if a match cannot be found.

The + operator works similarly to the * operator, but it requires at least one
list element.

The following example shows how the matched expressions can be bound to
variables.

(map set '(x y) (match '(a (? c) d *) '(a (b c) d e f)))

x  → b
y  → (e f)

Note that match for strings has been eliminated. For more powerful string
matching, use regex, find, find-all or parse.

unify is another function for matching expressions in a PROLOG like manner.[/text])

(set '_max "{max\n\nsyntax: (max num-1 [num-2 ... ])\n\nEvaluates the expressions num-1\226\128\148 and returns the largest number.\n\n(max 4 6 2 3.54 7.1)  \226\134\146 7.1\n\nSee also the min function.")

(set '_member "{member\n\nsyntax: (member exp list)\nsyntax: (member str-key str [num-option])\n\nIn the first syntax, member searches for the element exp in the list list. If\nthe element is a member of the list, a new list starting with the element found\nand the rest of the original list is constructed and returned. If nothing is\nfound, nil is returned. When specifying num-option, member performs a regular\nexpression search.\n\n(set 'aList '(a b c d e f g h))  \226\134\146 (a b c d e f g h)\n(member 'd aList)                \226\134\146 (d e f g h)\n(member 55 aList)                \226\134\146 nil\n\nIn the second syntax, member searches for str-key in str. If str-key is found,\nall of str (starting with str-key) is returned. nil is returned if nothing is\nfound.\n\n(member \"LISP\" \"newLISP\")  \226\134\146 \"LISP\"\n(member \"LI\" \"newLISP\")    \226\134\146 \"LISP\"\n(member \"\" \"newLISP\")      \226\134\146 \"newLISP\"\n(member \"xyz\" \"newLISP\")   \226\134\146 nil\n(member \"li\" \"newLISP\" 1)  \226\134\146 \"LISP\"\n\nSee also the related functions slice and find.")

(set '_min "{min\n\nsyntax: (min num-1 [num-2 ... ])\n\nEvaluates the expressions num-1\226\128\148 and returns the smallest number.\n\n(min 4 6 2 3.54 7.1)  \226\134\146 2\n\nSee also the max function.")

(set '_mod "{mod\n\nsyntax: (mod num-1 num-2 [num-3 ... ])\nsyntax: (mod num-1)\n\nCalculates the modular value of the numbers in num-1 and num-2. mod computes\nthe remainder from the division of the numerator num-i by the denominator num-i\n+ 1. Specifically, the return value is numerator - n * denominator, where n is\nthe quotient of the numerator divided by the denominator, rounded towards zero\nto an integer. The result has the same sign as the numerator and its magnitude\nis less than the magnitude of the denominator.\n\nIn the second syntax 1 is assumed for num-2 and the result is the fractional\npart of num-1.\n\n(mod 10.5 3.3)   \226\134\146  0.6\n(mod -10.5 3.3)  \226\134\146 -0.6\n(mod -10.5)      \226\134\146 -0.5\n\nUse the % (percent sign) function when working with integers only.")

(set '_mul "{mul\n\nsyntax: (mul num-1 num-2 [num-3 ... ])\n\nEvaluates all expressions num-1\226\128\148, calculating and returning the product. mul\ncan perform mixed-type arithmetic, but it always returns floating point\nnumbers. Any floating point calculation with NaN also returns NaN.\n\n(mul 1 2 3 4 5 1.1)  \226\134\146 132\n(mul 0.5 0.5)        \226\134\146 0.25")

(set '_multiply "{multiply\n\nsyntax: (multiply matrix-A matrix-B)\n\nReturns the matrix multiplication of matrices in matrix-A and matrix-B. If\nmatrix-A has the dimensions n by m and matrix-B the dimensions k by l (m and k\nmust be equal), the result is an n by l matrix. multiply can perform mixed-type\narithmetic, but the results are always double precision floating points, even\nif all input values are integers.\n\nThe dimensions of a matrix are determined by the number of rows and the number\nof elements in the first row. For missing elements in non-rectangular matrices,\n0.0 is assumed. A matrix can either be a nested list or array.\n\n(set 'A '((1 2 3) (4 5 6)))\n(set 'B '((1 2) (1 2) (1 2)))\n(multiply A B)  \226\134\146 ((6 12) (15 30))\n\n(set 'v '(10 20 30))\n(multiply A (transpose (list v))) \226\134\146 ((140) (320))\n\nWhen multiplying a matrix with a vector of n elements, the vector must be\ntransformed into n rows by 1 column matrix using transpose.\n\nAll operations shown here on lists can be performed on arrays, as well.\n\nSee also the matrix operations det, invert, mat and transpose.")

(set '_name "{name\n\nThis function is deprecated, use term instead.")

(set '_net-accept "{net-accept\n\nsyntax: (net-accept int-socket)\n\nAccepts a connection on a socket previously put into listening mode. Returns a\nnewly created socket handle for receiving and sending data on this connection.\n\n(set 'socket (net-listen 1234))\n(net-accept socket)\n\nNote that for ports less than 1024, newLISP must be started in superuser mode\non Unix-like operating systems.\n\nSee also the files server and client examples in the examples/ directory of the\nsource distribution.")

(set '_net-close "{net-close\n\nsyntax: (net-close int-socket [true])\n\nCloses a network socket in int-socket that was previously created by a\nnet-connect or net-accept function. Returns true on success and nil on failure.\n\n(net-close aSock)\n\nThe optional true flag suppresses immediate shutdown of sockets by waiting for\npending data transmissions to finish.")

(set '_net-connect [text]{net-connect

syntax: (net-connect str-remote-host int-port [int-timeout-ms])
syntax: (net-connect str-remote-host int-port [str-mode [int-ttl]])
syntax: (net-connect str-file-path)

In the first syntax, connects to a remote host computer specified in
str-remote-host and a port specified in int-port. Returns a socket handle after
having connected successfully; otherwise, returns nil.

(set 'socket (net-connect "example.com" 80))
(net-send socket "GET /\r\n\r\n")
(net-receive socket buffer 10000)
(println buffer)
(exit)

If successful, the net-connect function returns a socket number which can be
used to send and receive information from the host. In the example a HTTP GET
request is sent and subsequently a web page received. Note that newLISP has
already a buit-in function get-url offering the same functionality.

Optionally a timeout value int-timeout in milli seconds can be specified.
Without a timeout value the function will wait up to 10 seconds for an open
port. With a timeout value the function can be made to return on an unavailable
port much earlier or later. The following example shows a port scanner looking
for open ports:

(set 'host (main-args 2))
(println "Scanning: " host)
(for (port 1 1024)
    (if (set 'socket (net-connect host port 500))
        (println "open port: " port " " (or (net-service port "tcp") ""))
        (print port "\r"))
)

The programs takes the host string from the shell command line as either a
domain name or an IP number in dot notation then tries to open each port from 1
to 1024. For each openport the port number and the service description string
is printed. If no description is available, an empty string "" is output. For
closed ports the function outputs numbers in the shell window staying on the
same line.

On Unix net-connnect may return with nil before the timeout expires, when the
port is not available. On Win32 net-connect will always wait for the timout to
expire before failing with nil.

UDP communications

In the second syntax, a third parameter, the string "udp" or "u" can be
specified in the optional str-mode to create a socket suited for UDP (User
Datagram Protocol) communications. In UDP mode, net-connect does not try to
connect to the remote host, but creates the socket and binds it to the remote
address, if an address is specified. A subsequent net-send will send a UDP
packet containing that target address. When using net-send-to, only one of the
two functions net-connect or net-send-to should provide a target address. The
other function should specify and empty string "" as the target address.

;; example server
(net-listen 4096 "226.0.0.1" "udp")  → 5
(net-receive-from 5 20)

;; example client I
(net-connect "226.0.0.1" 4096 "udp") → 3
(net-send 3 "hello")

;; example client II
(net-connect "" 4096 "udp") → 3
(net-send-to "226.0.0.1" 4096 "hello" 3)

The functions net-receive and net-receive-from can both be used and will
perform UDP communications when the "udp" option as been used in net-listen or
net-connect. net-select and net-peek can be used to check for received data in
a non-blocking fashion.

net-listen binds a specific local address and port to the socket. When
net-connect is used, the local address and port will be picked by the
socket-stack functions of the host OS.

UDP multicast communications

When specifying "multi" or "m" as a third parameter for str-mode, a socket for
UDP multicast communications will be created. Optionally, the fourth parameter
int-ttl can be specified as a TTL (time to live) value. If no int-ttl value is
specified, a value of 3 is assumed.

Note that specifying UDP multicast mode in net-connect does not actually
establish a connection to the target multicast address but only puts the socket
into UDP multicasting mode. On the receiving side, use net-listen together with
the UDP multicast option.

;; example client I
(net-connect "" 4096 "multi")  → 3
(net-send-to "226.0.0.1" 4096 "hello" 3)

;; example client II
(net-connect "226.0.0.1" 4096 "multi")  → 3
(net-send 3 "hello")

;; example server
(net-listen 4096 "226.0.0.1" "multi")  → 5
(net-receive-from 5 20)
→ ("hello" "192.168.1.94" 32769)

On the server side, net-peek or net-select can be used for non-blocking
communications. In the above example, the server would block until a datagram
is received.

The address 226.0.0.1 is just one multicast address in the Class D range of
multicast addresses from 224.0.0.0 to 239.255.255.255.

The net-send and net-receive functions can also be used instead of net-send-to
and net-receive-from.

UDP broadcast communications

Specifying the string "broadcast" or "b" in the third parameter, str-mode,
causes UDP broadcast communications to be set up. In this case, the broadcast
address ending in 255 is used.

;; example client
(net-connect "192.168.2.255" 3000 "broadcast")  → 3
(net-send 3 "hello")

;; example server
(net-listen 3000 "" "udp")  → 5

(net-receive 5 buff 10)
buff  → "hello"
;; or
(net-receive-from 5 10)
→ ("hello" "192.168.2.1" 46620)

Note that on the receiving side, net-listen should be used with the default
address specified with an "" (empty string). Broadcasts will not be received
when specifying an address. As with all UDP communications, net-listen does not
actually put the receiving side in listen mode, but rather sets up the sockets
for the specific UDP mode.

The net-select or net-peek functions can be used to check for incoming
communications in a non-blocking fashion.

Local domain Unix sockets

In the third syntax, net-connect connects to a server on the local file system
via a local domain Unix socket named using str-file-path. Returns a socket
handle after having connected successfully; otherwise, returns nil.

(net-connect "/tmp/mysocket")  → 3

; on OS/2 use "\\socket\\" prefix

(net-connect "\\socket\\mysocket")

A local domain file system socket is created and returned. On the server side,
local domain sockets have been created using net-listen and net-accept. After
the connection has been established the functions net-select, net-send and
net-receive can be used as usual for TCP/IP stream communications. This type of
connection can be used as a fast bi-directional communications channel between
processes on the same file system. This type of connection is not available on
Win32 platforms.[/text])

(set '_net-error "{net-error\n\nsyntax: (net-error)\nsyntax: (net-error int-error)\n\nRetrieves the last error that occurred when calling a any of the following\nfunctions: net-accept, net-connect, net-eval, net-listen, net-lookup,\nnet-receive, net-receive-udp, net-select, net-send, net-send-udp, and\nnet-service. Whenever one of these functions fails, it returns nil and\nnet-error can be used to retrieve more information.\n\nFunctions that communicate using sockets close the socket automatically and\nremove it from the net-sessions list.\n\nEach successful termination of a net-* function clears the error number.\n\nThe following messages are returned:\n\nno description\n1  Cannot open socket\n2  DNS resolution failed\n3  Not a valid service\n4  Connection failed\n5  Accept failed\n6  Connection closed\n7  Connection broken\n8  Socket send() failed\n9  Socket recv() failed\n10 Cannot bind socket\n11 Too many sockets in net-select\n12 Listen failed\n13 Badly formed IP\n14 Select failed\n15 Peek failed\n16 Not a valid socket\n17 Cannot unblock socket\n18 Operation timed out\n19 HTTP bad formed URL\n20 HTTP file operation failed\n21 HTTP transfer failed\n22 HTTP invalid response from server\n23 HTTP no response from server\n24 HTTP document empty\n25 HTTP error in header\n26 HTTP error in chunked format\n\n(net-error) \226\134\146 nil\n\n(net-connect \"jhghjgkjhg\" 80)  \226\134\146  nil\n\n(net-error)  \226\134\146  (2 \"ERR: \"DNS resolution failed\")\n\nWhen int-error is specified the number and error text for that error number is\nreturned.\n\n(net-error 10) \226\134\146 (10 \"Cannot bind socket\")\n\nSee also last-error and sys-error.")

(set '_net-eval [text]{net-eval

syntax: (net-eval str-host int-port exp [int-timeout [func-handler]])
syntax: (net-eval '((str-host int-port exp) ... ) [int-timeout [func-handler]])

Can be used to evaluate source remotely on one or more newLISP servers. This
function handles all communications necessary to connect to the remote servers,
send source for evaluation, and wait and collect responses.

The expression in exp evaluates to either a string or an expression which will
be evaluated remotely in the environment of the target node.

The remote TCP/IP servers are started in the following way:

newlisp -c -d 4711 &

; or with logging connections

newlisp -l -c -d 4711 &

; communicate via Uix local domain socket

newlisp -c /tmp/mysocket

Instead of 4711, any other port number can be used. Multiple nodes can be
started on different hosts and with the same or different port numbers. The -l
or -L logging options can be specified to log connections and remote commands.

The -d daemon mode allows newLISP to maintain state between connections. When
keeping state between connections is not desired, the inetd daemon mode offers
more advantages. The Internet inetd or xinetd services daemon will start a new
newLISP process for each client connection. This makes for much faster
servicing of multiple connections. In -d daemon mode, each new client request
would have to wait for the previous request to be finished. See the chapter
inetd daemon mode on how to configure this mode correctly.

In the first syntax, net-eval talks to only one remote newLISP server node,
sending the host in str-host on port int-port a request to evaluate the
expression exp. If int-timeout is not given, net-eval will wait up to 60
seconds for a response after a connection is made. Otherwise, if the timeout in
milliseconds has expired, nil is returned; else, the evaluation result of exp
is returned.

; the code to be evaluated is given in a quoted expression
(net-eval "192.168.1.94" 4711 '(+ 3 4))       → 7

; expression as a string
(net-eval "192.168.1.94" 4711 "(+ 3 4)")      → 7

; with timeout
(net-eval "192.168.1.94" 4711 '(+ 3 4) 1)     → nil  ; timeout to short
(net-error)                                   → (17 "ERR: Operation timed out")

(net-eval "192.168.1.94" 4711 '(+ 3 4) 1000)  → 7

; program contained in a variable
(set 'prog '(+ 3 4))
(net-eval "192.168.1.94" 4711 prog)           → 7

; specify a local-domain Unix socket (not available on Win32)
(net-eval "/tmp/mysocket" 0 '(+ 3 4))         → 7

The second syntax of net-eval returns a list of the results after all of the
responses are collected or timeout occurs. Responses that time out return nil.
The last example line shows how to specify a local-domain Unix socket
specifying the socket path and a port number of 0. Connection errors or errors
that occur when sending information to nodes are returned as a list of error
numbers and descriptive error strings. See the function net-error for a list of
potential error messages.

; two different remote nodes different IPs
(net-eval '(
    ("192.168.1.94" 4711 '(+ 3 4))
    ("192.168.1.95" 4711 '(+ 5 6))
    ) 5000)
→ (7 11)

; two persistent nodes on the same CPU different ports
(net-eval '(
    ("localhost" 8081 '(foo "abc"))
    ("localhost" 8082 '(myfunc 123)')
    ) 3000)

; inetd or xinetd nodes on the same server and port
; nodes are loaded on demand
(net-eval '(
    ("localhost" 2000 '(foo "abc"))
    ("localhost" 2000 '(myfunc 123))
    ) 3000)

The first example shows two expressions evaluated on two different remote
nodes. In the second example, both nodes run on the local computer. This may be
useful when debugging or taking advantage of multiple CPUs on the same
computer. When specifying 0 for the port number , net-eval takes the host name
as the file path to the local-domain Unix socket.

Note that definitions of foo and myfunc must both exist in the target
environment. This can be done using a net-eval sending define statements
before. It also can be done by preloading code when starting remode nodes.

When nodes are inetd or xinetd-controlled, several nodes may have the same IP
address and port number. In this case, the Unix daemon inetd or xinetd will
start multiple newLISP servers on demand. This is useful when testing
distributed programs on just one machine. The last example illustrates this
case. It is also useful on multi core CPUs, where the platform OS can
distribute different processes on to different CPU cores.

The source sent for evaluation can consist of entire multiline programs. This
way, remote nodes can be loaded with programs first, then specific functions
can be called. For large program files, the functions put-url or save (with a
URL file name) can be used to transfer programs. The a net-eval statement could
load these programs.

Optionally, a handler function can be specified. This function will be
repeatedly called while waiting and once for every remote evaluation
completion.

(define (myhandler param)
    (if param
        (println param))
)

(set 'Nodes '(
    ("192.168.1.94" 4711)
    ("192.168.1.95" 4711)
))

(set 'Progs '(
    (+ 3 4)
    (+ 5 6)
))

(net-eval (map (fn (n p) (list (n 0) (n 1) p)) Nodes Progs) 5000 myhandler)
→
("192.168.1.94" 4711 7)
("192.168.1.95" 4711 11)

The example shows how the list of node specs can be assembled from a list of
nodes and sources to evaluate. This may be useful when connecting to a larger
number of remote nodes.

(net-eval (list
  (list (Nodes 0 0) (Nodes 0 1) (Progs 0))
  (list (Nodes 1 0) (Nodes 1 1) (Progs 1))
 ) 3000 myhandler)

While waiting for input from remote hosts, myhandler will be called with nil as
the argument to param. When a remote node result is completely received,
myhandler will be called with param set to a list containing the remote host
name or IP number, the port, and the resulting expression. net-eval will return
true before a timeout or nil if the timeout was reached or exceeded. All remote
hosts that exceeded the timeout limit will contain a nil in their results list.

For a longer example see this program: mapreduce. The example shows how a word
counting task gets distributed to three remote nodes. The three nodes count
words in different texts and the master node receives and consolidates the
results.[/text])

(set '_net-interface "{net-interface\n\nsyntax: (net-interface str-ip-addr)\nsyntax: (net-interface)\n\nSets the default local interface address to be used for network connections. If\nnot set then network functions will default to an internal default address,\nexcept when overwritten by an optional interface address given in net-listen.\n\nWhen no str-ip-addr is specified, the current default is returned. If the\nnet-interface has not been used yet to specify an IP address, the address\n0.0.0.0 is returned. This means that all network routines will use the default\naddress preconfigured by the underlying operating system.\n\nThis function has only usage on multihomed servers with either multiple network\ninterface hardware or otherwise supplied multiple IP numbers. On all other\nmachines network functions will automatically select the single network\ninterface installed.\n\nOn error the function returns nil and net-error can be used to report the\nerror.\n\n(net-interface \"192.168.1.95\")  \226\134\146 \"192.168.1.95\"\n(net-interface \"localhost\")     \226\134\146 \"127.0.0.1\"\n\nAn interface address can be defined as either an IP address or a name. The\nreturn value is the address given in str-ip-addr")

(set '_net-ipv "{net-ipv\n\nsyntax: (net-ipv int-version)\nsyntax: (net-ipv)\n\nSwitches between IPv4 and IPv6 internet protocol versions. int-version contains\neither a 4 for IPv4 or a 6 for IPv6. When no parameter is given, net-ipv\nreturns the current setting.\n\n(net-ipv)      \226\134\146 4\n(net-ipv 6)    \226\134\146 6\n\nBy default newLISP starts up in IPv4 mode. The IPv6 protocol mode can also be\nspecified from the commandline when starting newlisp:\n\nnewlisp -6\n\nOnce a socket is connected with either net-connect or listened on with\nnet-listen, the net-accept, net-select, net-send, net-receive and\nnet-receive-from functions automatically adjust to the address protocol used\nwhen creating the sockets. Different connections with different IPv4/6 settings\ncan be open at the same time.\n\nNote, that currently net-packet does not support IPv6 and will work in IPv4\nmode regardless of settings.")

(set '_net-listen [text]{net-listen

syntax: (net-listen int-port [str-ip-addr [str-mode]])
syntax: (net-listen str-file-path)

Listens on a port specified in int-port. A call to net-listen returns
immediately with a socket number, which is then used by the blocking net-accept
function to wait for a connection. As soon as a connection is accepted,
net-accept returns a socket number that can be used to communicate with the
connecting client.

(set 'port 1234)
(set 'listen (net-listen port))
(unless listen (begin
    (print "listening failed\n")
    (exit)))

(print "Waiting for connection on: " port "\n")

(set 'connection (net-accept listen))
(if connection
    (while (net-receive connection buff 1024 "\n")
        (print buff)
        (if (= buff "\r\n") (exit)))
    (print "Could not connect\n"))

The example waits for a connection on port 1234, then reads incoming lines
until an empty line is received. Note that listening on ports lower than 1024
may require superuser access on Unix systems.

On computers with more than one interface card, specifying an optional
interface IP address or name in str-ip-addr directs net-listen to listen on the
specified address.

;; listen on a specific address
(net-listen port "192.168.1.54")

Local domain Unix sockets

In the second syntax, net-listen listens for a client on the local file system
via a local domain Unix socket named using str-file-path. If successful,
returns a socket handle that can be used with net-accept to accept a client
connection; otherwise, returns nil.

(net-listen "/tmp/mysocket")  → 5

; on OS/2 use "\\socket\\" prefix

(net-listen "\\socket\\mysocket")

(net-accept 5)

A local domain file system socket is created and listened on. A client will try
to connect using the same str-file-path. After a connection has been accepted
the functions net-select, net-send and net-receive can be used as usual for TCP
/IP stream communications. This type of connection can be used as a fast
bi-directional communications channel between processes on the same file
system. This type of connection is not available on Win32 platforms.

UDP communications

As a third parameter, the optional string "udp" or "u" can be specified in
str-mode to create a socket suited for UDP (User Datagram Protocol)
communications. A socket created in this way can be used directly with
net-receive-from to await incoming UDP data without using net-accept, which is
only used in TCP communications. The net-receive-from call will block until a
UDP data packet is received. Alternatively, net-select or net-peek can be used
to check for ready data in a non-blocking fashion. To send data back to the
address and port received with net-receive-from, use net-send-to.

Note that net-peer will not work, as UDP communications do not maintain a
connected socket with address information.

(net-listen 10002 "192.168.1.120" "udp")

(net-listen 10002 "" "udp")

The first example listens on a specific network adapter, while the second
example listens on the default adapter. Both calls return a socket number that
can be used in subsequent net-receive, net-receive-from, net-send-to,
net-select, or net-peek function calls.

Both a UDP server and UDP client can be set up using net-listen with the "udp"
option. In this mode, net-listen does not really listen as in TCP/IP
communications; it just binds the socket to the local interface address and
port.

For a working example, see the files examples/client and examples/server in the
newLISP source distribution.

Instead of net-listen and the "udp" option, the functions net-receive-udp and
net-send-udp can be used for short transactions consisting only of one data
packet.

net-listen, net-select, and net-peek can be used to facilitate non-blocking
reading. The listening/reading socket is not closed but is used again for
subsequent reads. In contrast, when the net-receive-udp and net-send-udp pair
is used, both sides close the sockets after sending and receiving.

UDP multicast communications

If the optional string str-mode is specified as "multi" or "m", net-listen
returns a socket suitable for multicasting. In this case, str-ip-addr contains
one of the multicast addresses in the range 224.0.0.0 to 239.255.255.255.
net-listen will register str-ip-addr as an address on which to receive
multicast transmissions. This address should not be confused with the IP
address of the server host.

;; example client

(net-connect "226.0.0.1" 4096 "multi")  → 3

(net-send-to "226.0.0.1" 4096 "hello" 3)


;; example server

(net-listen 4096 "226.0.0.1" "multi")  → 5

(net-receive-from 5 20)
→ ("hello" "192.168.1.94" 32769)

On the server side, net-peek or net-select can be used for non-blocking
communications. In the example above, the server would block until a datagram
is received.

The net-send and net-receive functions can be used instead of net-send-to and
net-receive-from.

Packet divert sockets and ports

If str-mode is specified as "divert", a divert socket can be created for a
divert port in int-port on BSD like platforms. The content of IP address in
str-ip-addr is ignored and can be specifed as an empty string. Only the
int-port is relevant and will be bound to the raw socket returned.

To use the divert option in net-listen, newLISP must run in super-user mode.
This option is only available on Unix like platforms.

The divert socket will receive all raw packets diverted to the divert port.
Packets may also be written back to a divert socket, in which case they
re-enter OS kernel IP packet processing.

Rules for packet diversion to the divert port must be defined using either the
ipfw BSD or ipchains Linux configuration utilities.

The net-receive-from and net-send-to functions are used to read and write raw
packets on the divert socket created and returned by the net-listen statement.
The same address received by net-receive-from is used in the net-send-to call
when re-injecting the packet:

; rules have been previously configured for a divert port
(set 'divertSocket (net-listen divertPort "" "divert"))

(until (net-error)
    (set 'rlist (net-receive-from divertSocket maxBytes))
    (set 'buffer (rlist 1))
    ; buffer can be processed here before reinjecting
    (net-send-to (rlist 0) divertPort buffer divertSocket)
)

For more information see the Unix man pages for divert and the ipfw (BSDs) or
ipchains (Linux) configuration utilities.[/text])

(set '_net-local "{net-local\n\nsyntax: (net-local int-socket)\n\nReturns the IP number and port of the local computer for a connection on a\nspecific int-socket.\n\n(net-local 16)  \226\134\146 (\"204.179.131.73\" 1689)\n\nUse the net-peer function to access the remote computer's IP number and port.")

(set '_net-lookup "{net-lookup\n\nsyntax: (net-lookup str-ip-number)\nsyntax: (net-lookup str-hostname [bool])\n\nReturns either a hostname string from str-ip-number in IP dot format or the IP\nnumber in dot format from str-hostname:\n\n(net-lookup \"209.24.120.224\")    \226\134\146 \"www.nuevatec.com\"\n(net-lookup \"www.nuevatec.com\")  \226\134\146 \"209.24.120.224\"\n\n(net-lookup \"216.16.84.66.sbl-xbl.spamhaus.org\" true)\n\226\134\146 \"216.16.84.66\"\n\nOptionally, a bool flag can be specified in the second syntax. If the\nexpression in bool evaluates to anything other than nil, host-by-name lookup\nwill be forced, even if the name string starts with an IP number.")

(set '_net-packet "{net-packet\n\nsyntax: (net-packet str-packet)\n\nThe function allows custom configured network packets to be sent via a raw\nsockets interface. The packet in str-packet must start with an IP (Internet\nProtocol) header followed by either a TCP, UDP or ICMP header and optional\ndata. newLISP must be run with super user privileges, and this function is only\navailable on Mac OS X, Linux and other Unix operating systems and only for\nIPv4. Currently net-packet is IPv4 only and has been tested on MAC OS X, Linux\nand OpenBSD.\n\nOn success the function returns the number of bytes sent. On failure the\nfunction returns nil and both, net-error and sys-error, should be inspected.\n\nWhen custom configured packets contains zeros in the checksum fields,\nnet-packet will calculate and insert the correct checksums. Already existing\nchecksums stay untouched.\n\nThe following example injects a UDP packet for IP number 192.168.1.92. The IP\nheader consists of 20 bytes ending with the target IP number. The following UDP\nheader has a length of 8 bytes and is followed by the data string Hello World.\nThe checksum bytes in both headers are left as 0x00 0x00 and will be\nrecalculated internally.\n\n; packet as generated by: (net-send-udp \"192.168.1.92\" 12345 \"Hello World\")\n\n(set 'udp-packet (pack (dup \"b\" 39) '(\n    0x45 0x00 0x00 0x27 0x4b 0x8f 0x00 0x00 0x40 0x11 0x00 0x00 192  168  1    95\n    192  168  1    92   0xf2 0xc8 0x30 0x39 0x00 0x13 0x00 0x00 0x48 0x65 0x6c 0x6c\n    0x6f 0x20 0x57 0x6f 0x72 0x6c 0x64)))\n\n(unless (net-packet udp-packet)\n    (println \"net-error: \" (net-error))\n    (println \"sys-error: \" (sys-error)))\n\nThe net-packet function is used when testing net security. Its wrong\napplication can upset the correct functioning of network routers and other\ndevices connected to a network. For this reason the function should only be\nused on well isolated, private intra-nets and only by network professionals.\n\nFor other examples of packet configuration, see the file qa-specific-tests/\nqa-packet in the newLISP source distribution.")

(set '_net-peek "{net-peek\n\nsyntax: (net-peek int-socket)\n\nReturns the number of bytes ready for reading on the network socket int-socket.\nIf an error occurs or the connection is closed, nil is returned.\n\n(set 'aSock (net-connect \"aserver.com\" 123))\n\n(while ( = (net-peek aSock) 0)\n    (do-something-else))\n\n(net-receive aSock buff 1024)\n\nAfter connecting, the program waits in a while loop until aSock can be read.\n\nUse the peek function to check file descriptors and stdin.")

(set '_net-peer "{net-peer\n\nsyntax: (net-peer int-socket)\n\nReturns the IP number and port number of the remote computer for a connection\non int-socket.\n\n(net-peer 16)  \226\134\146 (\"192.100.81.100\" 13)\n\nUse the net-local function to access the local computer's IP number and port\nnumber.")

(set '_net-ping [text]{net-ping

syntax: (net-ping str-address [int-timeout [int-count bool]]])
syntax: (net-ping list-addresses [int-timeout [int-count bool]]])

This function is only available on Unix-based systems and must be run in
superuser mode, i.e. using: sudo newlisp to start newLISP on Mac OS X or other
BSD's, or as the root user on Linux. Broadcast mode and specifying ranges with
the - (hyphen) or * (star) are not available on IPv6 address mode.

Since version 10.1.7, superuser mode is not required on Mac OS X.

In the first syntax, net-ping sends a ping ICMP 64-byte echo request to the
address specified in str-address. If it is a broadcast address, the ICMP packet
will be received by all addresses on the subnet. Note that for security
reasons, many computers do not answer ICMP broadcast ping (ICMP_ECHO) requests.
An optional timeout parameter can be specified in int-timeout. If no timeout is
specified, a waiting time of 1000 milliseconds (one second) is assumed.

net-ping returns either a list of lists of IP strings and round-trip time in
microseconds for which a response was received or an empty list if no response
was received.

A return value of nil indicates a failure. Use the net-error function to
retrieve the error message. If the message reads Cannot open socket, it is
probably because newLISP is running without root permissions. newLISP can be
started using:

sudo newlisp

Alternatively, newLISP can be installed with the set-user-ID bit set to run in
superuser mode.

(net-ping "newlisp.org")     → (("66.235.209.72" 634080))
(net-ping "127.0.0.1")       → (("127.0.0.1" 115))
(net-ping "yahoo.com" 3000)  → nil

In the second syntax, net-ping is run in batch mode. Only one socket is opened
in this mode, but multiple ICMP packets are sent out—one each to multiple
addresses specified in a list or specified by range. Packets are sent out as
fast as possible. In this case, multiple answers can be received. If the same
address is specified multiple times, the receiving IP address will be flooded
with ICMP packets.

To limit the number of responses to be waited for in broadcast or batch mode,
an additional argument indicating the maximum number of responses to receive
can be specified in int-count. Usage of this parameter can cause the function
to return sooner than the specified timeout. When a given number of responses
has been received, net-ping will return before the timeout has occurred. Not
specifying int-count or specifying 0 assumes an int-count equal to the number
of packets sent out.

As third optional parameter, a true value can be specified. This setting will
return an error string instead of the response time, if the host does not
answer.

(net-ping '("newlisp.org" "192.168.1.255") 2000 20)
→ (("66.235.209.72" 826420) ("192.168.1.1" 124) ("192.168.1.254" 210))

(net-ping "192.168.1.*" 500) ; from 1 to 254
→ (("192.168.1.1" 120) ("192.168.1.2" 245) ("192.168.2.3" 180) ("192.168.2.254" 234))

(net-ping "192.168.1.*" 500 2) ; returns after 2 responses
→ (("192.168.1.3" 115) ("192.168.1.1" 145))

(net-ping "192.168.1.1-10" 1000) ; returns after 1 second
→ (("192.168.1.3" 196) ("192.168.1.1" 205))

(net-ping '("192.168.1.100-120" "192.168.1.124-132") 2000) ; returns after 2 seconds
→ ()

Broadcast or batch mode—as well as normal addresses and IP numbers or
hostnames— can be mixed in one net-ping statement by putting all of the IP
specs into a list.

The second and third lines show how the batch mode of net-ping can be initiated
by specifying the * (asterisk) as a wildcard character for the last subnet
octet in the IP number. The fourth and fifth lines show how an IP range can be
specified for the last subnet octet in the IP number. net-ping will iterate
through all numbers from either 1 to 254 for the star * or the range specified,
sending an ICMP packet to each address. Note that this is different from the
broadcast mode specified with an IP octet of 255. While in broadcast mode,
net-ping sends out only one packet, which is received by multiple addresses.
Batch mode explicitly generates multiple packets, one for each target address.
When specifying broadcast mode, int-count should be specified, too.

When sending larger lists of IPs in batch mode over one socket, a longer
timeout may be necessary to allow enough time for all of the packets to be sent
out over one socket. If the timeout is too short, the function net-ping may
return an incomplete list or the empty list (). In this case, net-error will
return a timeout error. On error, nil is returned and net-error can be used to
retrieve an error message.

On some systems only lists up to a specific length can be handled regardless of
the timeout specified. In this case, the range should be broken up into
sub-ranges and used with multiple net-ping invocations. In any case, net-ping
will send out packages as quickly as possible.[/text])

(set '_net-receive "{net-receive !\n\nsyntax: (net-receive int-socket sym-buffer int-max-bytes [wait-string])\n\nReceives data on the socket int-socket into a string contained in sym-buffer.\nsym-buffer can also be a default functor specified by a context symbol for\nreference passing in and out of user-defined functions.\n\nA maximum of int-max-bytes is received. net-receive returns the number of bytes\nread. If there is a break in the connection, nil is returned. The space\nreserved in sym-buffer is exactly the size of bytes read.\n\nNote that net-receive is a blocking call and does not return until the data\narrives at int-socket. Use net-peek or net-select to find out if a socket is\nready for reading.\n\nOptionally, a wait-string can be specified as a fourth parameter. net-receive\nthen returns after a character or string of characters matching wait-string is\nreceived. The wait-string will be part of the data contained in sym-buffer.\n\n(define (gettime)\n    (net-connect \"netcom.com\" 13)\n    (net-receive socket buf 256)\n    (print buf \"\\n\")\n    (net-close socket))\n\nWhen calling gettime, the program connects to port 13 of the server netcom.com.\nPort 13 is a date-time service on most server installations. Upon connection,\nthe server sends a string containing the date and time of day.\n\n(define (net-receive-line socket sBuff)\n    (net-receive socket sBuff 256 \"\\n\"))\n\n(set 'bytesReceived (net-receive-line socket 'sm))\n\nThe second example defines a new function net-receive-line, which returns after\nreceiving a newline character (a string containing one character in this\nexample) or 256 characters. The \"\\n\" string is part of the contents of sBuff.\n\nNote that when the fourth parameter is specified, net-receive is slower than\nthe normal version because information is read character-by-character. In most\nsituations, the speed difference can be neglected.")

(set '_net-receive-from "{net-receive-from\n\nsyntax: (net-receive-from int-socket int-max-size)\n\nnet-receive-from can be used to set up non-blocking UDP communications. The\nsocket in int-socket must previously have been opened by either net-listen or\nnet-connect (both using the \"udp\" option). int-max-size specifies the maximum\nnumber of bytes that will be received. On Linux/BSD, if more bytes are\nreceived, those will be discarded; on Win32, net-receive-from returns nil and\ncloses the socket.\n\nOn success net-receive returns a list of the data string, remote IP number and\nremote port used. On failure it returns nil.\n\n;; listen on port 1001 on the default address\n(net-listen 1001 \"\" \"udp\")  \226\134\146 1980\n\n;; optionally poll for arriving data with 100ms timeout\n(while (not (net-select 1980 \"r\" 100000)) (do-something ... ))\n\n(net-receive-from 1980 20)  \226\134\146 (\"hello\" \"192.168.0.5\" 3240)\n\n;; send answer back to sender\n(net-send-to \"192.168.0.5\" 3240 \"hello to you\" 1980)\n\n(net-close 1980) ; close socket\n\nThe second line in this example is optional. Without it, the net-receive-from\ncall would block until data arrives. A UDP server could be set up by listening\nand polling several ports, serving them as they receive data.\n\nNote that net-receive could not be used in this case because it does not return\nthe sender's address and port information, which are required to talk back. In\nUDP communications, the data packet itself contains the address of the sender,\nnot the socket over which communication takes place. net-receive can also be\nused for TCP/IP communications.\n\nSee also the net-connect function with the \"udp\" option and the net-send-to\nfunction for sending UDP data packets over open connections.\n\nFor blocking short UDP transactions, see the net-send-udp and net-receive-udp\nfunctions.")

(set '_net-receive-udp "{net-receive-udp\n\nsyntax: (net-receive-udp int-port int-maxsize [int-microsec [str-addr-if]])\n\nReceives a User Datagram Protocol (UDP) packet on port int-port, reading\nint-maxsize bytes. If more than int-maxsize bytes are received, bytes over\nint-maxsize are discarded on Linux/BSD; on Win32, net-receive-udp returns nil.\nnet-receive-udp blocks until a datagram arrives or the optional timeout value\nin int-microsec expires. When setting up communications between datagram sender\nand receiver, the net-receive-udp statement must be set up first.\n\nNo previous setup using net-listen or net-connect is necessary.\n\nnet-receive-udp returns a list containing a string of the UDP packet followed\nby a string containing the sender's IP number and the port used.\n\n;; wait for datagram with maximum 20 bytes\n(net-receive-udp 10001 20)\n\n;; or\n(net-receive-udp 10001 20 5000000)  ; wait for max 5 seconds\n\n;; executed on remote computer\n(net-send-udp \"nuevatec.com\" 1001 \"Hello\")  \226\134\146 4\n\n;; returned from the net-receive-udp statement\n\226\134\146 (\"Hello\" \"128.121.96.1\" 3312)\n\n;; sending binary information\n(net-send-udp \"ahost.com\" 2222 (pack \"c c c c\" 0 1 2 3))\n\226\134\146 4\n\n;; extracting the received info\n(set 'buff (first (net-receive-udp 2222 10)))\n\n(print (unpack \"c c c c\" buff))  \226\134\146 (0 1 2 3)\n\nSee also the net-send-udp function for sending datagrams and the pack and\nunpack functions for packing and unpacking binary information.\n\nTo listen on a specified address on computers with more than one interface\ncard, an interface IP address or name can be optionally specified in\nstr-addr-if. When specifying str-addr-if, a timeout must also be specified in\nint-wait.\n\nAs an alternative, UDP communication can be set up using net-listen, or\nnet-connect together with the \"udp\" option to make non-blocking data exchange\npossible with net-receive-from and net-send-to.")

(set '_net-select [text]{net-select

syntax: (net-select int-socket str-mode int-micro-seconds)
syntax: (net-select list-sockets str-mode int-micro-seconds)

In the first form, net-select finds out about the status of one socket
specified in int-socket. Depending on str-mode, the socket can be checked if it
is ready for reading or writing, or if the socket has an error condition. A
timeout value is specified in int-micro-seconds.

In the second syntax, net-select can check for a list of sockets in
list-sockets.

The following value can be given for str-mode:

"read" or "r" to check if ready for reading or accepting.
"write" or "w" to check if ready for writing.
"exception" or "e" to check for an error condition.

Read, send, or accept operations can be handled without blocking by using the
net-select function. net-select waits for a socket to be ready for the value
given in int-micro-seconds, then returns true or nil depending on the readiness
of the socket. During the select loop, other portions of the program can run.
On error, net-error is set. When -1 is specified for int-micro-seconds,
net-select will never time out.

(set 'listen-socket (net-listen 1001))

;; wait for connection
(while (not (net-select listen-socket "read" 1000))
    (if (net-error) (print (net-error))))

(set 'connection (net-accept listen-socket))
(net-send connection "hello")

;; wait for incoming message
(while (not (net-select connection "read" 1000))
    (do-something))

(net-receive connection buff 1024)

When net-select is used, several listen and connection sockets can be watched,
and multiple connections can be handled. When used with a list of sockets,
net-select will return a list of ready sockets. The following example would
listen on two sockets and continue accepting and servicing connections:

(set 'listen-list '(1001 1002))

; accept-connection, read-connection and write-connection
; are userdefined functions

(while (not (net-error))
    (dolist (conn (net-select listen-list "r" 1000))
    (accept-connection conn))  ; build an accept-list

    (dolist (conn (net-select accept-list "r" 1000))
    (read-connection conn))    ; read on connected sockets

    (dolist (conn (net-select accept-list "w" 1000))
    (write-connection conn)))  ; write on connected sockets

In the second syntax, a list is returned containing all the sockets that passed
the test; if timeout occurred, an empty list is returned. An error causes
net-error to be set.

Note that supplying a nonexistent socket to net-select will cause an error to
be set in net-error.[/text])

(set '_net-send "{net-send\n\nsyntax: (net-send int-socket str-buffer [int-num-bytes])\n\nSends the contents of str-buffer on the connection specified by int-socket. If\nint-num-bytes is specified, up to int-num-bytes are sent. If int-num-bytes is\nnot specified, the entire contents will be sent. net-send returns the number of\nbytes sent or nil on failure.\n\nOn failure, use net-error to get more error information.\n\n(set 'buf \"hello there\")\n\n(net-send sock buf)       \226\134\146 11\n(net-send sock buf 5)     \226\134\146 5\n\n(net-send sock \"bye bye\") \226\134\146 7\n\nThe first net-send sends the string \"hello there\", while the second net-send\nsends only the string \"hello\".")

(set '_net-send-to "{net-send-to\n\nsyntax: (net-send-to str-remotehost int-remoteport str-buffer int-socket)\n\nCan be used for either UDP or TCP/IP communications. The socket in int-socket\nmust have previously been opened with a net-connect or net-listen function. If\nthe opening functions was used with the \"udp\" option, net-listen or net-connect\nare not used to listen or to connect but only to create the UDP socket. The\nhost in str-remotehost can be specified either as a hostname or as an IP-number\nstring.\n\nWhen using net-connect together with net-send-to, then only one of the\nfunctions should specify the remote host. The other should leave the address as\nan empty string.\n\n;;;;;;;;;;;;;;;;;; UDP server\n(set 'socket (net-listen 10001 \"\" \"udp\"))\n(if socket (println \"server listening on port \" 10001)\n       (println (net-error)))\n(while (not (net-error))\n   (set 'msg (net-receive-from socket 255))\n   (println \"-> \" msg)\n   (net-send-to (nth 1 msg) (nth 2 msg)\n                (upper-case (first msg)) socket))\n\n;;;;;;;;;;;;;;;;;; UDP client\n(set 'socket (net-listen 10002 \"\" \"udp\"))\n(if (not socket) (println (net-error)))\n(while (not (net-error))\n   (print \"> \")\n   (net-send-to \"127.0.0.1\" 10001 (read-line) socket)\n   (net-receive socket buff 255)\n   (println \"-> \" buff))\n\nIn the examples both, the client and the server use net-listen to create the\nUDP socket for sending and receiving. The server extracts the client address\nand port from the message received and uses it in the net-send-to statement.\n\nSee also the net-receive-from function and the net-listen function with the\n\"udp\" option.\n\nFor blocking short UDP transactions use net-send-udp and net-receive-udp.")

(set '_net-send-udp "{net-send-udp\n\nsyntax: (net-send-udp str-remotehost int-remoteport str-buffer [bool])\n\nSends a User Datagram Protocol (UDP) to the host specified in str-remotehost\nand to the port in int-remoteport. The data sent is in str-buffer.\n\nThe theoretical maximum data size of a UDP packet on an IPv4 system is 64K\nminus IP layer overhead, but much smaller on most Unix flavors. 8k seems to be\na safe size on Mac OS X, BSDs and Linux.\n\nNo previous setup using net-connect or net-listen is necessary. net-send-udp\nreturns immediately with the number of bytes sent and closes the socket used.\nIf no net-receive-udp statement is waiting at the receiving side, the datagram\nsent is lost. When using datagram communications over insecure connections,\nsetting up a simple protocol between sender and receiver is recommended for\nensuring delivery. UDP communication by itself does not guarantee reliable\ndelivery as TCP/IP does.\n\n(net-send-udp \"somehost.com\" 3333 \"Hello\")  \226\134\146 5\n\nnet-send-udp is also suitable for sending binary information (e.g., the zero\ncharacter or other non-visible bytes). For a more comprehensive example, see\nnet-receive-udp.\n\nOptionally, the sending socket can be put in broadcast mode by specifying true\nor any expression not evaluating to nil in bool:\n\n(net-send-udp \"192.168.1.255\" 2000 \"Hello\" true)  \226\134\146 5\n\nThe UDP will be sent to all nodes on the 192.168.1 network. Note that on some\noperating systems, sending the network mask 255 without the bool true option\nwill enable broadcast mode.\n\nAs an alternative, the net-connect function using the \"udp\" option\226\128\148together\nwith the net-send-to function\226\128\148can be used to talk to a UDP listener in a\nnon-blocking fashion.")

(set '_net-service "{net-service\n\nsyntax: (net-service str-service str-protocol)\nsyntax: (net-service int-port str-protocol)\n\nIn the first syntax net-service makes a lookup in the services database and\nreturns the standard port number for this service.\n\nIn the second syntax a service port is supplied in int-port to look up the\nservice name.\n\nReturns nil on failure.\n\n; get the port number from the name\n(net-service \"ftp\" \"tcp\")       \226\134\146 21\n(net-service \"http\" \"tcp\")      \226\134\146 80\n(net-service \"net-eval\" \"tcp\")  \226\134\146 4711  ; if configured\n\n; get the service name from the port number\n(net-service 22 \"tcp\")          \226\134\146 \"ssh\"")

(set '_net-sessions "{net-sessions\n\nsyntax: (net-sessions)\n\nReturns a list of active listening and connection sockets.")

(set '_new [text]{new

syntax: (new context-source sym-context-target [bool])
syntax: (new context-source)

The context context-source is copied to sym-context-target. If the target
context does not exist, a new context with the same variable names and
user-defined functions as in context-source is created. If the target context
already exists, then new symbols and definitions are added. Existing symbols
are only overwritten when the expression in bool evaluates to anything other
than nil; otherwise, the content of existing symbols will remain. This makes
mixins of context objects possible. new returns the target context, which
cannot be MAIN.

In the second syntax, the existing context in context-source gets copied into
the current context as the target context.

All references to symbols in the originating context will be translated to
references in the target context. This way, all functions and data structures
referring to symbols in the original context will now refer to symbols in the
target context.

(new CTX 'CTX-2)  → CTX-2

;; force overwrite of existing symbols
(new CTX MyCTX true)  → MyCTX

The first line in the example creates a new context called CTX-2 that has the
exact same structure as the original one. Note that CTX is not quoted because
contexts evaluate to themselves, but CTX-2 must be quoted because it does not
exist yet.

The second line merges the context CTX into MyCTX. Any existing symbols of the
same name in MyCTX will be overwritten. Because MyCTX already exists, the quote
before the context symbol can be omitted.

Context symbols need not be mentioned explicitly, but they can be contained in
variables:

(set 'foo:x 123)
(set 'bar:y 999)

(set 'ctxa foo)
(set 'ctxb bar)

(new ctxa ctxb)  ; from foo to bar

bar:x  → 123  ; x has been added to bar
bar:y  → 999)

The example refers to contexts in variables and merges context foo into bar.

See also the function def-new for moving and merging single functions instead
of entire contexts. See the context function for a more comprehensive example
of new.[/text])

(set '_nil? "{nil?\n\nsyntax: (nil? exp)\n\nIf the expression in exp evaluates to nil, then nil? returns true; otherwise,\nit returns nil.\n\n(map nil? '(x nil  1 nil \"hi\" ()))\n\226\134\146 (nil true nil true nil nil)\n\n(nil? nil)  \226\134\146 true\n(nil? '())  \226\134\146 nil\n\n; nil? means strictly nil\n(nil? (not '()))  \226\134\146 nil\n\nThe nil? predicate is useful for distinguishing between nil and the empty list\n().\n\nNote that nil? means strictly nil while true? means everything not nil or the\nempty list ().")

(set '_normal "{normal\n\nsyntax: (normal float-mean float-stdev int-n)\nsyntax: (normal float-mean float-stdev)\n\nIn the first form, normal returns a list of length int-n of random,\ncontinuously distributed floating point numbers with a mean of float-mean and a\nstandard deviation of float-stdev. The random generator used internally can be\nseeded using the seed function.\n\n(normal 10 3 10)\n\226\134\146 (7 6.563476562 11.93945312 6.153320312 9.98828125\n7.984375 10.17871094 6.58984375 9.42578125 12.11230469)\n\nIn the second form, normal returns a single normal distributed floating point\nnumber:\n\n(normal 0 1)  \226\134\146 0.6630859375\n\nSee also the random and rand functions for evenly distributed numbers, amb for\nrandomizing evaluation in a list of expressions, and seed for setting a\ndifferent start point for pseudo random number generation.")

(set '_not "{not\n\nsyntax: (not exp)\n\nIf exp evaluates to nil or the empty list (), then true is returned; otherwise,\nnil is returned.\n\n(not true)            \226\134\146 nil\n(not nil)             \226\134\146 true\n(not '())             \226\134\146 true\n(not (< 1 10))        \226\134\146 nil\n(not (not (< 1 10)))  \226\134\146 true")

(set '_now [text]{now

syntax: (now [int-minutes-offset [int-index]])

Returns information about the current date and time as a list of integers. An
optional time-zone offset can be specified in minutes in int-minutes-offset.
This causes the time to be shifted forward or backward in time, before being
split into separate date values.

An optional list-index in int-index makes now return a specific member in the
result list.

(now)       → (2002 2 27 18 21 30 140000 57 3 -300 0)
(now 0 -2)  → -300 ; minutes west of GMT


(apply date-value (now))  → 1014834090

The numbers represent the following date-time fields:

format                       description
year                         Gregorian calendar
month                        (1–12)
day                          (1–31)
hour                         (0–23) UTC
minute                       (0–59)
second                       (0–59)
microsecond                  (0–999999) OS-specific, millisecond resolution
day of current year          Jan 1st is 1
day of current week          (1–7) starting Monday
time zone offset in minutes  west of GMT
daylight savings time type   (0–6) on Linux/Unix or bias in minutes on Win32


The second example returns the Coordinated Universal Time (UTC) time value of
seconds after January 1, 1970.

Ranging from 0 to 23, hours are given in UTC and are not adjusted for the local
time zone. The resolution of the microseconds field depends on the operating
system and platform. On some platforms, the last three digits of the
microseconds field are always 0 (zero).

The "day of the week" field starts with 1 on Monday comforming to the ISO 8601
international standard for date and time representation.

On some platforms, the daylight savings time flag is not active and returns 0
(zero) even during daylight savings time (dst).

Depending on the geographical area, the daylight savings time type (dst) has a
different value from 1 to 6:

type area
0    not on dst
1    USA style dst
2    Australian style dst
3    Western European dst
4    Middle European dst
5    Eastern European dst
6    Canada dst

See also the date, date-list, date-parse, date-value, time, and time-of-day
functions.[/text])

(set '_nper "{nper\n\nsyntax: (nper num-interest num-pmt num-pv [num-fv [int-type]])\n\nCalculates the number of payments required to pay a loan of num-pv with a\nconstant interest rate of num-interest and payment num-pmt. If payment is at\nthe end of the period, int-type is 0 (zero) or int-type is omitted; for payment\nat the beginning of each period, int-type is 1.\n\n(nper (div 0.07 12) 775.30 -100000)  \226\134\146 239.9992828\n\nThe example calculates the number of monthly payments required to pay a loan of\n$100,000 at a yearly interest rate of 7 percent with payments of $775.30.\n\nSee also the fv, irr, npv, pmt, and pv functions.")

(set '_npv "{npv\n\nsyntax: (npv num-interest list-values)\n\nCalculates the net present value of an investment with a fixed interest rate\nnum-interest and a series of future payments and income in list-values.\nPayments are represented by negative values in list-values, while income is\nrepresented by positive values in list-values.\n\n(npv 0.1 '(1000 1000 1000))\n\226\134\146 2486.851991\n\n(npv 0.1 '(-2486.851991 1000 1000 1000))\n\226\134\146 -1.434386832e-08  ; ~ 0.0 (zero)\n\nIn the example, an initial investment of $2,481.85 would allow for an income of\n$1,000 after the end of the first, second, and third years.\n\nSee also the fv, irr, nper, pmt, and pv functions.")

(set '_nth [text]{nth utf8

syntax: (nth int-index list)
syntax: (nth int-index array)
syntax: (nth int-index str)

syntax: (nth list-indices list)
syntax: (nth list-indices array)

In the first syntax group nth uses int-index an index into the list, array or
str found and returning the element found at that index. See also Indexing
elements of strings and lists.

Multiple indices may be specified to recursively access elements in nested
lists or arrays. If there are more indices than nesting levels, the extra
indices are ignored. When multiple indices are used, they must be put in a list
as shown in the second syntax group.

(set 'L '(a b c))
(nth 0 L)    → a
; or simply
(L 0) → a

(set 'names '(john martha robert alex))
→ (john martha robert alex)

(nth 2 names)    → robert
; or simply
(names 2)        → robert

(names -1)       → alex


; multiple indices
(set 'persons '((john 30) (martha 120) ((john doe) 17)))

(persons 1 1)           → 120

(nth '(2 0 1) persons)  → doe

; or simply
(persons 2 0 1)        → doe

; multiple indices in a vector
(set 'v '(2 0 1))
(persons v)       → doe
(nth v persons)   → doe

; negative indices
(persons -2 0)    → martha

; out-of-bounds indices cause error
(persons 10)  → ERR: list index out of bounds
(person -5)   → ERR: list index out of bounds

The list L can be the context of the default functor L:L. This allows lists
passed by reference:

(set 'L:L '(a b c d e f g))

(define (second ctx)
        (nth 1 ctx))

(reverse L) → (g f e d c b a)
L:L → (g f e d c b a)

;; passing the list in L:L by reference
(second L)   → b

;; passing the list in L:L by value
(second L:L) → b

Reference passing is faster and uses less memory in big lists and should be
used on lists with more than a few hundred items.

Note that the implicit indexing version of nth is not breaking newLISP syntax
rules but should be understood as a logical expansion of newLISP syntax rules
to other data types than built-in functions or lambda expressions. A list in
the functor position of an s-expression assumes self-indexing functionality
using the index arguments following.

The implicit indexed syntax forms are faster but the other form with an
explicit nth may be more readable in some situations.

nth works on arrays just like it does on lists:

(set 'aArray (array 2 3 '(a b c d e f)))
→ ((a b c) (d e f))
(nth 1 aArray)      →  (d e f)
(aArray 1)          →  (d e f)

(nth '(1 0) aArray)    → d
(aArray 1 0)           → d
(aArray '(1 0))        → d

(set 'vec '(1 0))
(aArray vec)           → d

In the String version, nth returns the character found at the position
int-index in str and returns it as a string.

(nth  0 "newLISP")   → "n"

("newLISP" 0)        → "n"

("newLISP" -1)       → "P"

Note that nth works on character boundaries rather than byte boundaries when
using the UTF-8–enabled version of newLISP. To access ASCII and binary string
buffers on single byte boundaries use slice.

See also setf for modifying multidimensional lists and arrays and push and pop
for modifying lists.[/text])

(set '_null? "{null?\n\nsyntax: (null? exp)\n\nChecks if an expression evaluates to nil, the empty list (), the empty string\n\"\", NaN (not a number), or 0 (zero), in which case it returns true. In all\nother cases, null? returns nil. The predicate null? is useful in conjunction\nwith the functions filter or clean to check the outcome of other newLISP\noperations.\n\n(set 'x (sqrt -1)) \226\134\146 NaN ; or nan on UNIX\n(null? x) \226\134\146 true\n\n(map null? '(1 0 0.0 2 \"hello\" \"\" (a b c) () true))\n\226\134\146 (nil true true nil nil true nil true nil)\n\n(filter null? '(1 0 2 0.0 \"hello\" \"\" (a b c) () nil true))\n\226\134\146 (0 0 \"\" () nil)\n\n(clean null? '(1 0 2 0.0 \"hello\" \"\" (a b c) () nil true))\n\226\134\146 (1 2 \"hello\" (a b c) true)\n\nSee also the predicates empty?, nil? and zero?.")

(set '_number? "{number?\n\nsyntax: (number? exp)\n\ntrue is returned only if exp evaluates to a floating point number or an\ninteger; otherwise, nil is returned.\n\n(set 'x 1.23)\n(set 'y 456)\n(number? x)      \226\134\146 true\n(number? y)      \226\134\146 true\n(number? \"678\")  \226\134\146 nil\n\nSee the functions float? and integer? to test for a specific number type.")

(set '_odd? "{odd?\n\nsyntax: (odd? int-number)\n\nChecks the parity of an integer number. If the number is not even divisable by\n2, it has odd parity. When a floating point number is passed for int-number, it\nwill be converted first to an integer by cutting off its fractional part.\n\n(odd? 123)  \226\134\146 true\n(odd? 8)    \226\134\146 nil\n(odd? 8.7)  \226\134\146 nil\n\nUse even? to check if an integer is even, divisable by 2.")

(set '_open "{open\n\nsyntax: (open str-path-file str-access-mode [str-option])\n\nThe str-path-file is a file name, and str-access-mode is a string specifying\nthe file access mode. open returns an integer, which is a file handle to be\nused on subsequent read or write operations on the file. On failure, open\nreturns nil. The access mode \"write\" creates the file if it doesn't exist, or\nit truncates an existing file to 0 (zero) bytes in length.\n\nThe following strings are legal access modes:\n\n\"read\" or \"r\" for read only access\n\"write\" or \"w\" for write only access\n\"update\" or \"u\" for read/write access\n\"append\" or \"a\" for append read/write access\n\n\n(device (open \"newfile.data\" \"write\"))  \226\134\146 5\n(print \"hello world\\n\")  \226\134\146 \"hello world\"\n(close (device))         \226\134\146 5\n\n(set 'aFile (open \"newfile.data\" \"read\"))\n(seek aFile 6)\n(set 'inChar (read-char aFile))\n(print inChar \"\\n\")\n(close aFile)\n\nThe first example uses open to set the device for print and writes the word\n\"hello world\" into the file newfile.data. The second example reads a byte value\nat offset 6 in the same file (the ASCII value of 'w' is 119). Note that using\nclose on (device) automatically resets device to 0 (zero).\n\nAs an additional str-option, \"non-block\" or \"n\" can be specified after the\n\"read\" or \"write\" option. Only available on Unix systems, non-blocking mode can\nbe useful when opening named pipes but is not required to perform I/O on named\npipes.\n\nTo create a named pipe in newLISP, use the exec or import function:\n\n(exec \"mkfifo myfifo\")\n\n;; or alternatively\n\n(import \"/lib/libc.so.6\" \"mkfifo\")\n(mkfifo \"/tmp/myfifo\" 0777)\n\nThe named pipe can now be used like a file with open, read, and write.")

(set '_or "{or\n\nsyntax: (or exp-1 [exp-2 ... ])\n\nEvaluates expressions exp-x from left to right until finding a result that does\nnot evaluate to nil or the empty list (). The result is the return value of the\nor expression.\n\n(set 'x 10)\n(or (> x 100) (= x 10))          \226\134\146 true\n(or \"hello\" (> x 100) (= x 10))  \226\134\146 \"hello\"\n(or '())                         \226\134\146 ()\n(or true)                        \226\134\146 true\n(or)                             \226\134\146 nil")

(set '_ostype "{ostype\n\nsyntax: ostype\n\nostype is a built-in system constant containing the name of the operating\nsystem newLISP is running on.\n\nostype  \226\134\146 \"Win32\"\n\nOne of the following strings is returned: \"Linux\", \"BSD\", \"OSX\", \"Tru64Unix\",\n\"Solaris\", \"SunOS\", \"Win32\", or \"OS/2\".\n\nostype can be used to write platform-independent code:\n\n(if\n    (= ostype \"Linux\") (import \"libz.so\")\n    (= ostype \"BSD\") (import \"libz.so\")\n    (= ostype \"OSX\") (import \"libz.dylib\")\n    ...\n    (println \"cannot import libz on this platform\")\n)\n\nUse sys-info to learn more about the current flavor of newLISP running.\n\nFor a table of other built-in system variables and symbols see the chapter\nSystem Symbols and Constants in the appendix.")

(set '_pack [text]{pack

syntax: (pack str-format exp-1 [exp-2 ... ])
syntax: (pack str-format list)

syntax: (pack struct exp-1 [exp-2 ... ])
syntax: (pack struct list)

When the first parameter is a string, pack packs one or more expressions (exp-1
to exp-n) into a binary format specified in the format string str-format, and
returning the binary structure in a string buffer. The symmetrical unpack
function is used for unpacking. The expression arguments can also be given in a
list. pack and unpack are useful when reading and writing binary files (see
read and write) or when unpacking binary structures from return values of
imported C functions using import.

When the first parameter is the symbol of a struct definition, pack uses the
format as specified in struct. While pack with str-format literally packs as
specified, pack with struct will insert structure aligning pad-bytes depending
on data type, order of elements and CPU architecture. Refer to the description
of the struct function for more detail.

The following characters are used in str-format:

format     description
c          a signed 8-bit number
b          an unsigned 8-bit number
d          a signed 16-bit short number
u          an unsigned 16-bit short number
ld         a signed 32-bit long number
lu         an unsigned 32-bit long number
Ld         a signed 64-bit long number
Lu         an unsigned 64-bit long number
f          a float in 32-bit representation
lf         a double float in 64-bit representation
sn         a string of n null padded ASCII characters
nn         n null characters
>          switch to big endian byte order
<          switch to little endian byte order


pack will convert all floats into integers when passed to b, c, d, ld, or lu
formats. It will also convert integers into floats when passing them to f and
lf formats.

(pack "c c c" 65 66 67)  → "ABC"
(unpack "c c c" "ABC")   → (65 66 67)

(pack "c c c" 0 1 2)             → "\000\001\002"
(unpack "c c c" "\000\001\002")  → (0 1 2)

(set 's (pack "c d u" 10 12345 56789))
(unpack "c d u" s)  → (10 12345 56789)

(set 's (pack "s10 f" "result" 1.23))
(unpack "s10 f" s)
→ ("result\000\000\000\000" 1.230000019)

(pack "n10") → "\000\000\000\000\000\000\000\000\000\000"

(set 's (pack "s3 lf" "result" 1.23))
(unpack "s3 f" s)  → ("res" 1.23)

(set 's (pack "c n7 c" 11 22))
(unpack "c n7 c" s)  → (11 22))

(unpack "b" (pack "b" -1.0))  → (255)
(unpack "f" (pack "f" 123))   → (123)

The last two statements show how floating point numbers are converted into
integers when required by the format specification.

The expressions to pack can also be given in a list:

(set 'lst '("A" "B" "C"))
(set 'adr (pack "lululu" lst))
(map get-string (unpack "lululu" adr))    → ("A" "B" "C")

Note that the list should be referenced directly in pack, so the pointers
passed by adr are valid. adr would be written as char * adr[] in the
C-programming language and represents a 32-bit pointer to an array of 32-bit
string pointers.

The > and < specifiers can be used to switch between little endian and big
endian byte order when packing or unpacking:

(pack "d" 1)   → "\001\000"  ;; on little endian CPU
(pack ">d" 1)  → "\000\001"  ;; force big endian

(pack "ld" 1)   → "\001\000\000\000" ;; on little endian CPU
(pack "<ld" 1)  → "\000\000\000\001" ;; force big endian

(pack ">u <u" 1 1) → "\000\001\001\000" ;; switch twice

Switching the byte order will affect all number formats with 16-, 32-, or
64-bit sizes.

The pack and unpack format need not be the same:

(set 's (pack "s3" "ABC"))
(unpack "c c c" s)  → (65 66 67)

The examples show spaces between the format specifiers. These are not required
but can be used to improve readability.

See also the address, get-int, get-long, get-char, get-string, and unpack
functions.[/text])

(set '_parse [text]{parse

syntax: (parse str-data [str-break [int-option]])

Breaks the string that results from evaluating str-data into string tokens,
which are then returned in a list. When no str-break is given, parse tokenizes
according to newLISP's internal parsing rules. A string may be specified in
str-break for tokenizing only at the occurrence of a string. If an int-option
number is specified, a regular expression pattern may be used in str-break.

When str-break is not specified, the maximum token size is 2048 for quoted
strings and 256 for identifiers. In this case, newLISP uses the same faster
tokenizer it uses for parsing newLISP source. If str-break is specified, there
is no limitation on the length of tokens. A different algorithm is used that
splits the source string str-data at the string in str-break.

(parse "hello how are you")     → ("hello" "how" "are" "you")

(parse "one:two:three" ":")     → ("one" "two" "three")

(parse "one--two--three" "--")  → ("one" "two" "three")

(parse "one-two--three---four" "-+" 0)
→ ("one" "two" "three" "four")

(parse "hello regular   expression 1, 2, 3" {,\s*|\s+} 0)
→ ("hello" "regular" "expression" "1" "2" "3")

The last two examples show a regular expression as the break string with the
default option 0 (zero). Instead of { and } (left and right curly brackets),
double quotes can be used to limit the pattern. In this case, double
backslashes must be used inside the pattern. The last pattern could be used for
parsing CSV (Comma Separated Values) files. For the regular expression option
numbers, see regex.

parse will return empty fields around separators as empty strings:

(parse "1,2,3," ",") → ("1" "2" "3" "")
(parse "1,,,4" ",")  → ("1" "" "" "4")
(parse "," ",")      → ("" "")

(parse "")      → ()
(parse "" " ")  → ()

This behavior is needed when parsing records with empty fields.

Parsing an empty string will always result in an empty list.

Use the regex function to break strings up and the directory, find, find-all,
regex, replace, and search functions for using regular expressions.[/text])

(set '_peek "{peek\n\nsyntax: (peek int-handle)\n\nReturns the number of bytes ready to be read on a file descriptor; otherwise,\nit returns nil if the file descriptor is invalid. peek can also be used to\ncheck stdin. This function is only available on Unix-like operating systems.\n\n(peek 0)  ; check # of bytes ready on stdin\n\nUse the net-peek function to check for network sockets, or for the number of\navailable bytes on them. On Unix systems, net-peek can be used to check file\ndescriptors. The difference is that net-peek also sets net-error.")

(set '_pipe "{pipe\n\nsyntax: (pipe)\n\nCreates an inter-process communications pipe and returns the read and write\nhandles to it within a list.\n\n(pipe)  \226\134\146 (3 4)  ; 3 for read, 4 for writing\n\nThe pipe handles can be passed to a child process launched via process or to\nfork for inter-process communications.\n\nNote that the pipe does not block when being written to, but it does block\nreading until bytes are available. A read-line blocks until a newline character\nis received. A read blocks when fewer characters than specified are available\nfrom a pipe that has not had the writing end closed by all processes.\n\nMore than one pipe can be opened if required.\n\nnewLISP can also use named pipes. See the open function for further\ninformation.")

(set '_pmt "{pmt\n\nsyntax: (pmt num-interest num-periods num-principal [num-future-value [int-type\n]])\n\nCalculates the payment for a loan based on a constant interest of num-interest\nand constant payments over num-periods of time. num-future-value is the value\nof the loan at the end (typically 0.0). If payment is at the end of the period,\nint-type is 0 (zero) or int-type is omitted; for payment at the beginning of\neach period, int-type is 1.\n\n(pmt (div 0.07 12) 240 100000)  \226\134\146 -775.2989356\n\nThe above example calculates a payment of $775.30 for a loan of $100,000 at a\nyearly interest rate of 7 percent. It is calculated monthly and paid over 20\nyears (20 * 12 = 240 monthly periods). This illustrates the typical way payment\nis calculated for mortgages.\n\nSee also the fv, irr, nper, npv, and pv functions.")

(set '_pop "{pop ! utf8\n\nsyntax: (pop list [int-index-1 [int-index-2 ... ]])\nsyntax: (pop list [list-indexes])\n\nsyntax: (pop str [int-index [int-length]])\n\nUsing pop, elements can be removed from lists and characters from strings.\n\nIn the first syntax, pop extracts an element from the list found by evaluating\nlist. If a second parameter is present, the element at int-index is extracted\nand returned. See also Indexing elements of strings and lists.\n\nIn the second version, indices are specified in the list list-indexes. This\nway, pop works easily together with ref and ref-all, which return lists of\nindices.\n\npop changes the contents of the target list. The popped element is returned.\n\n(set 'pList '((f g) a b c \"hello\" d e 10))\n\n(pop pList)  \226\134\146 (f g)\n(pop pList)  \226\134\146 a\npList        \226\134\146 (b c \"hello\" d e 10)\n\n(pop pList 3)    \226\134\146 d\n(pop pList 100)  \226\134\146 10\npList            \226\134\146 (b c \"hello\" e)\n\n(pop pList -1)  \226\134\146 e\npList           \226\134\146 (b c \"hello\")\n\n(pop pList -2)  \226\134\146 c\npList           \226\134\146 (b \"hello\")\n\n(set 'pList '(a 2 (x y (p q) z)))\n\n(pop pList -1 2 0)  \226\134\146 p\n\n;; use indices in a list\n(set 'pList '(a b (c d () e)))\n\n(push 'x pList '(2 2 0))\n\226\134\146 (a b (c d (x) e))\n\npList\n\226\134\146 (a b (c d (x) e))\n\n(ref 'x pList)  \226\134\146 (2 2 0)\n\n(pop pList '(2 2 0))  \226\134\146 x\n\npop can also be used on strings with one index:\n\n;; use pop on strings\n\n(set 'str \"newLISP\")\n\n(pop str -4 4)  \226\134\146 \"LISP\"\n\nstr  \226\134\146 \"new\"\n\n(pop str 1)  \226\134\146 \"e\"\n\nstr  \226\134\146 \"nw\"\n\n(set 'str \"x\")\n\n(pop str)  \226\134\146 \"x\"\n(pop str)  \226\134\146 \"\"\n\nPopping an empty string will return an empty string.\n\nSee also the push function, the inverse operation to pop.")

(set '_pop-assoc "{pop-assoc !\n\nsyntax: (pop-assoc exp-key list-assoc)\nsyntax: (pop-assoc list-keys list-assoc)\n\nRemoves an association referred to by the key in exp-key from the association\nlist in list-assoc and returns the popped expression.\n\n;; simple associations\n\n(set 'L '((a 1) (b 2) (c 3)))\n(pop-assoc 'b L) \226\134\146 (b 2)\nL \226\134\146 ((a 1) (c 3))\n\n;; nested associations\n\n(set 'L '((a (b 1) (c (d 2)))))\n(pop-assoc 'a L) \226\134\146 (a (b 1) (c (d 2)))\nL \226\134\146 ()\n\n(set 'L '((a (b 1) (c (d 2)))))\n(pop-assoc '(a b) L)  \226\134\146 (b 1)\nL \226\134\146  ((a (c (d 2))))\n\n(set 'L '((a (b 1) (c (d 2)))))\n(pop-assoc '(a c) L)  \226\134\146 (c (d 2))\nL \226\134\146 ((a (b 1))))\n\nSee also assoc for retrieving associations and setf for modifying association\nlists.")

(set '_post-url "{post-url\n\nsyntax: (post-url str-url str-content [str-content-type [str-option] [\nint-timeout [ str-header]]])\n\nSends an HTTP POST request to the URL in str-url. POST requests are used to\npost information collected from web entry forms to a web site. Most of the\ntime, the function post-url mimics what a web browser would do when sending\ninformation collected in an HTML form to a server, but it can also be used to\nupload files (see an HTTP reference). The function returns the page returned\nfrom the server in a string.\n\nWhen post-url encounters an error, it returns a string description of the error\nbeginning with ERR:.\n\nThe last parameter, int-timeout, is for an optional timeout value, which is\nspecified in milliseconds. When no response from the host is received before\nthe timeout has expired, the string ERR: timeout is returned.\n\n;; specify content type\n(post-url \"http://somesite.com/form.pl\"\n          \"name=johnDoe&city=New%20York\"\n          \"application/x-www-form-urlencoded\")\n\n;; specify content type and timeout\n(post-url \"http://somesite.com/form.pl\"\n          \"name=johnDoe&city=New%20York\"\n          \"application/x-www-form-urlencoded\" 8000)\n\n;; assumes default content type and no timeout\n(post-url \"http://somesite.com/form.pl\"\n          \"name=johnDoe&city=New%20York\"\n\nThe above example uploads a user name and city using a special format called\napplication/x-www-form-urlencoded. post-url can be used to post other content\ntypes such as files or binary data. See an HTTP reference for other\ncontent-type specifications and data encoding formats. When the content-type\nparameter is omitted, post-url assumes application/x-www-form-urlencoded as the\ndefault content type.\n\nAdditional parameters\n\nWhen str-content-type is specified, the str-option \"header\" or \"list\" can be\nspecified as the return page. If the int-timeout option is specified, the\ncustom header option str-header can be specified, as well. See the function\nget-url for details on both of these options.\n\nSee also the get-url and put-url functions.")

(set '_pow "{pow\n\nsyntax: (pow num-1 num-2 [num-3 ... ])\nsyntax: (pow num-1)\n\nCalculates num-1 to the power of num-2 and so forth.\n\n(pow 100 2)      \226\134\146 10000\n(pow 100 0.5)    \226\134\146 10\n(pow 100 0.5 3)  \226\134\146 1000\n\n(pow 3)  \226\134\146 9\n\nWhen num-1 is the only argument, pow assumes 2 for the exponent.")

(set '_prefix "{prefix\n\nsyntax: (prefix sym)\n\nReturns the context of a symbol in sym:\n\n(setf s 'Foo:bar)      \226\134\146 Foo:bar\n(prefix s)             \226\134\146 Foo\n(context? (prefix s))  \226\134\146 true\n\n(term s)                         \226\134\146 \"bar\"\n(= s (sym (term s) (prefix s)))  \226\134\146 true\n\n>(context (prefix s))   ; switches to context Foo\nFoo\nFoo>\n\nSee also term to extract the term part of a symbol.")

(set '_pretty-print "{pretty-print\n\nsyntax: (pretty-print [int-length [str-tab [str-fp-format]])\n\nReformats expressions for print, save, or source and when printing in an\ninteractive console. The first parameter, int-length, specifies the maximum\nline length, and str-tab specifies the string used to indent lines. The third\nparameter str-fp-format describes the default format for printing floating\npoint numbers. All parameters are optional. pretty-print returns the current\nsettings or the new settings when parameters are specified.\n\n(pretty-print)  \226\134\146 (80 \" \" \"%1.10g\")  ; default setting\n\n(pretty-print 90 \"\\t\")  \226\134\146 (90 \"\\t\")\n\n(pretty-print 100)  \226\134\146 (100 \"\\t\")\n\n(sin 1)    \226\134\146 0.8414709848\n(pretty-print 80 \" \" \"%1.3f\")\n(sin 1)    \226\134\146 0.841\n\n(set 'x 0.0)\nx   \226\134\146 0.000\n\nThe first example reports the default settings of 80 for the maximum line\nlength and a space character for indenting. The second example changes the line\nlength to 90 and the indent to a TAB character. The third example changes the\nline length only. The last example changes the default format for floating\npoint numbers. This is useful when printing unformatted floating point numbers\nwithout fractional parts, and these numbers should still be recognizable as\nfloating point numbers. Without the custom format, x would be printed as 0\nindistinguishable from floating point number. All situations where unformatted\nfloating point numbers are printed, are affected.\n\nNote that pretty-print cannot be used to prevent line breaks from being\nprinted. To completely suppress pretty printing, use the function string to\nconvert the expression to a raw unformatted string as follows:\n\n;; print without formatting\n\n(print (string my-expression))")

(set '_primitive? "{primitive?\n\nsyntax: (primitive? exp)\n\nEvaluates and tests if exp is a primitive symbol and returns true or nil\ndepending on the result.\n\n(set 'var define)\n(primitive? var)  \226\134\146 true")

(set '_print "{print\n\nsyntax: (print exp-1 [exp-2 ... ])\n\nEvaluates and prints exp-1\226\128\148 to the current I/O device, which defaults to the\nconsole window. See the built-in function device for details on how to specify\na different I/O device.\n\nList expressions are indented by the nesting levels of their opening\nparentheses.\n\nSeveral special characters may be included in strings encoded with the escape\ncharacter \\:\n\ncharacter description\n\\n        the line-feed character (ASCII 10)\n\\r        the carriage-return character (ASCII 13)\n\\t        the tab character (ASCII 9)\n\\nnn      where nnn is a decimal ASCII code between 000 and 255\n\\xnn      where nn is a hexadecimal ASCII code between 00 and FF\n\n\n(print (set 'res (+ 1 2 3)))\n(print \"the result is\" res \"\\n\")\n\n\"\\065\\066\\067\"  \226\134\146 \"ABC\"\n\nTo finish printing with a line-feed, use println.")

(set '_println "{println\n\nsyntax: (println exp-1 [exp-2 ... ])\n\nEvaluates and prints exp-1\226\128\148 to the current I/O device, which defaults to the\nconsole window. A line-feed is printed at the end. See the built-in function\ndevice for details on how to specify a different I/O device. println works\nexactly like print but emits a line-feed character at the end.\n\nSee also the write-line and print functions.")

(set '_prob-chi2 "{prob-chi2\n\nsyntax: (prob-chi2 num-chi2 int-df)\n\nReturns the probability of an observed Chi\194\178 statistic in num-chi2 with num-df\ndegrees of freedom to be equal or greater under the null hypothesis. prob-chi2\nis derived from the incomplete Gamma function gammai.\n\n(prob-chi2 10 6)  \226\134\146 0.1246520195\n\nSee also the inverse function crit-chi2.")

(set '_prob-f "{prob-f\n\nsyntax: (prob-f num-f int-df1 int-df2)\n\nReturns the probability of an observed F statistic in num-f with int-df1 and\nint-df2 degrees of freedom to be equal or greater under the null hypothesis.\n\n(prob-f 2.75 10 12)  \226\134\146 0.0501990804\n\nSee also the inverse function crit-f.")

(set '_prob-t "{prob-t\n\nsyntax: (prob-t num-t int-df1)\n\nReturns the probability of an observed Student's t statistic in num-t with\nint-df degrees of freedom to be equal or greater under the null hypothesis.\n\n(prob-t 1.76 14)  \226\134\146 0.05011454551\n\nSee also the inverse function crit-t.")

(set '_prob-z "{prob-z\n\nsyntax: (prob-z num-z)\n\nReturns the probability of num-z, not to exceed the observed value where num-z\nis a normal distributed value with a mean of 0.0 and a standard deviation of\n1.0.\n\n(prob-z 0.0)  \226\134\146 0.5\n\nSee also the inverse function crit-z.")

(set '_process [text]{process

syntax: (process str-command)
syntax: (process str-command int-pipe-in int-pipe-out [int-win32-option])
syntax: (process str-command int-pipe-in int-pipe-out [int-unix-pipe-error])

In the first syntax, process launches a process specified in str-command and
immediately returns with a process ID or nil if a process could not be created.
This process will execute the program specified or immediately die if
str-command could not be executed.

On Mac OS X and other Unixes, the application or script must be specified with
its full path-name. The new process inherits the OS environment from the parent
process.

Command line arguments are parsed out at spaces. Arguments containing spaces
must be delimited using single quotes on Mac OS X and other Unixes. On Win32,
double quotes are used. The process id returned can be used to destroy the
running process using destroy, if the process does not exit by itself.

(process "c:/WINDOWS/system32/notepad.exe")  → 1894 ; on Win32

; find out the path of the program to start using exec,
; if the path is not known

(process (first (exec "which xclock")))  → 22607 ; on Unix

If the path of the executable is unknown, exec together with the Unix which
command can be used to start a program. The pid returned can be used to destroy
the process.

In the second syntax, standard input and output of the created process can be
redirected to pipe handles. When remapping standard I/O of the launched
application to a pipe, it is possible to communicate with the other application
via write-line and read-line or write and read statements:

;; Linux/Unix
;; create pipes
(map set '(myin bcout) (pipe))
(map set '(bcin myout) (pipe))

;; launch Unix 'bc' calculator application
(process "/usr/bin/bc" bcin bcout) → 7916

(write myout "3 + 4\n")  ; bc expects a line-feed

(read-line myin)  → "7"


;; bc can use bignums with arbitrary precision

(write myout "123456789012345 * 123456789012345\n")

(read-line myin)  → "15241578753238669120562399025"

;; destroy the process
(destroy 7916)

;; Win32
(map set '(myin cmdout) (pipe))
(map set '(cmdin myout) (pipe))

(process "c:/Program Files/newlisp/newlisp.exe -c" cmdin cmdout)
→ 1284

(write-line myout "(+ 3 4)")

(read-line myin) → "7"

;; destroy the process
(destroy 1284)

On Win32 versions of newLISP, a fourth optional parameter of int-win32-option
can be specified to control the display status of the application. This option
defaults to 1 for showing the application's window, 0 for hiding it, and 2 for
showing it minimized on the Windows launch bar.

On both Win32 and Linux/Unix systems, standard error will be redirected to
standard out by default. On Linux/Unix, an optional pipe handle for standard
error output can be defined in int-unix-pipe-error.

The function peek can be used to check for information on the pipe handles:

;; create pipes
(map set '(myin bcout) (pipe))
(map set '(bcin myout) (pipe))
(map set '(errin errout) (pipe))

;; launch Unix 'bc' calculator application
(process "bc" bcin bcout errout)

(write myout command)

;; wait for bc sending result or error info
(while (and (= (peek myin) 0)
            (= (peek errin) 0)) (sleep 10))

(if (> (peek errin) 0)
        (println (read-line errin)))

(if (> (peek myin) 0)
        (println (read-line myin)))

Not all interactive console applications can have their standard I/O channels
remapped. Sometimes only one channel, in or out, can be remapped. In this case,
specify 0 (zero) for the unused channel. The following statement uses only the
launched application's output:

(process "app" 0 appout)

Normally, two pipes are used: one for communications to the child process and
the other one for communications from the child process.

See also the pipe and share functions for inter-process communications and the
semaphore function for synchronization of several processes. See the fork
function for starting separate newLISP processes on Linux/Unix.[/text])

(set '_prompt-event "{prompt-event\n\nsyntax: (prompt-event sym-event-handler | func-event-handler)\n\nRefines the prompt as shown in the interactive newLISP shell. The\nsym-event-handler or func-event-handler is either a symbol of a user-defined\nfunction or a lambda expression:\n\n> (prompt-event (fn (ctx) (string ctx \":\" (real-path) \"$ \")))\n$prompt-event\nMAIN:/Users/newlisp$ (+ 3 4)\n7\nMAIN:/Users/newlisp$\n\nThe current context before calling the prompt-event code is passed as a\nparameter to the function. Computer output is shown in bold.\n\nThe example redefines the > prompt to be the current context followed by a\ncolon :, followed by the directory name, followed by the dollar symbol.\nTogether with the command-event function this can be used to create fully\ncustomized shells or custom command interpreters.\n\nThe function in prompt-event must return a string of 63 characters maximum. Not\nreturning a string will leave the prompt unchanged.")

(set '_protected? "{protected?\n\nsyntax: (protected? sym)\n\nChecks if a symbol in sym is protected. Protected symbols are built-in\nfunctions, context symbols, and all symbols made constant using the constant\nfunction:\n\n(protected? 'println)    \226\134\146 true\n(constant 'aVar 123)\n(protected? 'aVar)       \226\134\146 true")

(set '_push [text]{push ! utf8

syntax: (push exp list [int-index-1 [int-index-2 ... ]])
syntax: (push exp list [list-indexes])

syntax: (push str-1 str-2 [int-index])

Inserts the value of exp into the list list. If int-index is present, the
element is inserted at that index. If the index is absent, the element is
inserted at index 0 (zero), the first element. push is a destructive operation
that changes the contents of the target list.

The list changed is returned as a reference on which other built-in functions
can work. See also Indexing elements of strings and lists.

If more than one int-index is present, the indices are used to access a nested
list structure. Improper indices (those not matching list elements) are
discarded.

The second version takes a list of list-indexes but is otherwise identical to
the first. In this way, push works easily together with ref and ref-all, which
return lists of indices.

If list does not contain a list, list must contain a nil and will be
initialized to the empty list.

Repeatedly using push to the end of a list using -1 as the int-index is
optimized and as fast as pushing to the front of a list with no index at all.
This can be used to efficiently grow a list.

; inserting in front
(set 'pList '(b c))  → (b c)
(push 'a pList)      → (a b c)
pList                → (a b c)

; insert at index
(push "hello" pList 2)  → (a b "hello" c)

; optimized appending at the end
(push 'z pList -1)  → (a b "hello" c z)

; inserting lists in lists
(push '(f g) pList)  → ((f g) a b "hello" c z)

; inserting at negative index
(push 'x pList -3)  → ((f g) a b "hello" x c z)

; using multiple indices
(push 'h pList 0 -1)  → ((f g h) a b "hello" x c z)

; use indices in a list
(set 'pList '(a b (c d () e)))

(push 'x pList '(2 2 0))  → (a b (c d (x) e))

(ref 'x pList)   → (2 2 0)

(pop pList '(2 2 0))  → x

; push on un-initialized symbol
aVar  → nil

(push 999 aVar)  → (999)

aVar  → (999)

push and pop can be combined to model a queue:

; pop and push a as a queue
(set 'Q '(a b c d e))

(pop (push 'f Q -1)) → a
(pop (push 'g Q -1)) → b

Q →  (c d e f g)

Because push returns a reference to the modified list, pop can work on it
directly.

In the third syntax push can be used to change strings. When int-index is used,
it refers to character positions rather than byte positions. UTF-8 characters
may be multi-byte characters.

;; push on strings

(set 'str "abcdefg")

(push "hijk" str -1)  → "abcdefghijk"
str                   → "abcdefghijk"

(push "123" str)  → "123abcdefghijk"
(push "4" str 3)  → "1234abcdefghijk"

(set 'str "\u03b1\u03b2\u03b3")  →  "αβγ"

(push "*" str 1)  →  "α*βγ"

;; push on a string reference

(set 'lst '("abc" "xyz"))

(push x (lst 0)) → "xabc"

lst → ("xabc" "xyz")

See also the pop function, which is the inverse operation to push.[/text])

(set '_put-url [text]{put-url

syntax: (put-url str-url str-content [str-option] [int-timeout [str-header]])

The HTTP PUT protocol is used to transfer information in str-content to a file
specified in str-url. The lesser-known HTTP PUT mode is frequently used for
transferring web pages from HTML editors to Web servers. In order to use PUT
mode, the web server's software must be configured correctly. On the Apache web
server, use the 'Script PUT' directive in the section where directory access
rights are configured.

If str-url starts with file:// then str-content is written to the local file
system.

Optionally, an int-timeout value can be specified in milliseconds as the last
parameter. put-url will return ERR: timeout when the host gives no response and
the timeout expires. On other error conditions, put-url returns a string
starting with ERR: and the description of the error.

put-url requests are also understood by newLISP server nodes.

(put-url "http://asite.com/myFile.txt" "Hi there")
(put-url "http://asite.com/myFile.txt" "Hi there" 2000)

(put-url "http://asite.com/webpage.html"
    (read-file "webpage.html"))

; write /home/joe/newfile.txt on the local file system
(puts-url "file:///home/joe/newfile.txt" "Hello World!")

The first example creates a file called myFile.txt on the target server and
stores the text string 'Hi there' in it. In the second example, the local file
webpage.html is transferred to asite.com.

On an Apache web server, the following could be configured in httpd.conf.

<directory /www/htdocs>
Options All
Script PUT /cgi-bin/put.cgi
</directory>

The script put.cgi would contain code to receive content from the web server
via STDIN. The following is a working put.cgi written in newLISP for the Apache
web server:

#!/usr/home/johndoe/bin/newlisp
#
#
# get PUT method data from CGI STDIN
# and write data to a file specified
# int the PUT request
#
#


(print "Content-Type: text/html\n\n")

(set 'cnt 0)
(set 'result "")

(if (= "PUT" (env "REQUEST_METHOD"))
    (begin
      (set 'len (int (env "CONTENT_LENGTH")))

      (while (< cnt len)
          (set 'n (read (device) buffer len))
          (if (not n)
            (set 'cnt len)
            (begin
              (inc cnt n)
              (write result buffer))))

      (set 'path (append
              "/usr/home/johndoe"
              (env "PATH_TRANSLATED")))

      (write-file path result)
    )
)

(exit)

Note that the script appends ".txt" to the path to avoid the CGI execution of
uploaded malicious scripts. Note also that the two lines where the file path is
composed may work differently in your web server environment. Check environment
variables passed by your web server for composition of the right file path.

put-url returns content returned by the put.cgi script.

Additional parameters

In str-option, "header" or "list" can be specified for the returned page. If
the int-timeout option is specified, the custom header option str-header can be
specified, as well. See the function get-url for details on both of these
options.

See also the functions get-url and post-url, which can be used to upload files
when formatting form data as multipart/form-data.[/text])

(set '_pv "{pv\n\nsyntax: (pv num-int num-nper num-pmt [num-fv [int-type]])\n\nCalculates the present value of a loan with the constant interest rate\nnum-interest and the constant payment num-pmt after num-nper number of\npayments. The future value num-fv is assumed to be 0.0 if omitted. If payment\nis at the end of the period, int-type is 0 (zero) or int-type is omitted; for\npayment at the beginning of each period, int-type is 1.\n\n(pv (div 0.07 12) 240 775.30)  \226\134\146 -100000.1373\n\nIn the example, a loan that would be paid off (future value = 0.0) in 240\npayments of $775.30 at a constant interest rate of 7 percent per year would\nstart out at $100,000.14.\n\nSee also the fv, irr, nper, npv, and pmt functions.")

(set '_quote "{quote\n\nsyntax: (quote exp)\n\nReturns exp without evaluating it. The same effect can be obtained by\nprepending a ' (single quote) to exp.\n\n(quote x)         \226\134\146 x\n(quote 123)       \226\134\146 123\n(quote (a b c))   \226\134\146 (a b c)\n(= (quote x) 'x)  \226\134\146 true")

(set '_quote? "{quote?\n\nsyntax: (quote? exp)\n\nEvaluates and tests whether exp is quoted. Returns true or nil depending on the\nresult.\n\n(set 'var ''x)  \226\134\146 'x\n(quote? var)    \226\134\146 true\n\nNote that in the set statement, ''x is quoted twice because the first quote is\nlost during the evaluation of the set assignment.")

(set '_rand "{rand\n\nsyntax: (rand int-range [int-N])\n\nEvaluates the expression in int-range and generates a random number in the\nrange of 0 (zero) to (int-range - 1). When 0 (zero) is passed, the internal\nrandom generator is initialized using the current value returned by the C time\n() function. Optionally, a second parameter can be specified to return a list\nof length int-N of random numbers.\n\n(dotimes (x 100) (print (rand 2))) =>\n11100000110100111100111101 ... 10111101011101111101001100001000\n\n(rand 3 100)  \226\134\146 (2 0 1 1 2 0 \226\128\166)\n\nThe first line in the example prints equally distributed 0's and 1's, while the\nsecond line produces a list of 100 integers with 0, 1, and 2 equally\ndistributed. Use the random and normal functions to generate floating point\nrandom numbers, and use seed to vary the initial seed for random number\ngeneration.")

(set '_random "{random\n\nsyntax: (random float-offset float-scale int-n)\nsyntax: (random float-offset float-scale)\n\nIn the first form, random returns a list of int-n evenly distributed floating\npoint numbers scaled (multiplied) by float-scale, with an added offset of\nfloat-offset. The starting point of the internal random generator can be seeded\nusing seed.\n\n(random 0 1 10)\n\226\134\146 (0.10898973 0.69823783 0.56434872 0.041507289 0.16516733\n    0.81540917 0.68553784 0.76471068 0.82314585 0.95924564)\n\nWhen used in the second form, random returns a single evenly distributed\nnumber:\n\n(random 10 5)  \226\134\146 11.0971\n\nSee also the normal and rand functions.")

(set '_randomize "{randomize\n\nsyntax: (randomize list [bool])\n\nRearranges the order of elements in list into a random order.\n\n(randomize '(a b c d e f g))  \226\134\146 (b a c g d e f)\n(randomize (sequence 1 5))    \226\134\146 (3 5 4 1 2)\n\nrandomize will always return a sequence different from the previous one without\nthe optional bool flag. This may require the function to calculate several sets\nof reordered elements, which in turn may lead to different processing times\nwith different invocations of the function on the same input list length. To\nallow for the output to be equal to the input, true or any expression\nevaluating to not nil must be specified in bool.\n\nrandomize uses an internal pseudo random sequence generator that returns the\nsame series of results each time newLISP is started. Use the seed function to\nchange this sequence.")

(set '_read "{read !\n\nsyntax: (read int-file sym-buffer int-size [str-wait])\n\nReads a maximum of int-size bytes from a file specified in int-file into a\nbuffer in sym-buffer. Any data referenced by the symbol sym-buffer prior to the\nreading is deleted. The handle in int-file is obtained from a previous open\nstatement. The symbol sym-buffer contains data of type string after the read\noperation. sym-buffer can also be a default functor specified by a context\nsymbol for reference passing in and out of user-defined functions.\n\nread is a shorter writing of read-buffer. The longer form still works but is\ndeprecated and should be avoided in new code.\n\nOptionally, a string to be waited for can be specified in str-wait. read will\nread a maximum amount of bytes specified in int-size or return earlier if\nstr-wait was found in the data. The wait-string is part of the returned data\nand must not contain binary 0 (zero) characters.\n\nReturns the number of bytes read or nil when the wait-string was not found. In\nany case, the bytes read are put into the buffer pointed to by sym-buffer, and\nthe file pointer of the file read is moved forward. If no new bytes have been\nread, sym-buffer will contain nil.\n\n(set 'handle (open \"aFile.ext\" \"read\"))\n(read handle buff 200)\n\nReads 200 bytes into the symbol buff from the file aFile.ext.\n\n(read handle buff 1000 \"password:\")\n\nReads 1000 bytes or until the string password: is encountered. The string\npassword: will be part of the data returned.\n\nSee also the write function.")

(set '_read-char "{read-char\n\nsyntax: (read-char [int-file])\n\nReads a byte from a file specified by the file handle in int-file or from the\ncurrent I/O device - e.g. stdin - when no file handle is specified. The file\nhandle is obtained from a previous open operation. Each read-char advances the\nfile pointer by one byte. Once the end of the file is reached, nil is returned.\n\n(define (slow-file-copy from-file to-file)\n    (set 'in-file (open from-file \"read\"))\n    (set 'out-file (open to-file \"write\"))\n    (while (set 'chr (read-char in-file))\n        (write-char out-file chr))\n    (close in-file)\n    (close out-file)\n    \"finished\")\n\nUse read-line and device to read whole text lines at a time. Note that newLISP\nsupplies a fast built-in function called copy-file for copying files.\n\nSee also the write-char function.")

(set '_read-expr "{read-expr\n\nsyntax: (read-expr str-source [sym-context [exp-error [int-offset]]])\n\nread-expr parses the first expressions it finds in str-source and returns the\ntranslated expression without evaluating it. An optional context in sym-context\nspecifies a namespace for the translated expression.\n\nAfter a call to read-expr the system variable $0 contains the number of\ncharacters scanned.\n\nIf an error occurs when translating str-source the expression in exp-error is\nevaluated and the result returned.\n\nint-offset specifies an optional offset into str-source where processing should\nstart. When calling read-expr repeatedly this number can be updated using $0.\n\n(set 'code \"; a statement\\n(define (double x) (+ x x))\")\n\n(read-expr code) \226\134\146 (define (double x) (+ x x))\n\n$0 \226\134\146 41\n\n\nread-expr behaves similar to eval-string but without the evaluation step:\n\n(read-expr \"(+ 3 4)\")    \226\134\146 (+ 3 4)\n\n(eval-string \"(+ 3 4)\")  \226\134\146 7\n\nUsing read-expr a customized code reader can be programmed preprocessing\nexpressions before evaluation.\n\nSee also reader-event for preprocessing expressions event-driven.")

(set '_read-file "{read-file\n\nsyntax: (read-file str-file-name)\n\nReads a file in str-file-name in one swoop and returns a string buffer\ncontaining the data.\n\nOn failure the function returns nil. For error information, use sys-error when\nused on files. When used on URLs net-error gives more error information.\n\n(write-file \"myfile.enc\"\n    (encrypt (read-file \"/home/lisp/myFile\") \"secret\"))\n\nThe file myfile is read, then encrypted using the password \"secret\" before\nbeing written back into a new file titled \"myfile.enc\" in the current\ndirectory.\n\nread-file can take an http:// or file:// URL in str-file-name. When the prefix\nis http://, read-file works exactly like get-url and can take the same\nadditional parameters.\n\n(read-file \"http://asite.com/somefile.tgz\" 10000)\n\nThe file somefile.tgz is retrieved from the remote location http://asite.com.\nThe file transfer will time out after 10 seconds if it is not finished. In this\nmode, read-file can also be used to transfer files from remote newLISP server\nnodes.\n\nSee also the write-file and append-file functions.")

(set '_read-key "{read-key\n\nsyntax: (read-key)\n\nReads a key from the keyboard and returns an integer value. For navigation\nkeys, more than one read-key call must be made. For keys representing ASCII\ncharacters, the return value is the same on all OSes, except for navigation\nkeys and other control sequences like function keys, in which case the return\nvalues may vary on different OSes and configurations.\n\n(read-key)  \226\134\146 97  ; after hitting the A key\n(read-key)  \226\134\146 65  ; after hitting the shifted A key\n(read-key)  \226\134\146 10  ; after hitting [enter] on Linux\n(read-key)  \226\134\146 13  ; after hitting [enter] on Win32\n\n(while (!= (set 'c (read-key)) 1) (println c))\n\nThe last example can be used to check return sequences from navigation and\nfunction keys. To break out of the loop, press Ctrl-A.\n\nNote that read-key will only work when newLISP is running in a Unix shell or\nWin32 command shell. It will not work in the Java based newLISP-GS or Tcl/Tk\nbased newLISP-Tk frontend. It will also not work when executed by newLISP Unix\nshared library or newLISP Win32 DLL (Dynamic Link Library).")

(set '_read-line "{read-line\n\nsyntax: (read-line [int-file])\n\nReads from the current I/O device a string delimited by a line-feed character\n(ASCII 10). There is no limit to the length of the string that can be read. The\nline-feed character is not part of the returned string. The line always breaks\non a line-feed, which is then swallowed. A line breaks on a carriage return\n(ASCII 13) only if followed by a line-feed, in which case both characters are\ndiscarded. A carriage return alone only breaks and is swallowed if it is the\nlast character in the stream.\n\nBy default, the current device is the keyboard (device 0). Use the built-in\nfunction device to specify a different I/O device (e.g., a file). Optionally, a\nfile handle can be specified in the int-file obtained from a previous open\nstatement.\n\nThe last buffer contents from a read-line operation can be retrieved using\ncurrent-line.\n\nWhen read-line is reading from a file or from stdin in a CGI program or pipe,\nit will return nil when input is exhausted.\n\nWhen using read-line on stdin, line length is limited to 2048 characters and\nperformance is much faster.\n\n(print \"Enter a num:\")\n(set 'num (int (read-line)))\n\n(set 'in-file (open \"afile.dat\" \"read\"))\n(while (read-line in-file)\n        (write-line))\n(close in-file)\n\nThe first example reads input from the keyboard and converts it to a number. In\nthe second example, a file is read line-by-line and displayed on the screen.\nThe write-line statement takes advantage of the fact that the result from the\nlast read-line operation is stored in a system internal buffer. When write-line\nis used without argument, it writes the contents of the last read-line buffer\nto the screen.\n\nSee also the current-line function for retrieving this buffer.")

(set '_read-utf8 "{read-utf8\n\nsyntax: (read-utf8 int-file)\n\nReads an UTF-8 character from a file specified by the file handle in int-file.\nThe file handle is obtained from a previous open operation. Each read-utf8\nadvances the file pointer by the number of bytes contained in the UTF-8\ncharacter. Once the end of the file is reached, nil is returned.\n\nThe function returns an integer value which can be converted to a displayable\nUTF-8 character string using the char function.\n\n(set 'fle (open \"utf8text.txt\" \"read\"))\n(while (setq chr (read-utf8 fle))\n        (print (char chr)))\n\nThe example reads a file containing UTF-8 encoded text and displays it to the\nterminal screen.")

(set '_reader-event [text]{reader-event

syntax: (reader-event [sym-event-handler | func-event-handler])
syntax: (reader-event 'nil)

An event handler can be specified to hook between newLISP's reader, translation
and evaluation process. The function specified in sym-event-handler or
func-event-handler gets called after newLISP translates an expression and
before evaluating it. The event handler can do transformation on the expression
before it gets evaluated.

Specifying a quoted nil for the event will disable it.

The following one-liner reader-event could be used to enhance the interactive
shell with a tracer:

>(reader-event (lambda (ex) (print " => " ex)))
$reader-event
> (+ 1 2 3)
 => (+ 1 2 3)
6
>

The expression intercepted passes through unchanged, but output is enhanced.

The following example shows the core of a simple macro rewrite pre-processor. A
full version is installed in the standard location as module file macro.lsp.

(context 'macro)

; initialize macro list
(setf macro-list '())

; registers a macro
(define-macro (macro:macro callp body)
    (push (list (first callp) '*) macro-list -1)
    (eval (expand '(define-macro callp (expand 'body)) 'callp 'body))
)

; the event handler translating expressions
(define (rewrite expr)
    (if (list? expr)
        (dolist (pattern macro-list)
            (if (match pattern expr)
                (setf expr (eval expr))
                (set-ref-all pattern expr (eval $it) match)) )
    )
    expr
)

; register event handler
(reader-event rewrite)

(context MAIN)

The reader event function will be called after each reading of an s-expression
by the load or eval-string function.

Register a function macro for pre-processing:

(macro (square X) (pow X 2)) ; must use uppercase vars

; use the macro
(square 3) → 9

After registering the macro square newLISP will expand each occurrence of a
(square ...) to a (pow ... 2) expression.

Note, that variables in the macro definition must be upper-case. The macro
registering function uses a syntax form of expand working only on upper-case
variables.

For a more detailed description see the documentation for macro.lsp.[/text])

(set '_real-path "{real-path\n\nsyntax: (real-path [str-path])\n\nReturns the full path from the relative file path given in str-path. If a path\nis not given, \".\" (the current directory) is assumed.\n\n(real-path)  \226\134\146 \"/usr/home/fred\"  ; current directory\n(real-path \"./somefile.txt\")\n\226\134\146 \"/usr/home/fred/somefile.txt\"\n\nThe output length is limited by the OS's maximum allowed path length. If\nreal-path fails (e.g., because of a nonexistent path), nil is returned.")

(set '_receive "{receive !\n\nsyntax: (receive int-pid sym-message)\nsyntax: (receive)\n\nIn the first syntax, the function is used for message exchange between child\nprocesses launched with spawn and their parent process. The message received\nreplaces the contents in sym-message.\n\nThe function reads one message from the receiver queue of int-pid for each\ninvocation. When the queue is empty, nil is returned.\n\n; sending process\n(send spid \"hello\")  \226\134\146 true\n\n; receiving process\n(receive pid msg)    \226\134\146 true\nmsg                  \226\134\146 \"hello\"\n\nTo make receive blocking and wait for arriving messages, use the following\nform:\n\n; wait until a message can be read\n(until (receive pid msg))\n\nThe function will loop until a message can be read from the queue.\n\nIn the second syntax, the function returns a list of all child processes with\npending messages for the parent process:\n\n; read pending messages from child processes\n(dolist (pid (receive))\n    (receive pid msg)\n    (println \"received message: \" msg \" from:\" pid)\n)\n\nThe list of child process IDs returned by (receive) only contains PIDs of\nprocesses which have unread messages in their send queues. The (receive pid\nmsg) statement now can be issued non-blocking, because it always is guarenteed\nto find a pending message in a child's message queue.\n\nThe receive function is not available on Win32.\n\nFor a more detailed discussion of this function and examples, see the send\nfunction.")

(set '_ref [text]{ref

syntax: (ref exp-key list [func-compare [true]])

ref searches for the key expression exp-key in list and returns a list of
integer indices or an empty list if exp-key cannot be found. ref can work
together with push and pop, both of which can also take lists of indices.

By default, ref checks if expressions are equal. With func-compare, more
complex comparison functions can be used. The comparison function can be a
previously defined function. Note that this function always takes two
arguments, even if only the second argument is used inside the function.

When the optional true parameter is present, the element found is returned
instead of the index vector.

; get index vectors for list elements

(set 'pList '(a b (c d (x) e)))

(ref 'x pList)    → (2 2 0)

(ref '(x) pList)   → (2 2)

; the key expression is in a variable

(set 'p '(c d (x) e))

(ref p pList p)     → (2)

; indexing using the vector returned from ref

(set 'v (ref '(x) pList)) → (2 2)

(pList v) → (x)

; if nothing is found, nil is returned

(ref 'foo plist)  → nil

; not specifying a comparison functor assumes =

(set 'L '(a b (c d (e) f)))

(ref 'e L)      → (2 2 0)
(ref 'e L =)    → (2 2 0)

; a is the first symbol where e is greater

(ref 'e L >)  → (0)

; return the element instead of the index

(ref 'e L > true)  → a

; use an anonymous comparison function

(ref 'e L (fn (x y) (or (= x y) (= y 'd))))      → (2 1)

(ref 'e L (fn (x y) (or (= x y) (= y 'd))) true) → d

The following example shows the use of match and unify to formulate searches
that are as powerful as regular expressions are for strings:

(set 'L '((l 3) (a 12) (k 5) (a 10) (z 22)))

; use match as a comparison function

(ref '(a ?) L match) → (1)

; use unify as a comparison function

(set 'L '( ((a b) (c d)) ((e e) (f g)) ))

(ref '(X X) L unify)      → (1 0)

(ref '(X g) L unify)      → (1 1)

(ref '(X g) L unify true) → (f g)

The '(X X) pattern with unify searches for a list pair where the two elements
are equal. The unify pattern '(X g) searches for a list pair with the symbol g
as the second member. The patterns are quoted to protect them from evaluation.

Pass the list as a default functor:

(set 'C:C '(a b (c d) e f))

(ref 'd C)  → (2 1)

This is suitable when passing lists by reference using a context. See also the
chapter Passing data by reference.

See also the ref-all function, which searches for all occurrences of a key
expression in a nested list.[/text])

(set '_ref-all [text]{ref-all

syntax: (ref-all exp-key list [func-compare [true]])

Works similarly to ref, but returns a list of all index vectors found for
exp-key in list.

When the optional true parameter is present, the elements found is returned of
the index vectors.

By default, ref-all checks if expressions are equal. With func-compare, more
complex comparison functions can be used.

(set 'L '(a b c (d a f (a h a)) (k a (m n a) (x))))

(ref-all 'a L) → ((0) (3 1) (3 3 0) (3 3 2) (4 1) (4 2 2))

; the index vector returned by ref-all can be used to index the list

(L '(3 1)) → a

; mapped implicit indexing of L

(map 'L (ref-all 'a L)) → (a a a a a a)

; with comparison operator

(set 'L '(a b c (d f (h l a)) (k a (m n) (x))))

; not specifying a comparison functor assumes =

(ref-all 'c L)       → ((2))
(ref-all 'c L =)     → ((2))

; look for all elements where c is greater

(ref-all 'c L >)       → ((0) (1) (3 2 2) (4 1))
(ref-all 'c L > true)  → (a b a a)


; use an anonymous function to compare

(ref-all 'a L (fn (x y) (or (= x y) (= y 'k))))
→ ((0) (3 2 2) (4 0) (4 1))

; the key is nil because the comparison function only looks at the second argument

(ref-all nil L (fn (x y) (> (length y) 2)))
→ ((3) (3 2) (4))

; define the comparison functions first

(define (is-long? x y) (> (length y) 2)) ; the x gets occupied by 'nil

(ref-all nil L is-long?)    →  ((3) (3 2) (4))

(define (is-it-or-d x y) (or (= x y) (= y 'd)))

(set 'L '(a b (c d (e) f)) )

(ref-all 'e L is-it-or-d)  → ((2 1) (2 2 0))

The comparison function can be a previously defined function. Note that the
comparison function always takes two arguments, even if only the second
argument is used inside the function (as in the example using is-long?).

Using the match and unify functions, list searches can be formulated that are
as powerful as regular expression searches are for strings.

(set 'L '((l 3) (a 12) (k 5) (a 10) (z 22)) )

; look for all pairs staring with the symbol a

(ref-all '(a ?) L match)      → ((1) (3))
(ref-all '(a ?) L match true) → ((a 12) (a 10))

; look for all pairs where elements are equal

(set 'L '( ((a b) (c d)) ((e e) (f g)) ((z) (z))))

(ref-all '(X X) L unify)      → ((1 0) (2))
(ref-all '(X X) L unify true) → ((e e) ((z) (z)))

; look for all pairs where the second element is the symbol g

(set 'L '( ((x y z) g) ((a b) (c d)) ((e e) (f g)) ))

(ref-all '(X g) L unify)      → ((0) (2 1))
(ref-all '(X g) L unify true) → (((x y z) g) (f g))

See also the ref function.[/text])

(set '_regex-comp "{regex-comp\n\nsyntax: (regex-comp str-pattern [int-option])\n\nnewLISP automatically compiles regular expression patterns and caches the last\ncompilation to speed up repetitive pattern searches. If patterns change from\none to the next, but are repeated over and over again, then the caching of the\nlast pattern is not sufficient. regex-comp can be used to pre-compile\nrepetitive patterns to speed up regular expression searches:\n\n; slower without pre-compilation\n\n(dolist (line page)\n        (replace pattern-str1 line repl1 0)\n        (replace pattern-str2 line repl2 512)\n)\n\n; fast with pre-compilation and option 0x10000\n\n(set 'p1 (regex-comp pattern-str1))\n(set 'p2 (regex-comp pattern-str2 512))\n\n(dolist (line page)\n        (replace p1 line repl1 0x10000)\n        (replace p2 line repl2 0x10000)\n)\n\nWhen using pre-compiled patterns in any of the functions using regular\nexpressions, the option number is set to 0x10000 to signal that pre-compiled\npatterns are used. Normal pattern options are specified during pre-compilation\nwith regex-comp . The 0x10000 option can only be combined with 0x8000, the\noption used to specify that only one replacement should be made when using\nreplace.")

(set '_remove-dir "{remove-dir\n\nsyntax: (remove-dir str-path)\n\nRemoves the directory whose path name is specified in str-path. The directory\nmust be empty for remove-dir to succeed. Returns nil on failure.\n\n(remove-dir \"temp\")\n\nRemoves the directory temp in the current directory.")

(set '_rename-file "{rename-file\n\nsyntax: (rename-file str-path-old str-path-new)\n\nRenames a file or directory entry given in the path name str-path-old to the\nname given in str-path-new. Returns nil or true depending on the operation's\nsuccess.\n\n(rename-file \"data.lisp\" \"data.backup\")")

(set '_replace [text]{replace !

syntax: (replace exp-key list exp-replacement [func-compare])
syntax: (replace exp list)

syntax: (replace str-key str-data exp-replacement)
syntax: (replace str-pattern str-data exp-replacement int-regex-option)

List replacement

If the second argument is a list, replace replaces all elements in the list
list that are equal to the expression in exp-key. The element is replaced with
exp-replacement. Note that replace is destructive. It changes the list passed
to it and returns the changed list. The number of replacements made is
contained in the system variable $0 when the function returns. During
executions of the replacement expression, the system variable $0 and the
anaphoric system variable $it are set to the expression to be replaced.

Optionally, func-compare can specify a comparison operator or user-defined
function. By default, func-compare is the = (equals sign).

;; list replacement

(set 'aList '(a b c d e a b c d))

(replace 'b aList 'B)  → (a B c d e a B c d)
aList  → (a B c d e a B c d)
$0     → 2  ; number of replacements

;; list replacement with special compare functor/function

; replace all numbers where 10 < number
(set 'L '(1 4 22 5 6 89 2 3 24))

(replace 10 L 10 <) → (1 4 10 5 6 10 2 3 10)

; same as:

(replace 10 L 10 (fn (x y) (< x y))) → (1 4 10 5 6 10 2 3 10)

; change name-string to symbol, x is ignored as nil

(set 'AL '((john 5 6 4) ("mary" 3 4 7) (bob 4 2 7 9) ("jane" 3)))

(replace nil AL (cons (sym ($it 0)) (rest $it))
                (fn (x y) (string? (y 0))))
→ ((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3))

Using the match and unify functions, list searches can be formulated that are
as powerful as regular expression string searches:

; calculate the sum in all associations with 'mary

(set 'AL '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))

(replace '(mary *)  AL (list 'mary (apply + (rest $it))) match)
→ ((john 5 6 4) (mary 14) (bob 4 2 7 9) (jane 3))

; make sum in all expressions

(set 'AL '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))

(replace '(*) AL (list ($0 0) (apply + (rest $it))) match)
→ ((john 15) (mary 14) (bob 22) (jane 3))

; using unify
(replace '(X X) '((3 10) (2 5) (4 4) (6 7) (8 8)) (list ($it 0) 'double ($it 1)) unify)
→ ((3 10) (2 5) (4 double 4) (6 7) (8 double 8))


List removal

The last form of replace has only two arguments: the expression exp and list.
This form removes all exps found in list.

;; removing elements from a list

(set 'lst '(a b a a c d a f g))
(replace 'a lst)  → (b c d f g)
lst               → (b c d f g)

$0  → 4

String replacement without regular expression

If all arguments are strings, replace replaces all occurrences of str-key in
str-data with the evaluated exp-replacement, returning the changed string. The
expression in exp-replacement is evaluated for every replacement. The number of
replacements made is contained in the system variable $0. This form of replace
can also process binary 0s (zeros).

;; string replacement
(set 'str "this isa sentence")
(replace "isa" str "is a")  → "this is a sentence"

Regular expression replacement

The presence of a fourth parameter indicates that a regular expression search
should be performed with a regular expression pattern specified in str-pattern
and an option number specified in int-option (e.g., 1 (one) for
case-insensitive searching or 0 (zero) for a standard Perl Compatible Regular
Expression (PCRE) search). See regex above for details.

By default, replace replaces all occurrences of a search string even if a
beginning-of-line specification is included in the search pattern. After each
replace, a new search is started at a new position in str-data. Setting the
option bit to 0x8000 in int-option will force replace to replace only the first
occurrence. The changed string is returned.

replace with regular expressions also sets the internal variables $0, $1, and
$2— with the contents of the expressions and subexpressions found. The
anaphoric system variable $it is set to the same value as $0. These can be used
to perform replacements that depend on the content found during replacement.
The symbols $it, $0, $1, and $2— can be used in expressions just like any other
symbols. If the replacement expression evaluates to something other than a
string, no replacement is made. As an alternative, the contents of these
variables can also be accessed by using ($ 0), ($ 1), ($ 2), and so forth. This
method allows indexed access (e.g., ($ i), where i is an integer).

After all replacements are made, the number of replacements is contained in the
system variable $0.

;; using the option parameter to employ regular expressions

(set 'str "ZZZZZxZZZZyy")     → "ZZZZZxZZZZyy"
(replace "[x|y]" str "PP" 0)  → "ZZZZZPPZZZZPPPP"
str                           → "ZZZZZPPZZZZPPPP"

;; using system variables for dynamic replacement

(set 'str "---axb---ayb---")
(replace "(a)(.)(b)" str (append $3 $2 $1) 0)
→ "---bxa---bya---"

str  → "---bxa---bya---"

;; using the 'replace once' option bit 0x8000

(replace "a" "aaa" "X" 0)  → "XXX"

(replace "a" "aaa" "X" 0x8000)  → "Xaa"

;; URL translation of hex codes with dynamic replacement

(set 'str "xxx%41xxx%42")
(replace "%([0-9A-F][0-9A-F])" str
               (char (int (append "0x" $1))) 1)

str  → "xxxAxxxB"

$0   → 2

The setf function together with nth, first or last can also be used to change
elements in a list.

See directory, find, find-all, parse, regex, and search for other functions
using regular expressions.[/text])

(set '_reset "{reset\n\nsyntax: (reset)\nsyntax: (reset true)\n\nIn the first syntax, reset returns to the top level of evaluation, switches the\ntrace mode off, and switches to the MAIN context/namespace. reset restores the\ntop-level variable environment using the saved variable environments on the\nstack. It also fires an error \"user reset - no error\". This behavior can be\nused when writing error handlers.\n\nreset may return memory that was claimed by newLISP to the operating system.\nreset walks through the entire cell space, which may take a few seconds in a\nheavily loaded system.\n\nreset occurs automatically after an error condition.\n\nIn the second syntax, reset will stop the current process and start a new clean\nnewLISP process with the same command-line parameters. This mode will only work\nwhen newLISP was started using its full path-name, e.g. /usr/bin/newlisp\ninstead of only newlisp. This mode is not available on Win32.")

(set '_rest "{rest utf8\n\nsyntax: (rest list)\nsyntax: (rest array)\nsyntax: (rest str)\n\nReturns all of the items in a list or a string, except for the first. rest is\nequivalent to cdr or tail in other Lisp dialects.\n\n(rest '(1 2 3 4))            \226\134\146 (2 3 4)\n(rest '((a b) c d))          \226\134\146 (c d)\n(set 'aList '(a b c d e))    \226\134\146 (a b c d e)\n(rest aList)                 \226\134\146 (b c d e)\n(first (rest aList))         \226\134\146 b\n(rest (rest aList))          \226\134\146 (d e)\n(rest (first '((a b) c d)))  \226\134\146 (b)\n\n(set 'A (array 2 3 (sequence 1 6)))\n\226\134\146 ((1 2) (3 4) (5 6))\n\n(rest A)  \226\134\146 ((3 4) (5 6))\n\n(rest '()) \226\134\146 ()\n\nIn the second version, rest returns all but the first character of the string\nstr in a string.\n\n(rest \"newLISP\")          \226\134\146 \"ewLISP\"\n(first (rest \"newLISP\"))  \226\134\146 \"e\"\n\nSee also the first and last functions.\n\nNote that an implicit rest is available for lists. See the chapter Implicit\nrest and slice.\n\nNote that rest works on character boundaries rather than byte boundaries when\nthe UTF-8\226\128\147enabled version of newLISP is used.")

(set '_reverse "{reverse !\n\nsyntax: (reverse list)\nsyntax: (reverse string)\n\nIn the first form, reverse reverses and returns the list. Note that reverse is\ndestructive and changes the original list.\n\n(set 'l '(1 2 3 4 5 6 7 8 9))\n\n(reverse l)  \226\134\146 (9 8 7 6 5 4 3 2 1)\nl            \226\134\146 (9 8 7 6 5 4 3 2 1)\n\nIn the second form, reverse is used to reverse the order of characters in a\nstring.\n\n(set 'str \"newLISP\")\n\n(reverse str)  \226\134\146 \"PSILwen\"\nstr            \226\134\146 \"PSILwen\"\n\nSee also the sort function.")

(set '_rotate "{rotate !\n\nsyntax: (rotate list [int-count])\nsyntax: (rotate str [int-count])\n\nRotates and returns the list or string in str. A count can be optionally\nspecified in int-count to rotate more than one position. If int-count is\npositive, the rotation is to the right; if int-count is negative, the rotation\nis to the left. If no int-count is specified, rotate rotates 1 to the right.\nrotate is a destructive function that changes the contents of the original list\nor string.\n\n(set 'l '(1 2 3 4 5 6 7 8 9))\n\n(rotate l)    \226\134\146 (9 1 2 3 4 5 6 7 8)\n(rotate l 2)  \226\134\146 (7 8 9 1 2 3 4 5 6)\n\nl  \226\134\146 (7 8 9 1 2 3 4 5 6)\n\n(rotate l -3)  \226\134\146 (1 2 3 4 5 6 7 8 9)\n\n(set 'str \"newLISP\")\n\n(rotate str)     \226\134\146 \"PnewLIS\"\n(rotate str 3)   \226\134\146 \"LISPnew\"\n(rotate str -4)  \226\134\146 \"newLISP\"\n\nWhen working on a string, rotate works on byte boundaries rather than character\nboundaries.")

(set '_round "{round\n\nsyntax: (round number [int-digits])\n\nRounds the number in number to the number of digits given in int-digits. When\ndecimals are being rounded, int-digits is negative. It is positive when the\ninteger part of a number is being rounded.\n\nIf int-digits is omitted, the function rounds to 0 decimal digits.\n\n(round 123.49 2)    \226\134\146 100\n(round 123.49 1)    \226\134\146 120\n(round 123.49 0)    \226\134\146 123\n(round 123.49)      \226\134\146 123\n(round 123.49 -1)   \226\134\146 123.5\n(round 123.49 -2)   \226\134\146 123.49\n\nNote that rounding for display purposes is better accomplished using format.")

(set '_save [text]{save

syntax: (save str-file)
syntax: (save str-file sym-1 [sym-2 ... ])

In the first syntax, the save function writes the contents of the newLISP
workspace (in textual form) to the file str-file. save is the inverse function
of load. Using load on files created with save causes newLISP to return to the
same state as when save was originally invoked. System symbols starting with
the $ character (e.g., $0 from regular expressions or $main-args from the
command-line), symbols of built-in functions and symbols containing nil are not
saved.

In the second syntax, symbols can be supplied as arguments. If sym-n is
supplied, only the definition of that symbol is saved. If sym-n evaluates to a
context, all symbols in that context are saved. More than one symbol can be
specified, and symbols and context symbols can be mixed. When contexts are
saved, system variables and symbols starting with the $ character are not
saved. Specifying system symbols explicitly causes them to be saved.

Each symbol is saved by means of a set statement or—if the symbol contains a
lambda or lambda-macro function—by means of define or define-macro statements.

save returns true on completion.

(save "save.lsp")

(save "/home/myself/myfunc.LSP" 'my-func)
(save "file:///home/myself/myfunc.LSP" 'my-func)

(save "http://asite.com:8080//home/myself/myfunc.LSP" 'my-func)

(save "mycontext.lsp" 'mycontext)

;; multiple args
(save "stuff.lsp" 'aContext 'myFunc '$main-args 'Acontext)

Because all context symbols are part of the context MAIN, saving MAIN saves all
contexts.

Saving to a URL will cause an HTTP PUT request to be sent to the URL. In this
mode, save can also be used to push program source to remote newLISP server
nodes. Note that a double backslash is required when path names are specified
relative to the root directory. save in HTTP mode will observe a 60-second
timeout.

Symbols made using sym that are incompatible with the normal syntax rules for
symbols are serialized using a sym statement instead of a set statement.

save serializes contexts and symbols as if the current context is MAIN.
Regardless of the current context, save will always generate the same output.

See also the functions load (the inverse operation of save) and source, which
saves symbols and contexts to a string instead of a file.[/text])

(set '_search "{search\n\nsyntax: (search int-file str-search [bool-flag [int-options]])\n\nSearches a file specified by its handle in int-file for a string in str-search.\nint-file can be obtained from a previous open file. After the search, the file\npointer is positioned at the beginning or the end of the searched string or at\nthe end of the file if nothing is found.\n\nBy default, the file pointer is positioned at the beginning of the searched\nstring. If bool-flag evaluates to true, then the file pointer is positioned at\nthe end of the searched string.\n\nIn int-options, the options flags can be specified to perform a PCRE regular\nexpression search. See the function regex for details. If int-options is not\nspecified a faster, plain string search is performed. search returns the new\nfile position or nil if nothing is found.\n\nWhen using the regular expression options flag, patterns found are stored in\nthe system variables $0 to $15.\n\n(set 'file (open \"init.lsp\" \"read\"))\n(search file \"define\")\n(print (read-line file) \"\\n\")\n(close file)\n\n(set 'file (open \"program.c\" \"r\"))\n(while (search file \"#define (.*)\" true 0) (println $1))\n(close file)\n\nThe file init.lsp is opened and searched for the string define and the line in\nwhich the string occurs is printed.\n\nThe second example looks for all lines in the file program.c which start with\nthe string #define and prints the rest of the line after the string \"#define \".\n\nFor other functions using regular expressions, see directory, find, find-all,\nparse, regex, and replace.")

(set '_seed "{seed\n\nsyntax: (seed int-seed)\n\nSeeds the internal random generator that generates numbers for amb, normal,\nrand, and random with the number specified in int-seed. Note that the random\ngenerator used in newLISP is the C-library function rand(). All randomizing\nfunctions in newLISP are based on this function.\n\nNote that the maximum value for int-seed is limited to 16 or 32 bits, depending\non the operating system used. Internally, only the 32 least significant bits\nare passed to the random seed function of the OS.\n\n(seed 12345)\n\n(seed (time-of-day))\n\nAfter using seed with the same number, the random generator starts the same\nsequence of numbers. This facilitates debugging when randomized data are\ninvolved. Using seed, the same random sequences can be generated over and over\nagain.\n\nThe second example is useful for guaranteeing a different seed any time the\nprogram starts.")

(set '_seek "{seek\n\nsyntax: (seek int-file [int-position])\n\nSets the file pointer to the new position int-position in the file specified by\nint-file.The new position is expressed as an offset from the beginning of the\nfile, 0 (zero) meaning the beginning of the file. If no int-position is\nspecified, seek returns the current position in the file. If int-file is 0\n(zero), on BSD, seek will return the number of characters printed to STDOUT,\nand on Linux and Win32, it will return -1. On failure, seek returns nil. When\nint-position is set to -1, seek sets the file pointer to the end of the file.\n\nseek can set the file position past the current end of the file. Subsequent\nwriting to this position will extend the file and fill unused positions with\nzero's. The blocks of zeros are not actually allocated on disk, so the file\ntakes up less space and is called a sparse file.\n\n(set 'file (open \"myfile\" \"read\"))  \226\134\146 5\n(seek file 100)                     \226\134\146 100\n(seek file)                         \226\134\146 100\n\n(open \"newlisp_manual.html\" \"read\")\n(seek file -1)  ; seek to EOF\n\226\134\146 593816\n\n(set 'fle (open \"large-file\" \"read\")\n(seek file 30000000000)  \226\134\146 30000000000\n\nnewLISP supports file position numbers up to 9,223,372,036,854,775,807.")

(set '_select "{select utf8\n\nsyntax: (select list list-selection)\nsyntax: (select list [int-index_i ... ])\n\nsyntax: (select string list-selection)\nsyntax: (select string [int-index_i ... ])\n\nIn the first two forms, select picks one or more elements from list using one\nor more indices specified in list-selection or the int-index_i.\n\n(set 'lst '(a b c d e f g))\n\n(select lst '(0 3 2 5 3))  \226\134\146 (a d c f d)\n\n(select lst '(-2 -1 0))  \226\134\146 (f g a)\n\n(select lst -2 -1 0)  \226\134\146 (f g a)\n\nIn the second two forms, select picks one or more characters from string using\none or more indices specified in list-selection or the int-index_i.\n\n(set 'str \"abcdefg\")\n\n(select str '(0 3 2 5 3))  \226\134\146 \"adcfd\"\n\n(select str '(-2 -1 0))  \226\134\146 \"fga\"\n\n(select str -2 -1 0)  \226\134\146 \"fga\"\n\nSelected elements can be repeated and do not have to appear in order, although\nthis speeds up processing. The order in list-selection or int-index_i can be\nchanged to rearrange elements.")

(set '_self "{self\n\nsyntax: (self [int-index ... ])\n\nThe function self accesses the target object of a FOOP method. One or more\nint-index are used to access the object members.\n\nObjects referenced with self are mutable:\n\n(new Class 'Circle)\n\n(define (Circle:move dx dy)\n        (inc (self 1) dx)\n        (inc (self 2) dy))\n\n(set 'aCircle (Circle 1 2 3))\n(:move aCircle 10 20)\n\naCircle \226\134\146 (Circle 11 22 3)\n\n; objects can be anonymous\n(set 'circles '((Circle 1 2 3) (Circle 4 5 6)))\n\n(:move (circles 0) 10 20)\n(:move (circles 1) 10 20)\n\ncircles \226\134\146 ((Circle 11 22 3) (Circle 14 25 6))\n\nSee also the chapter about programming with FOOP: Functional object-oriented\nprogramming in newLISP")

(set '_semaphore [text]{semaphore

syntax: (semaphore)
syntax: (semaphore int-id)
syntax: (semaphore int-id int-wait)
syntax: (semaphore int-id int-signal)
syntax: (semaphore int-id 0)

A semaphore is an interprocess synchronization object that maintains a count
between 0 (zero) and some maximum value. Useful in controlling access to a
shared resource, a semaphore is set to signaled when its count is greater than
zero and to non-signaled when its count is zero.

A semaphore is created using the first syntax. This returns the semaphore ID,
an integer used subsequently as int-id when the semaphore function is called.
Initially, the semaphore has a value of zero, which represents the non-signaled
state.

If calling semaphore with a negative value in int-wait causes it to be
decremented below zero, the function call will block until another process
signals the semaphore with a positive value in int-signal. Calls to the
semaphore with int-wait or int-signal effectively try to increment or decrement
the semaphore value by a positive or negative value specified in int-signal or
int-wait. Because the value of a semaphore must never fall below zero, the
function call will block when this is attempted (i.e., a semaphore with a value
of zero will block until another process increases the value with a positive
int-signal).

The second syntax is used to inquire about the value of a semaphore by calling
semaphore with the int-id only. This form is not available on Win32.

Supplying 0 (zero) as the last argument will release system resources for the
semaphore, which then becomes unavailable. Any pending waits on this semaphore
in other child processes will be released.

On Win32, only parent and child processes can share a semaphore. On Linux/Unix,
independent processes can share a semaphore.

On failure the semaphore function returns nil. sys-error can be used to
retrieve the error number and text from the underlying operating system.

The following code examples summarize the different syntax forms:

;; init semaphores
(semaphore)

;; assign a semaphore to sid
(set 'sid (semaphore))

;; inquire the state of a semaphore (not on Win32)
(semaphore sid)

;; put sid semaphore in wait state (-1)
(semaphore sid -1)

;; run sid semaphore previously put in wait (always 1)
(semaphore sid 1)

;; run sid semaphore with X times a skip (backward or forward) on the function
(semaphore sid X)

;; release sid semaphore system-wide (always 0)
(semaphore sid 0)

The following example shows semaphores controlling a child process:

;; counter process output in bold

(define (counter n)
        (println "counter started")
        (dotimes (x n)
                (semaphore sid -1)
                (println x)))

;; hit extra <enter> to make the prompt come back
;; after output to the console from the counter process

> (set 'sid (semaphore))

> (semaphore sid)
0

> (fork (counter 100))

counter started
> (semaphore sid 1)
0
> (semaphore sid 3)
1
2
3
> (semaphore sid 2)
4

5
> _

After the semaphore is acquired in sid, it has a value of 0 (the non-signaled
state). When starting the process counter, the semaphore will block after the
initial start message and will wait in the semaphore call. The -1 is trying to
decrement the semaphore, which is not possible because its value is already
zero. In the interactive, main parent process, the semaphore is signaled by
raising its value by 1. This unblocks the semaphore call in the counter
process, which can now decrement the semaphore from 1 to 0 and execute the
print statement. When the semaphore call is reached again, it will block
because the semaphore is already in the wait (0) state.

Subsequent calls to semaphore with numbers greater than 1 give the counter
process an opportunity to decrement the semaphore several times before
blocking.

More than one process can participate in controlling the semaphore, just as
more than one semaphore can be created. The maximum number of semaphores is
controlled by a system-wide kernel setting on Unix-like operating systems.

Use the fork function to start a new process and the share function to share
information between processes. For a more comprehensive example of using
semaphore to synchronize processes, see the file prodcons.lsp example in the
examples directory in the source distribution, as well as the examples and
modules distributed with newLISP.[/text])

(set '_send [text]{send

syntax: (send int-pid exp)
syntax: (send)

The send function enables communication between parent and child processes
started with spawn. Parent processes can send and receive messages to and from
their child processes and child processes can send and receive messages to and
from their parent process. A proxy technique – shown further down – is employed
to communicate between child process peers. send and receive do not require
locks or semaphores. They work on dual send and receive message queues.

Processes started using fork or process can not use send and receive message
functions. Instead they should use either share with semaphore or pipe to
communicate.

The send function is not available on Win32.

In the first syntax send is used to send a message from a parent to a child
process or a child to a parent process.

The second syntax is only used by parent processes to get a list of all child
processes ready to accept message from the parent in their receive queues. If a
child's receive queue is full, it will not be part of the list returned by the
(send) statement.

The content of a message may be any newLISP expression either atomic or list
expressions: boolean constants nil and true, integers, floating point numbers
or strings, or any list expression in valid newLISP syntax. The size of a
message is unlimited.

The exp parameter specifies the data to be sent to the recipient in int-pid.
The recipient can be either a spawned child process of the current process or
the parent process. If a message queue is full, it can be read from the
receiving end, but a send issued on the other side of the queue will fail and
return nil.

; child process dispatching message to parent

(set 'ppid (sys-info -4)) ; get parent pid

(send ppid "hello") ; send message

The targeted recipient of the message is the parent process:

; parent process receiving message from child

(receive child-pid msg) → true
msg                     → "hello"

When the send queue is full, send will return nil until enough message content
is read on the receiving side of the queue and the queue is ready to accept new
messages from send statements.

Using the until looping function, the message statements can be repeated until
they return a value not nil. This way, non-blocking send and receive can be
made blocking until they succeed:

; blocking sender
(until (send pid msg)) ; true after message is queued up

; blocking receiver
(until (receive pid msg)) ; true after message could be read

The sender statement blocks until the message could be deposited in the
recipients queue.

The receive statement blocks until a new message can be fetched from the queue.

As the until statements in this example lack body expressions, the last value
of the evaluated conditional expression is the return value of the until loop.

Blocking message exchange

The following code shows how a recipient can listen for incoming messages, and
in turn how a sender can retry to deposit a message into a queue. The example
shows 5 child processes constantly delivering status data to a parent process
which will display the data. After three data sets have been read, the parent
will abort all child processes and exit:

#!/usr/bin/newlisp

; child process transmits random numbers
(define (child-process)
    (set 'ppid (sys-info -4)) ; get parent pid
    (while true
        (until (send ppid (rand 100))))
)

; parent starts 5  child processes, listens and displays
; the true flag is specified to enable send/receive

(dotimes (i 5) (spawn 'result (child-process) true))

(for (i 1 3)
    (dolist (cpid (sync)) ; iterate thru pending child PIDs
        (until (receive cpid msg))
        (print "pid:" cpid "->" (format "%-2d  " msg)))
    (println)
)

(abort) ; cancel child-processes
(exit)

Running above example produces the following output:

pid:53181->47  pid:53180->61  pid:53179->75  pid:53178->39  pid:53177->3   
pid:53181->59  pid:53180->12  pid:53179->20  pid:53178->77  pid:53177->47  
pid:53181->6   pid:53180->56  pid:53179->96  pid:53178->78  pid:53177->18

The (sync) expression returns a list of all child PIDs, and (until (receive
cpid msg)) is used to force a wait until status messages are recived for each
of the child processes.

A timeout mechanism could be part of an until or while loop to stop waiting
after certain time has expired.

The examples show messages flowing from a child processes to a parent process,
in the same fashion messages could flow into the other direction from parent to
child processes. In that case the parent process would use (send) to obtain a
list of child processes with place in their message queues.

Messages containing code for evaluation

The most powerful feature of the message functions is the ability to send any
newLISP expression, which then can be evaluated by the recipient. The recipient
uses eval to evaluate the received expression. Symbols contained in the
expression are evaluated in the receivers environment.

The following example shows how a parent process acts like a message proxy. The
parent receives messages from a child process A and routes them to a second
child process with ID B. In effect this implements messages between child
process peers. The implementation relies on the fact that the recipient can
evaluate expressions contained in messages received. These expressions can be
any valid newLISP statements:

#!/usr/bin/newlisp

; sender child process of the message
(set 'A (spawn 'result
    (begin
        (dotimes (i 3)
            (set 'ppid (sys-info -4))
            /* the statement in msg will be evaluated in the proxy */
            (set 'msg '(until (send B (string "greetings from " A))))
            (until (send ppid msg)))
        (until (send ppid '(begin
            (sleep 100) ; make sure all else is printed
            (println "parent exiting ...\n")
            (set 'finished true))))) true))

; receiver child process of the message
(set 'B (spawn 'result
    (begin
        (set 'ppid (sys-info -4))
        (while true
            (until (receive ppid msg))
            (println msg)
            (unless (= msg (string "greetings from " A))
                (println "ERROR in proxy message: " msg)))) true))

(until finished (if (receive A msg) (eval msg))) ; proxy loop

(abort)
(exit)

Child process A sends three messages to B. As this cannot be done directly A
sends send statements to the parent for evaluation. The statement:

(until (send pidB (string "greetings from " A)))

will be evaluated in the environment of the parent process. Even so the
variables A and B are bound to nil in the sender process A, in the parent
process they will be bound to the correct process ID numbers.

After sending the three messages, the statement:

(set 'finished true)

is sent to the parent process. Once evaluated, it will cause the until loop to
finish.

For more details on send and receive and more examples see the Code Patterns
document.[/text])

(set '_sequence "{sequence\n\nsyntax: (sequence num-start num-end [num-step])\n\nGenerates a sequence of numbers from num-start to num-end with an optional step\nsize of num-step. When num-step is omitted, the value 1 (one) is assumed. The\ngenerated numbers are of type integer (when no optional step size is specified)\nor floating point (when the optional step size is present).\n\n(sequence 10 5)     \226\134\146 (10 9 8 7 6 5)\n(sequence 0 1 0.2)  \226\134\146 (0 0.2 0.4 0.6 0.8 1)\n(sequence 2 0 0.3)  \226\134\146 (2 1.7 1.4 1.1 0.8 0.5 0.2)\n\nNote that the step size must be a positive number, even if sequencing from a\nhigher to a lower number.\n\nUse the series function to generate geometric sequences.")

(set '_series "{series\n\nsyntax: (series num-start num-factor num-count)\nsyntax: (series exp-start func num-count)\n\nIn the first syntax, series creates a geometric sequence with num-count\nelements starting with the element in num-start. Each subsequent element is\nmultiplied by num-factor. The generated numbers are always floating point\nnumbers.\n\nWhen num-count is less than 1, then series returns an empty list.\n\n(series 2 2 5)     \226\134\146 (2 4 8 16 32)\n(series 1 1.2 6)   \226\134\146 (1 1.2 1.44 1.728 2.0736 2.48832)\n(series 10 0.9 4)  \226\134\146 (10 9 8.1 7.29)\n(series 0 0 10)    \226\134\146 (0 0 0 0 0 0 0 0 0 0)\n(series 99 1 5)    \226\134\146 (99 99 99 99 99)\n\nIn the second syntax, series uses a function specified in func to transform the\nprevious expression in to the next expression:\n\n; embed the function Phi: f(x) = 1 / (1 + x)\n; see also http://en.wikipedia.org/wiki/Golden_ratio\n\n(series 1 (fn (x) (div (add 1 x))) 20)  \226\134\146\n\n(1 0.5 0.6666666 0.6 0.625 0.6153846 0.619047 0.6176470 0.6181818\n 0.6179775 0.6180555 0.6180257 0.6180371 0.6180327 0.6180344\n 0.6180338 0.6180340 0.6180339 0.6180339 0.6180339)\n\n; pre-define the function\n\n(define (oscillate x)\n  (if (< x)\n    (+ (- x) 1)\n    (- (+ x 1)))\n)\n\n(series 1 oscillate 20)  \226\134\146\n\n(1 -2 3 -4 5 -6 7 -8 9 -10 11 -12 13 -14 15 -16 17 -18 19 -20)\n\n; any data type is accepted as a start expression\n\n(series \"a\" (fn (c) (char (inc (char c)))) 5) \226\134\146 (\"a\" \"b\" \"c\" \"d\" \"e\")\n\n; dependency of the two previous values in this fibonacci generator\n\n(let (x 1) (series x (fn (y) (+ x (swap y x))) 10))  \226\134\146\n\n(1 2 3 5 8 13 21 34 55 89)\n\n\nThe first example shows a series converging to the golden ratio, \207\134 (for any\nstarting value). The second example shows how func can be defined previously\nfor better readability of the series statement.\n\nThe series function also updates the internal list $idx index value, which can\nbe used inside func.\n\nUse the sequence function to generate arithmetic sequences.")

(set '_set "{set !\n\nsyntax: (set sym-1 exp-1 [sym-2 exp-2 ... ])\n\nEvaluates both arguments and then assigns the result of exp to the symbol found\nin sym. The set expression returns the result of the assignment. The assignment\nis performed by copying the contents of the right side into the symbol. The old\ncontents of the symbol are deleted. An error message results when trying to\nchange the contents of the symbols nil, true, or a context symbol. set can take\nmultiple argument pairs.\n\n(set 'x 123)     \226\134\146 123\n(set 'x 'y)      \226\134\146 y\n(set x \"hello\")  \226\134\146 \"hello\"\n\ny  \226\134\146 \"hello\"\n\n(set 'alist '(1 2 3))  \226\134\146 (1 2 3)\n\n\n(set 'x 1 'y \"hello\")  \226\134\146 \"hello\"  ; multiple arguments\n\nx  \226\134\146 1\ny  \226\134\146 \"hello\"\n\nThe symbol for assignment could be the result from another newLISP expression:\n\n(set 'lst '(x y z))  \226\134\146 (x y z)\n\n(set (first lst) 123)  \226\134\146 123\n\nx  \226\134\146 123\n\nSymbols can be set to lambda or lambda-macro expressions. This operation is\nequivalent to using define or define-macro.\n\n(set 'double (lambda (x) (+ x x)))\n\226\134\146 (lambda (x) (+ x x))\n\nis equivalent to:\n\n(define (double x) (+ x x))\n\226\134\146 (lambda (x) (+ x x))\n\nis equivalent to:\n\n(define double (lambda (x) (+ x x)))\n\226\134\146 (lambda (x) (+ x x))\n\nUse the constant function (which works like set) to protect the symbol from\nsubsequent alteration. Using the setq or setf function eliminates the need to\nquote the variable symbol.")

(set '_set-locale [text]{set-locale

syntax: (set-locale [str-locale [int-category]])

Reports or switches to a different locale on your operating system or platform.
When used without arguments, set-locale reports the current locale being used.
When str-locale is specified, set-locale switches to the locale with all
category options turned on (LC_ALL). Placing an empty string in str-locale
switches to the default locale used on the current platform.

set-locale returns either the current locale string and decimal point string in
a list or nil if the requested change could not be performed.

; report current locale

(set-locale)

; set default locale of your platform and country
; return value shown when executing on German MS-Windows

(set-locale "")    → ("German_Germany.1252" ",")
(add 1,234 1,234)  → 2,468

By default, newLISP – if not enabled for UTF-8 – starts up with the POSIX C
default locale. This guarantees that newLISP's behavior will be identical on
any platform locale. On UTF-8 enabled versions of newLISP the locale of the
current platform is chosen.

; after non-UTF-8 newLISP start up

(set-locale)  → ("C" ".")

In int-category, integer numbers may be specified as category options for
fine-tuning certain aspects of the locale, such as number display, date
display, and so forth. The numbers used vary from system-to-system. The options
valid on your platform can be found in the C include file locale.h. This file
defines constants like LC_ALL, LC_NUMERIC, and LC_MONETARY. When set-locale is
used without the option number, it assumes the LC_ALL option, which turns on
all options for that locale.

Note that the locale also controls the decimal separator in numbers. The
default C locale uses the decimal dot, but most others use a decimal comma.

Note that using set-locale does not change the behavior of regular expressions
in newLISP. To localize the behavior of PCRE (Perl Compatible Regular
Expressions), newLISP must be compiled with different character tables. See the
file, LOCALIZATION, in the newLISP source distribution for details.

See also the chapter Switching the locale.[/text])

(set '_set-ref "{set-ref !\n\nsyntax: (set-ref exp-key list exp-replacement [func-compare])\n\nSearches for exp-key in list and replaces the found element with\nexp-replacement. The list can be nested. The system variables $it contains the\nexpression found and can be used in exp-replacement. The function returns the\nnew modified list.\n\n(set 'data '(fruits (apples 123 44) (oranges 1 5 3)))\n\n(set-ref 'apples data 'Apples)  \226\134\146 (fruits (Apples 123 44) (oranges 1 5 3))\n\ndata \226\134\146 (fruits (Apples 123 44) (oranges 1 5 3)))\n\ndata could be the context identifier of a default function for passing lists by\nreference:\n\n(set 'db:db '(fruits (apples 123 44) (oranges 1 5 3)))\n\n(define (update ct key value)\n        (set-ref key ct value))\n\n(update db 'apples 'Apples)    \226\134\146 (fruits (Apples 123 44) (oranges 1 5 3))\n(update db 'oranges 'Oranges)  \226\134\146 (fruits (Apples 123 44) (Oranges 1 5 3))\n\ndb:db \226\134\146 (fruits (Apples 123 44) (Oranges 1 5 3))\n\nFor examples on how to use func-compare see set-ref-all\n\nFor changing all occurrences of an element in a list use set-ref-all.")

(set '_set-ref-all "{set-ref-all !\n\nsyntax: (set-ref-all exp-key list exp-replacement [func-compare])\n\nSearches for exp-key in list and replaces each instance of the found element\nwith exp-replacement. The list can be nested. The system variable $it contains\nthe expression found and can be used in exp-replacement. The function returns\nthe new modified list.\n\n(set 'data '((monday (apples 20 30) (oranges 2 4 9)) (tuesday (apples 5) (oranges 32 1))))\n\n(set-ref-all 'apples data \"Apples\")\n \226\134\146 ((monday (\"Apples\" 20 30) (oranges 2 4 9)) (tuesday (\"Apples\" 5) (oranges 32 1)))\n\nUsing the default functor in the (list key) pattern allows the list to be\npassed by reference to a user-defined function containing a set-ref-all\nstatement. This would result in less memory usage and higher speeds in when\ndoing replacements in large lists:\n\n(set 'db:db '((monday (apples 20 30) (oranges 2 4 9)) (tuesday (apples 5) (oranges 32 1))))\n\n(define (foo ctx)\n        (set-ref-all 'apples ctx \"Apples\")\n)\n\n(foo db)\n \226\134\146 ((monday (\"Apples\" 20 30) (oranges 2 4 9)) (tuesday (\"Apples\" 5) (oranges 32 1)))\n\nWhen evaluating (foo db), the list in db:db will be passed by reference and\nset-ref-all will make the changes on the original, not on a copy of db:db.\n\nLike with find, replace, ref and ref-all, complex searches can be expressed\nusing match or unify in func-compare:\n\n(set 'data '((monday (apples 20 30) (oranges 2 4 9)) (tuesday (apples 5) (oranges 32 1))))\n\n(set-ref-all '(oranges *) data (list (first $it) (apply + (rest $it))) match)\n    \226\134\146 ( ... (oranges 15) ... (oranges 33) ... )\n\nThe example sums all numbers found in records starting with the symbol oranges.\nThe found items appear in $it\n\nSee also set-ref which replaces only the first element found.")

(set  (sym "_setq setf" MAIN:man-content)  "{setq setf !\n\nsyntax: (setq place-1 exp-1 [place-2 exp-2 ... ])\n\nsetq and setf work alike in newLISP and set the contents of a symbol, list,\narray or string or of a list, array or string place reference. Like set, setq\nand setf can take multiple argument pairs. Although both setq and setf point to\nthe same built-in function internally, throughout this manual setq is used when\nsetting a symbol reference and setf is used when setting list or array\nreferences.\n\n(setq x 123)  \226\134\146 123\n\n; multiple arguments\n\n(setq x 1 y 2 z 3)  \226\134\146 3\n\nx  \226\134\146 1\ny  \226\134\146 2\nz  \226\134\146 3\n\n; with nth or implicit indices\n(setq L '(a b (c d) e f g))\n\n(setf (L 1) 'B)      \226\134\146 B\n; or the same\n(setf (nth 1 L) 'B)\nL                    \226\134\146 (a B (c d) e f g)\n\n(setf (L 2 0) 'C)    \226\134\146 C\nL                    \226\134\146 (a B (C d) e f g)\n\n(setf (L 2) 'X)\nL                    \226\134\146 (A B X e f g)\n\n; with assoc\n(setq L '((a 1) (b 2)))\n(setf (assoc 'b L) '(b 3)) \226\134\146 (b 3)\nL                          \226\134\146 ((a 1) (b 3))\n\n; with lookup\n(setf (lookup 'b L) 30) \226\134\146 30\nL                       \226\134\146 ((a 1) (b 30))\n\n; several list accessors can be nested\n(setq L '((a 1) (b 2)))\n\n(push 'b (setf (assoc 'b l) '(b 4))) 'b) \226\134\146 b\nL                                        \226\134\146((a 1) (b b 4)))\n\n; on strings\n(set 's \"NewISP\")\n\n(setf (s 0) \"n\") \226\134\146 \"n\"\ns \226\134\146 \"newISP\"\n\n(setf (s 3) \"LI\") \226\134\146 \"LI\"\ns \226\134\146 \"newLISP\"\n\nOften the new value set is dependent on the old value. setf can use the\nanaphoric system variable $it to refer to the old value inside the setf\nexpression:\n\n(setq L '((apples 4) (oranges 1)))\n\n(setf (L 1 1) (+ $it 1)) \226\134\146 2\n\nL                        \226\134\146 ((apples 4) (oranges 2))\n\n(set 's \"NewLISP\")\n\n(setf (s 0) (lower-case $it)) \226\134\146 \"n\")\n\ns \226\134\146 \"newLISP\"")

(set '_sgn "{sgn\n\nsyntax: (sgn num)\nsyntax: (sgn num exp-1 [exp-2 [exp-3]])\n\nIn the first syntax, the sgn function is a logical function that extracts the\nsign of a real number according to the following rules:\n\nx > 0 : sgn(x) = 1\nx < 0 : sgn(x) = -1\nx = 0 : sgn(x) = 0\n\n(sgn -3.5)  \226\134\146 -1\n(sgn 0)     \226\134\146 0\n(sgn 123)   \226\134\146 1\n\nIn the second syntax, the result of evaluating one of the optional expressions\nexp-1, exp-2, or exp-3 is returned, instead of -1, 0, or 1. If exp-n is missing\nfor the case triggered, then nil is returned.\n\n(sgn x -1 0 1)         ; works like (sgn x)\n(sgn x -1 1 1)         ; -1 for negative x all others 1\n(sgn x nil true true)  ; nil for negative else true\n(sgn x (abs x) 0)      ; (abs x) for x < 0, 0 for x = 0, else nil\n\nAny expression or constant can be used for exp-1, exp-2, or exp-3.")

(set '_share [text]{share

syntax: (share)
syntax: (share int-address-or-handle)
syntax: (share int-address-or-handle exp-value)

syntax: (share nil int-address)

Accesses shared memory for communicating between several newLISP processes.
When called without arguments, share requests a page of shared memory from the
operating system. This returns a memory address on Linux/Unix and a handle on
Win32, which can then be assigned to a variable for later reference. This
function is not available on OS/2.

To set the contents of shared memory, use the third syntax of share. Supply a
shared memory address on Linux/Unix or a handle on Win32 in
int-address-or-handle, along with an integer, float, string expression or any
other expression (since v.10.1.0) supplied in exp-value. Using this syntax, the
value supplied in exp-value is also the return value.

To access the contents of shared memory, use the second syntax of share,
supplying only the shared memory address or handle. The return value will be
any constant or expression (since v.10.1.0) written previously into the memory.
If the memory has not been previously set to a value, nil will be returned.

Only available on Unix-like operating systems, the last syntax unmaps a shared
memory address. Note that using a shared address after unmapping it will crash
the system.

Memory can be shared between parent and child processes, but not between
independent processes.

Since v.10.1.0 size of share objects can exceed the shared memory pagesize of
the operating system. For objects bigger than the pagesize, newLISP internally
uses files for sharing. This requires a /tmp directory on Unix-like operating
system and a temp directory in the root of the current disk drive on Win32
systems. On Unix-like systems this directory is present, on Win32 it may have
to be created.

(set 'mem (share))

(share mem 123)  → 123
(share mem)      → 123

(share mem "hello world") → "hello world"
(share mem)               → "hello world"

(share mem true)  → true
(share mem)       → true

(share mem '(+ 1 2 3 4))  → (+ 1 2 3 4)
(share mem)               → (+ 1 2 3 4)

; expressions received can be evaluated (since v.10.1.0)
(eval (share mem))        → 10

(share nil mem)   → true  ; unmap only on Unix

Expression read from shared memory and evaluated, will be evaluated in the
recipient's process environment.

Note that shared memory access between different processes should be
synchronized using a semaphore. Simultaneous access to shared memory can crash
the running process.

For a more comprehensive example of using shared memory in a multi process
Linux/Unix application, see the file example/prodcons.lsp in the newLISP source
distribution.[/text])

(set '_signal [text]{signal

syntax: (signal int-signal sym-event-handler | func-event-handler)
syntax: (signal int-signal "ignore" | "default" | "reset")
syntax: (signal int-signal)

Sets a user-defined handler in sym-event-handler for a signal specified in
int-signal or sets to a function expression in func-event-handler.

A parameter following int-signal is not evaluated.

If no signal handler is speified any of the string constants "ignore",
"default" or "reset" can be specified in either lower or upper case or simply
using the the first letter of the option string. When signal setup with any of
these three options has been successfull true is returned.

Using "ignore" will make newLISP ignore the signal. Using "default" will set
the handler to the default handler of the underlying platform OS. The "reset"
option will restore the handler to newLISP startup state.

On startup, newLISP either specifies an empty newLISP handler or a Ctrl-C
handler for SIGINT and a waitpipd(-1, 0, WNOHANG) C-call for SIGCHLD.

Different signals are available on different OS platforms and Linux/Unix
flavors. The numbers to specify in int-signal also differ from
platform-to-platform. Valid values can normally be extracted from a file found
in /usr/include/sys/signal.h or /usr/include/signal.h.

Some signals make newLISP exit even after a user-defined handler has been
specified and executed (e.g., signal SIGKILL). This behavior may also be
different on different platforms.

(constant 'SIGINT 2)
(define (ctrlC-handler) (println "ctrl-C has been pressed"))

(signal SIGINT 'ctrlC-handler)

; now press ctrl-C
; the following line will appear
; this will only work in an interactive terminal window
; and will not work in the newLISP-GS editor

ctrl-C has been pressed

; reset tratment of signal 2 to startup conditions

(signal SIGINT "reset")

On Win32, the above example would execute the handler before exiting newLISP.
On most Linux/Unix systems, newLISP would stay loaded and the prompt would
appear after hitting the [enter] key.

Instead of specifying a symbol containing the signal handler, a function can be
specified directly. The signal number is passed as a parameter:

(signal SIGINT exit)  → $signal-2

(signal SIGINT (fn (s) (println "signal " s " occurred")))

Note that the signal SIGKILL (9 on most platforms) will always terminate the
application regardless of an existing signal handler.

The signal could have been sent from another shell on the same computer:

kill -s SIGINT 2035

In this example, 2035 is the process ID of the running newLISP.

The signal could also have been sent from another newLISP application using the
function destroy:

(destroy 2035) → true

If newLISP receives a signal while evaluating another function, it will still
accept the signal and the handler function will be executed:

(constant 'SIGINT 2)
(define (ctrlC-handler) (println "ctrl-C has been pressed"))

(signal SIGINT 'ctrlC-handler)
;; or
(signal SIGINT ctrlC-handler)


(while true (sleep 300) (println "busy"))

;; generates following output
busy
busy
busy
ctrl-C has been pressed
busy
busy
…

Specifying only a signal number will return either the name of the currently
defined handler function or nil.

The user-defined signal handler can pass the signal number as a parameter.

(define (signal-handler sig)
        (println "received signal: " sig))

;; set all signals from 1 to 8 to the same handler
(for (s 1 8)
        (signal s 'signal-handler))

In this example, all signals from 1 to 8 are set to the same handler.[/text])

(set '_silent "{silent\n\nsyntax: (silent [exp-1 [exp-2 ... ]])\n\nEvaluates one or more expressions in exp-1\226\128\148. silent is similar to begin, but it\nsuppresses console output of the return value and the following prompt. It is\noften used when communicating from a remote application with newLISP (e.g., GUI\nfront-ends or other applications controlling newLISP), and the return value is\nof no interest.\n\nSilent mode is reset when returning to a prompt. This way, it can also be used\nwithout arguments in a batch of expressions. When in interactive mode, hit\n[enter] twice after a statement using silent to get the prompt back.\n\n(silent (my-func))  ; same as next\n\n(silent) (my-func)  ; same effect as previous")

(set '_sin "{sin\n\nsyntax: (sin num-radians)\n\nCalculates the sine function from num-radians and returns the result.\n\n(sin 1)                     \226\134\146 0.8414709838\n(set 'pi (mul 2 (acos 0)))  \226\134\146 3.141592654\n(sin (div pi 2))            \226\134\146 1")

(set '_sinh "{sinh\n\nsyntax: (sinh num-radians)\n\nCalculates the hyperbolic sine of num-radians. The hyperbolic sine is defined\nmathematically as: (exp (x) - exp (-x)) / 2. An overflow to inf may occur if\nnum-radians is too large.\n\n(sinh 1)     \226\134\146 1.175201194\n(sinh 10)    \226\134\146 11013.23287\n(sinh 1000)  \226\134\146 inf\n(sub (tanh 1) (div (sinh 1) (cosh 1))) \226\134\146 0")

(set '_sleep "{sleep\n\nsyntax: (sleep num-milliseconds)\n\nGives up CPU time to other processes for the amount of milliseconds specified\nin num-milli-seconds.\n\n(sleep 1000)  ; sleeps 1 second\n(sleep 0.5)   ; sleeps 500 micro seconds\n\nOn some platforms, sleep is only available with a resolution of one second. In\nthis case, the parameter int-milli-seconds will be rounded to the nearest full\nsecond.")

(set '_slice "{slice\n\nsyntax: (slice list int-index [int-length])\nsyntax: (slice array int-index [int-length])\nsyntax: (slice str int-index [int-length])\n\nIn the first form, slice copies a sublist from a list. The original list is\nleft unchanged. The sublist extracted starts at index int-index and has a\nlength of int-length. If int-length is negative, slice will take the parameter\nas offset counting from the end and copy up to that offset. If the parameter is\nomitted, slice copies all of the elements to the end of the list.\n\nSee also Indexing elements of strings and lists.\n\n(slice '(a b c d e f) 3 2)   \226\134\146 (d e)\n(slice '(a b c d e f) 2 -2)  \226\134\146 (c d)\n(slice '(a b c d e f) 2)     \226\134\146 (c d e f)\n(slice '(a b c d e f) -4 3)  \226\134\146 (c d e)\n\n(set 'A (array 3 2 (sequence 1 6))) \226\134\146 ((1 2) (3 4) (5 6))\n(slice A 1 2) \226\134\146 ((3 4) (5 6))\n\nIn the second form, a part of the string in str is extracted. int-index\ncontains the start index and int-length contains the length of the substring.\nIf int-length is not specified, everything to the end of the string is\nextracted. slice also works on string buffers containing binary data like 0's\n(zeroes). It operates on byte boundaries rather than character boundaries. See\nalso Indexing elements of strings and lists.\n\nNote that slice always works on single 8-bit byte boundaries for offset and\nlength numbers, even when running the UTF-8 enabled version of newLISP.\n\n(slice \"Hello World\" 6 2)  \226\134\146 \"Wo\"\n(slice \"Hello World\" 0 5)  \226\134\146 \"Hello\"\n(slice \"Hello World\" 6)    \226\134\146 \"World\"\n(slice \"newLISP\" -4 2)     \226\134\146 \"LI\"\n\nNote that an implicit slice is available for lists. See the chapter Implicit\nrest and slice.\n\nBe aware that slice always works on byte boundaries rather than character\nboundaries in the UTF-8\226\128\147enabled version of newLISP. As a result, slice can be\nused to manipulate binary content.")

(set '_sort "{sort !\n\nsyntax: (sort list [func-compare])\n\nAll members in list are sorted in ascending order. Anything may be sorted,\nregardless of the types. When members are themselves lists, each list element\nis recursively compared. If two expressions of different types are compared,\nthe lower type is sorted before the higher type in the following order:\n\nAtoms: nil, true, integer or float, string, symbol, primitive\nLists: quoted expression, list, lambda, lambda-macro\n\nThe sort is destructive, changing the order of the elements in the original\nlist. The return value of sort is a copy of the sorted list.\n\nAn optional comparison operator, user-defined function, or anonymous function\ncan be supplied. The functor or operator can be given with or without a\npreceding quote.\n\n(sort '(v f r t h n m j))     \226\134\146 (f h j m n r t v)\n(sort '((3 4) (2 1) (1 10)))  \226\134\146 ((1 10) (2 1) (3 4))\n(sort '((3 4) \"hi\" 2.8 8 b))  \226\134\146 (2.8 8 \"hi\" b (3 4))\n\n(set 's '(k a l s))\n(sort s)  \226\134\146 (a k l s)\n\n(sort '(v f r t h n m j) >) \226\134\146 (v t r n m j h f)\n\n(sort s <)  \226\134\146 (a k l s)\n(sort s >)  \226\134\146 (s l k a)\ns           \226\134\146 (s l k a)\n\n;; define a comparison function\n(define (comp x y)\n    (> (last x) (last y)))\n\n(set 'db '((a 3) (g 2) (c 5)))\n\n(sort db comp)  \226\134\146  ((c 5) (a 3) (g 2))\n\n;; use an anonymous function\n(sort db (fn (x y) (> (last x) (last y))))")

(set '_source "{source\n\nsyntax: (source)\nsyntax: (source sym-1 [sym-2 ... ])\n\nWorks almost identically to save, except symbols and contexts get serialized to\na string instead of being written to a file. Multiple variable symbols,\ndefinitions, and contexts can be specified. If no argument is given, source\nserializes the entire newLISP workspace. When context symbols are serialized,\nany symbols contained within that context will be serialized, as well. Symbols\ncontaining nil are not serialized. System symbols beginning with the $ (dollar\nsign) character are only serialized when mentioned explicitly.\n\nSymbols not belonging to the current context are written out with their context\nprefix.\n\n(define (double x) (+ x x))\n\n(source 'double)  \226\134\146 \"(define (double x)\\n  (+ x x))\\n\\n\"\n\nAs with save, the formatting of line breaks and leading spaces or tabs can be\ncontrolled using the pretty-print function.")

(set '_spawn [text]{spawn

syntax: (spawn sym exp [true])

Launches the evaluation of exp as a child process and immediately returns. The
symbol in sym is quoted and receives the result of the evaluation when the
function sync is executed. spawn is used to start parallel evaluation of
expressions in concurrent processes. If newLISP is running on a multi-core CPU,
the underlying operating system will distribute spawned processes onto
different cores, thereby evaluating expressions in parallel and speeding up
overall processing.

The optional true parameter must be set if send or receive is used to
communicated with the child process spawned.

The function spawn is not available on Win32.

After successfully starting a child process, the spawn expression returns the
process id of the forked process. The following examples shows how the
calculation of a range of prime numbers can be split up in four sub ranges to
speed up the calculation of the whole range:

; calculate primes in a range
(define (primes from to)
  (local (plist)
      (for (i from to)
          (if (= 1 (length (factor i)))
              (push i plist -1)))
      plist))

; start child processes
(set 'start (time-of-day))

(spawn 'p1 (primes 1 1000000))
(spawn 'p2 (primes 1000001 2000000))
(spawn 'p3 (primes 2000001 3000000))
(spawn 'p4 (primes 3000001 4000000))

; wait for a maximum of 60 seconds for all tasks to finish
(sync 60000) ; returns true if all finished in time
; p1, p2, p3 and p4 now each contain a lists of primes

(println "time spawn: " (- (time-of-day) start))
(println "time simple: " (time  (primes 1 4000000)))

(exit)

On a 1.83 Intel Core 2 Duo processor, the above example will finish after about
13 seconds. Calculating all primes using (primes 1 4000000) would take about 20
seconds.

The sync function will wait for all child processes to finish and receive the
evaluation results in the symbols p1 to p4. When all results are collected,
sync will stop waiting and return true. When the time specified was
insufficient , sync will return nil and another sync statement could be given
to further wait and collect results. A short timeout time can be used to do
other processing during waiting:

(spawn 'p1 (primes 1 1000000))
(spawn 'p2 (primes 1000001 2000000))
(spawn 'p3 (primes 2000001 3000000))
(spawn 'p4 (primes 3000001 4000000))

; print a dot after each 2 seconds of waiting
(until (sync 2000) (println "."))

sync when used without any parameters, will not wait but immediately return a
list of pending child processes. For the primes example, the following sync
expression could be used to watch the progress:

(spawn 'p1 (primes 1 1000000))
(spawn 'p2 (primes 1000001 2000000))
(spawn 'p3 (primes 2000001 3000000))
(spawn 'p4 (primes 3000001 4000000))

; show a list of pending process ids after each three-tenths of a second
(until (sync 300) (println (sync)))

A parameter of -1 tells sync to wait for a very long time (~ 1193 hours). A
better solution would be to wait for a maximum time, then abort all pending
child processes:

(spawn 'p1 (primes 1 1000000))
(spawn 'p2 (primes 1000001 2000000))
(spawn 'p3 (primes 2000001 3000000))
(spawn 'p4 (primes 3000001 4000000))

; wait for one minute, then abort and
; report unfinished PIDs

(if (not (sync 60000))
    (begin
        (println "aborting unfinished: " (sync))
        (abort))
    (println "all finished successfully")
)

The three functions spawn, sync and abort are part of the Cilk API. The
original implementation also does sophisticated scheduling and allocation of
threaded tasks to multiple CPU cores. The newLISP implementation of the Cilk
API lets the operating system of the underlying platform handle process
management. Internally, the API is implemented using the Unix libc functions
fork(), waitpid() and kill(). Intercommunications between processes and child
processes is done using the send and receive functions.

spawn can be called recursively from spawned subtasks:

(define (fibo n)
  (local (f1 f2)
    (if(< n 2) 1
       (begin
          (spawn 'f1 (fibo (- n 1)))
          (spawn 'f2 (fibo (- n 2)))
          (sync 10000)
          (+ f1 f2)))))

(fibo 7)  → 21

With (fibo 7) 41 processes will be generated. Although the above code shows the
working of the Cilk API in a recursive application, it would not be practical,
as the overhead required to spawn subtasks is much higher than the time saved
through parallelization.

Since version 10.1 a send and receive message functions are available for
communications between parent and child processes. Using these functions any
data or expression of any size can be transferred. Additionally messaged
expressions can be evaluated in the recipient's environment.[/text])

(set '_sqrt "{sqrt\n\nsyntax: (sqrt num)\n\nCalculates the square root from the expression in num and returns the result.\n\n(sqrt 10)  \226\134\146 3.16227766\n(sqrt 25)  \226\134\146 5")

(set '_starts-with "{starts-with\n\nsyntax: (starts-with str str-key [num-option])\nsyntax: (starts-with list [exp])\n\nIn the first version, starts-with checks if the string str starts with a key\nstring in str-key and returns true or nil depending on the outcome.\n\nIf a regular expression number is specified in num-option, str-key contains a\nregular expression pattern. See regex for valid option numbers.\n\n(starts-with \"this is useful\" \"this\")        \226\134\146 true\n(starts-with \"this is useful\" \"THIS\")        \226\134\146 nil\n\n;; use regular expressions\n(starts-with \"this is useful\" \"THIS\" 1)      \226\134\146 true\n(starts-with \"this is useful\" \"this|that\" 0) \226\134\146 true\n\nIn the second version, starts-with checks to see if a list starts with the list\nelement in exp. true or nil is returned depending on outcome.\n\n(starts-with '(1 2 3 4 5) 1)             \226\134\146 true\n(starts-with '(a b c d e) 'b)            \226\134\146 nil\n(starts-with '((+ 3 4) b c d) '(+ 3 4))  \226\134\146 true\n\nSee also the ends-with function.")

(set '_stats "{stats\n\nsyntax: (stats list-vector)\n\nThe functions calculates statistical values of central tendency and\ndsitribution moments of values in list-vector. The following values are\nreturned by stats in a list:\n\nname  description\nN     Number of values\nmean  Mean of values\navdev Average deviation from mean value\nsdev  Standard deviation (population estimate)\nvar   Variance (population estimate)\nskew  Skew of distribution\nkurt  Kurtosis of distribution\n\n\nThe following example uses the list output from the stats expression as an\nargument for the format statement:\n\n(set 'data '(90 100 130 150 180 200 220 300 350 400))\n\n(println (format [text]\n    N        = %5d\n    mean     = %8.2f\n    avdev    = %8.2f\n    sdev     = %8.2f\n    var      = %8.2f\n    skew     = %8.2f\n    kurt     = %8.2f\n[/text] (stats data)))\n\n; outputs the following\n\n    N        =    10\n    mean     =   212.00\n    avdev    =    84.40\n    sdev     =   106.12\n    var      = 11262.22\n    skew     =     0.49\n    kurtosis =    -1.34")

(set '_string "{string\n\nsyntax: (string exp-1 [exp-2 ... ])\n\nTranslates into a string anything that results from evaluating exp-1\226\128\148. If more\nthan one expression is specified, the resulting strings are concatenated.\n\n(string 'hello)          \226\134\146 \"hello\"\n(string 1234)            \226\134\146 \"1234\"\n(string '(+ 3 4))        \226\134\146 \"(+ 3 4)\"\n(string (+ 3 4) 8)       \226\134\146 \"78\"\n(string 'hello \" \" 123)  \226\134\146 \"hello 123\"\n\nIf a buffer passed to string contains \\000, only the string up to the first\nterminating zero will be copied:\n\n(set 'buff \"ABC\\000\\000\\000\")  \226\134\146 \"ABC\\000\\000\\000\"\n\n(length buff)  \226\134\146 6\n\n(string buff)  \226\134\146 \"ABC\"\n\n(length (string buff))  \226\134\146 3\n\nUse the append and join (allows the joining string to be specified) functions\nto concatenate strings containing zero bytes. Use the source function to\nconvert a lambda expression into its newLISP source string representation.")

(set '_string? "{string?\n\nsyntax: (string? exp)\n\nEvaluates exp and tests to see if it is a string. Returns true or nil depending\non the result.\n\n(set 'var \"hello\")\n(string? var)  \226\134\146 true")

(set '_struct [text]{struct

syntax: (struct symbol [str-data-type ... ])

The struct function can be used to define aggregate data types for usage with
the extended syntax of import, pack and unpack. This allows importing functions
which take C-language struct data types or pointers to these aggregate data
types.

The following example illustrates the usage of struct together with the C data
functions localtime and asctime. The localtime functions works similar to the
built-in now function. The asctime function takes the numerical data output by
localtime and formats these to readable text.

/* The C function prototypes for the functions to import */

struct tm * localtime(const time_t *clock);

char * asctime(const struct tm *timeptr);

/* the tm struct aggregating different time related values */

struct tm {
    int tm_sec;      /* seconds after the minute [0-60] */
    int tm_min;      /* minutes after the hour [0-59] */
    int tm_hour;     /* hours since midnight [0-23] */
    int tm_mday;     /* day of the month [1-31] */
    int tm_mon;      /* months since January [0-11] */
    int tm_year;     /* years since 1900 */
    int tm_wday;     /* days since Sunday [0-6] */
    int tm_yday;     /* days since January 1 [0-365] */
    int tm_isdst;    /* Daylight Savings Time flag */
    long tm_gmtoff;  /* offset from CUT in seconds */   /*** not on Windows ***/
    char *tm_zone;   /* timezone abbreviation */        /*** not on Windows ***/
};

Function import and definition of the structure data type in newLISP:

;; for pointers to structs always use void*
;; as a library use msvcrt.dll on Windows or libc.so on Unix.
;; The tm struct type is configured for Mac OSX and Linux.
;; On other OS the tm structure may be different

(import "libc.dylib" "asctime" "char*" "void*")
(import "libc.dylib" "localtime" "void*" "void*")

; definition of the struct
(struct 'tm "int" "int" "int" "int" "int" "int" "int" "int" "int" "long" "char*")


;; use import and struct

; todays date number (seconds after 1970 also called Unix epoch time)
(set 'today (date-value))  → 1324134913

;; the time value is passed by it's address
;; localtime retirns a pointer to a tm struct

(set 'ptr (localtime (address today))) → 2896219696

; unpack the the tm struct  (7:15:13 on the 17th etc.)
(unpack tm ptr) → (13 15 7 17 11 111 6 350 0 -28800 "PST")

; transform to readable form
(asctime ptr) → "Sat Dec 17 07:15:13 2011\n"

; all in one statement does actually not use struct, pointers are passed directly
(asctime (localtime (address today))) → "Sat Dec 17 07:15:13 2011"

; same as the built-in date function
(date today) → "Sat Dec 17 07:15:13 2011"

Care must be taken to pass valid addresses to pointer parameters in imported
functions or when passing address pointers to unpack. Invalid address pointers
can crash newLISP or make it unstable.

struct defininitions can be nested:

; the pair aggregate type
(struct 'pair "char" "char") → pair

; nested struct type
(struct 'comp "pair" "int")  → comp

; pack data using the extended pack syntax
; not the insertion of structure alignment bytes after the pair
(pack comp (pack pair 1 2) 3) → "\001\002\000\000\003\000\000\000"

; unpack reverses the process
(unpack comp "\001\002\000\000\003\000\000\000") → ((1 2) 3)

Nested structures are unpacked recursively.[/text])

(set '_sub "{sub\n\nsyntax: (sub num-1 [num-2 ... ])\n\nSuccessively subtracts the expressions in num-1, num-2\226\128\148. sub performs\nmixed-type arithmetic and handles integers or floating points, but it will\nalways return a floating point number. If only one argument is supplied, its\nsign is reversed. Any floating point calculation with NaN also returns NaN.\n\n(sub 10 8 0.25)  \226\134\146 1.75\n(sub 123)        \226\134\146 -123")

(set '_swap "{swap !\n\nsyntax: (swap place-1 place-2)\n\nThe contents of the two places place-1 and place-2 are swapped. A place can be\nthe contents of an unquoted symbol or any list or array references expressed\nwith nth, first, last or implicit indexing or places referenced by assoc or\nlookup.\n\nswap is a destructive operation that changes the contents of the lists, arrays,\nor symbols involved.\n\n(set 'lst '(a b c d e f))\n\n(swap (first lst) (last lst)) \226\134\146 a\nlst                           \226\134\146 (f b c d e a)\n\n(set 'lst-b '(x y z))\n\n(swap (lst 0) (lst-b -1)) \226\134\146 f\nlst                       \226\134\146 (z b c d e a)\nlst-b                     \226\134\146 (x y f)\n\n(set 'A (array 2 3 (sequence 1 6)) \226\134\146 ((1 2 3) (4 5 6))\n\n(swap (A 0) (A 1)) \226\134\146 (1 2 3)\nA                  \226\134\146 ((4 5 6) (1 2 3))\n\n(set 'x 1 'y 2)\n\n(swap x y)  \226\134\146 1\nx  \226\134\146 2\ny  \226\134\146 1\n\n(set 'lst '((a 1 2 3) (b 10 20 30)))\n(swap (lookup 'a lst -1) (lookup 'b lst 1))\nlst \226\134\146 ((a 1 2 10) (b 3 20 30))\n\n(swap (assoc 'a lst) (assoc 'b lst))\nlst \226\134\146  ((b 3 20 30) (a 1 2 10))\n\nAny two places can be swept in the same or different objects.")

(set '_sym [text]{sym

syntax: (sym string [sym-context nil-flag])
syntax: (sym number [sym-context nil-flag])
syntax: (sym symbol [sym-context nil-flag])

Translates the first argument in string, number, or symbol into a symbol and
returns it. If the optional context is not specified in sym-context, the
current context is used when doing symbol lookup or creation. Symbols will be
created if they do not already exist. When the context does not exist and the
context is specified by a quoted symbol, the symbol also gets created. If the
context specification is unquoted, the context is the specified name or the
context specification is a variable containing the context.

sym can create symbols within the symbol table that are not legal symbols in
newLISP source code (e.g., numbers or names containing special characters such
as parentheses, colons, etc.). This makes sym usable as a function for
associative memory access, much like hash table access in other scripting
languages.

As a third optional argument, nil can be specified to suppress symbol creation
if the symbol is not found. In this case, sym returns nil if the symbol looked
up does not exist. Using this last form, sym can be used to check for the
existence of a symbol.

(sym "some")           → some
(set (sym "var") 345)  → 345
var                    → 345
(sym "aSym" 'MyCTX)    → MyCTX:aSym
(sym "aSym" MyCTX)     → MyCTX:aSym  ; unquoted context

(sym "foo" MyCTX nil)  → nil  ; 'foo does not exist
(sym "foo" MyCTX)      → foo  ; 'foo is created
(sym "foo" MyCTX nil)  → foo  ; foo now exists

Because the function sym returns the symbol looked up or created, expressions
with sym can be embedded directly in other expressions that use symbols as
arguments. The following example shows the use of sym as a hash-like function
for associative memory access, as well as symbol configurations that are not
legal newLISP symbols:

;; using sym for simulating hash tables

(set (sym "John Doe" 'MyDB) 1.234)
(set (sym "(" 'MyDB) "parenthesis open")
(set (sym 12 'MyDB) "twelve")

(eval (sym "John Doe" 'MyDB))  → 1.234
(eval (sym "(" 'MyDB))         → "parenthesis open"
(eval (sym 12 'MyDB))          → "twelve"

;; delete a symbol from a symbol table or hash
(delete (sym "John Doe" 'MyDB))  → true

The last statement shows how a symbol can be eliminated using delete.

The third syntax allows symbols to be used instead of strings for the symbol
name in the target context. In this case, sym will extract the name from the
symbol and use it as the name string for the symbol in the target context:

(sym 'myVar 'FOO)  → FOO:myVar

(define-macro (def-context)
  (dolist (s (rest (args)))
    (sym s (first (args)))))

(def-context foo x y z)

(symbols foo)  → (foo:x foo:y foo:z)

The def-context macro shows how this could be used to create a macro that
creates contexts and their variables in a dynamic fashion.

A syntax of the context function can also be used to create, set and evaluate
symbols.[/text])

(set '_symbol? "{symbol?\n\nsyntax: (symbol? exp)\n\nEvaluates the exp expression and returns true if the value is a symbol;\notherwise, it returns nil.\n\n(set 'x 'y)  \226\134\146 y\n\n(symbol? x)  \226\134\146 true\n\n(symbol? 123)  \226\134\146 nil\n\n(symbol? (first '(var x y z)))  \226\134\146 true\n\nThe first statement sets the contents of x to the symbol y. The second\nstatement then checks the contents of x. The last example checks the first\nelement of a list.")

(set '_symbols "{symbols\n\nsyntax: (symbols [context])\n\nReturns a sorted list of all symbols in the current context when called without\nan argument. If a context symbol is specified, symbols defined in that context\nare returned.\n\n(symbols)       ; list of all symbols in current context\n(symbols 'CTX)  ; list of symbols in context CTX\n(symbols CTX)   ; omitting the quote\n(set 'ct CTX)   ; assigning context to a variable\n(symbols ct)    ; list of symbols in context CTX\n\nThe quote can be omitted because contexts evaluate to themselves.")

(set '_sync "{sync\n\nsyntax: (sync int-timeout [func-inlet])\nsyntax: (sync)\n\nWhen int-timeout in milliseconds is specified, sync waits for child processes\nlaunched with spawn to finish. Whenever a child process finishes, sync assigns\nthe evaluation result of the spawned subtask to the symbol specified in the\nspawn statement. The sync returns true if all child processes have been\nprocessed or nil if the timeout value has been reached and more child processes\nare pending.\n\nIf sync additionally is given with an optional user-defined inlet function in\nfunc-inlet, this function will be called with the child process-id as argument\nwhenever a spawned child process returns. func-inlet can contain either a\nlambda expression or a symbol which defines a function.\n\nWithout any parameter, sync returns a list of pending child process PIDs\n(process identifiers), for which results have not been processed yet.\n\nThe function sync is not available on Win32.\n\n; wait for 10 seconds and process finished child processes\n(sync 10000)\n\n; wait for the maximum time (~ 1193 hours)\n(sync -1)\n\n(define (report pid)\n    (println \"process: \" pid \" has returned\"))\n\n; call the report function, when a child returns\n(sync 10000 report) ; wait for 10 seconds max\n\n; return a list of pending child processes\n(sync)         \226\134\146 (245 246 247 248)\n\n; wait and do something else\n(until (true? (sync 10 report) )\n    (println (time-of-day)))\n\n\nWhen sync is given with a timeout parameter, it will block until timeout or\nuntil all child processes have returned, whichever comes earlier. When no\nparameter is specified or a function is specified, sync returns immediately.\n\nThe function sync is part of the Cilk API for synchronizing child processes and\nprocess parallelization. See the reference for the function spawn for a full\ndiscussion of the Cilk API.")

(set '_sys-error "{sys-error\n\nsyntax: (sys-error)\nsyntax: (sys-error int-error)\nsyntax: (sys-error 0)\n\nReports the last error generated by the underlying OS which newLISP is running\non. The error reported may differ on the platforms newLISP has been compiled\nfor. Consult the platform's C library information. The error is reported as a\nlist of error number and error text.\n\nIf no error has occurred or the system error number has been reset, nil is\nreturned.\n\nWhen int-error is greater 0 (zero) a list of the number and the error text is\nreturned.\n\nTo reset the error specify 0 as the error number.\n\nWhenever a function in newLISP within the system resources area returns nil,\nsys-error can be checked for the underlying reason. For file operations,\nsys-error may be set for nonexistent files or wrong permissions when accessing\nthe resource. Another cause of error could be the exhaustion of certain system\nresources like file handles or semaphores.\n\n;; trying to open a nonexistent file\n(open \"xyz\" \"r\")  \226\134\146 nil\n\n(sys-error)       \226\134\146 (2 \"No such file or directory\")\n\n;; reset errno\n(sys-error 0)     \226\134\146 (0 \"Unknown error: 0\")\n(sys-error)       \226\134\146 nil\n\nSee also last-error and net-error.")

(set '_sys-info "{sys-info\n\nsyntax: (sys-info [int-idx])\n\nCalling sys-info without int-idx returns a list of internal resource\nstatistics. Ten integers report the following status:\n\noffset description\n0      Number of Lisp cells\n1      Maximum number of Lisp cells constant\n2      Number of symbols\n3      Evaluation/recursion level\n4      Environment stack level\n5      Maximum call stack constant\n6      Pid of the parent process or 0\n7      Pid of running newLISP process\n8      Version number as an integer constant\n       Operating system constant:\n       linux=1, bsd=2, osx=3, solaris=4, win32=6, os/2=7, cygwin=8, tru64unix\n       =9, aix=10\n           bit 11 will be set for ffilib (extended import/callback API)\n9      versions (add 1024)\n           bit 10 will be set for IPv6 versions (add 512)\n           bit 9 will be set for 64-bit (changeable at runtime) versions (add\n       256)\n           bit 8 will be set for UTF-8 versions (add 128)\n           bit 7 will be added for library versions (add 64)\n\n\nThe numbers from 0 to 9 indicate the optional offset in the returned list.\n\nIt is recommended to use offsets 0 to 5 to address up and including \"Maximum\ncall stack constant\" and to use negative offsets -1 to -4 to access the last\nfour entries in the system info list. Future new entries will be inserted after\noffset 5. This way older source code does not need to change.\n\nWhen using int-idx, one element of the list will be returned.\n\n(sys-info)     \226\134\146 (401 268435456 376 1 0 2048 39220 10002 131)\n(sys-info 3)   \226\134\146 1\n(sys-info -2)  \226\134\146 10002\n\nThe number for the maximum of Lisp cells can be changed via the -m command-line\nswitch. For each megabyte of Lisp cell memory, 64k memory cells can be\nallocated. The maximum call stack depth can be changed using the -s\ncommand-line switch.")

(set '_t-test "{t-test\n\nsyntax: (t-test list-vector-A list-vecor-B [true])\n\nThe functions performs a t-test using the Student's t statistic for comparing\nthe means values in list-vector-A and list-vecor-B. If the true flag is not\nused, both vectors in A and B can be of different length and groups represented\nby A and B are not related.\n\nWhen the optional flag is set to true, measurements were taken from the same\ngroup twice, e.g. before and after a procedure.\n\nThe following results are returned in a list:\n\nname   description\nmean-a Mean of group A\nmean-b Mean of group B\nsdev-a Standard deviation in group A\nsdev-b Standard deviation in group B\nt      t between mean values\ndf     Degress of freedom\np      Two tailed probablity of t under the null hypothesis\n\nThe first example studies the effect of different sleep length before a test on\nthe SCAT (Sam's Cognitive Ability Test):\n\n; SCAT (Sam's Cognitive Ability Test)\n(set 'hours-sleep-8 '(5 7 5 3 5 3 3 9))\n(set 'hours-sleep-4 '(8 1 4 6 6 4 1 2))\n\n(t-test hours-sleep-8 hours-sleep-4)\n \226\134\146 (5 4 2.138 2.563 0.847 14 0.411)\n\nThe duration of sleeps before the SCAT does not have a significant impact with\na probability value of 0.411.\n\nIn the second example, the same group of people get tested twice, before and\nafter a treatment with Prosac depression medication:\n\n; Effect of Prozac on a group of depressed people\n(set 'mood-pre '(3 0 6 7 4 3 2 1 4))\n(set 'mood-post '(5 1 5 7 10 9 7 11 8))\n\n(t-test mood-pre mood-post true)\n \226\134\146 (3.333 7 2.236 3.041 -3.143 8 0.0137)\n\nThe effect of the Prosac treatment is moderately significant with a p of\n0.0137.")

(set '_tan "{tan\n\nsyntax: (tan num-radians)\n\nCalculates the tangent function from num-radians and returns the result.\n\n(tan 1)                     \226\134\146 1.557407725\n(set 'pi (mul 2 (asin 1)))  \226\134\146 3.141592654\n(tan (div pi 4))            \226\134\146 1")

(set '_tanh "{tanh\n\nsyntax: (tanh num-radians)\n\nCalculates the hyperbolic tangent of num-radians. The hyperbolic tangent is\ndefined mathematically as: sinh (x) / cosh (x).\n\n(tanh 1)     \226\134\146 0.761594156\n(tanh 10)    \226\134\146 0.9999999959\n(tanh 1000)  \226\134\146 1\n(= (tanh 1) (div (sinh 1) (cosh 1)))  \226\134\146 true")

(set '_term "{term\n\nsyntax: (term symbol)\n\nReturns as a string, the term part of a symbol without the context prefix.\n\n(set 'ACTX:var 123)\n(set 'sm 'ACTX:var)\n(string sm)     \226\134\146 \"ACTX:var\"\n(term sm)      \226\134\146 \"var\"\n\n(set 's 'foo:bar)\n(= s (sym (term s) (prefix s)))\n\nSee also prefix to extract the namespace or context prefix from a symbol.")

(set '_throw "{throw\n\nsyntax: (throw exp)\n\nWorks together with the catch function. throw forces the return of a previous\ncatch statement and puts the exp into the result symbol of catch.\n\n(define (throw-test)\n    (dotimes (x 1000)\n        (if (= x 500) (throw \"interrupted\"))))\n\n(catch (throw-test) 'result)  \226\134\146 true\n\nresult  \226\134\146 \"interrupted\"\n\n(catch (throw-test))  \226\134\146 \"interrupted\"\n\nThe last example shows a shorter form of catch, which returns the throw result\ndirectly.\n\nthrow is useful for breaking out of a loop or for early return from\nuser-defined functions or expression blocks. In the following example, the\nbegin block will return X if (foo X) is true; else Y will be returned:\n\n(catch (begin\n    \226\128\166\n    (if (foo X) (throw X) Y)\n    \226\128\166\n))\n\nthrow will not cause an error exception. Use throw-error to throw user error\nexceptions.")

(set '_throw-error "{throw-error\n\nsyntax: (throw-error exp)\n\nCauses a user-defined error exception with text provided by evaluating exp.\n\n(define (foo x y)\n    (if (= x 0) (throw-error \"first argument cannot be 0\"))\n    (+ x y))\n\n(foo 1 2)  \226\134\146 3\n\n(foo 0 2)  ; causes a user error exception\nERR: user error : first argument cannot be 0\ncalled from user-defined function foo\n\nThe user error can be handled like any other error exception using user-defined\nerror handlers and the error-event function, or the form of catch that can\ncapture error exceptions.")

(set '_time "{time\n\nsyntax: (time exp [int-count)\n\nEvaluates the expression in exp and returns the time spent on evaluation in\nfloating point milliseconds. Depending on the platform decimals of milliseconds\nare shown or not shown.\n\n(time (myprog x y z))  \226\134\146 450.340\n\n(time (myprog x y z) 10)  \226\134\146 4420.021\n\nIn first the example, 450 milliseconds elapsed while evaluating (myprog x y z).\nThe second example returns the time for ten evaluations of (myprog x y z). See\nalso date, date-value, time-of-day, and now.")

(set '_time-of-day "{time-of-day\n\nsyntax: (time-of-day)\n\nReturns the time in milliseconds since the start of the current day.\n\nSee also the date, date-value, time, and now functions.")

(set '_timer [text]{timer

syntax: (timer sym-event-handler | func-event-handler num-seconds [int-option])
syntax: (timer sym-event-handler | func-event-handler)
syntax: (timer)

Starts a one-shot timer firing off the Unix signal SIGALRM, SIGVTALRM, or
SIGPROF after the time in seconds (specified in num-seconds) has elapsed. When
the timer fires, it calls the user-defined function in sym- or
func-event-handler.

On Linux/Unix, an optional 0, 1, or 2 can be specified to control how the timer
counts. With default option 0, real time is measured. Option 1 measures the
time the CPU spends processing in the process owning the timer. Option 2 is a
combination of both called profiling time. See the Unix man page setitimer()
for details.

The event handler can start the timer again to achieve a continuous flow of
events. Starting with version 8.5.9, seconds can be defined as floating point
numbers with a fractional part (e.g., 0.25 for 250 milliseconds).

Defining 0 (zero) as time shuts the running timer down and prevents it from
firing.

When called with sym- or func-event-handler, timer returns the elapsed time of
the timer in progress. This can be used to program time lines or schedules.

timer called without arguments returns the symbol of the current event handler.

(define (ticker)
    (println (date)) (timer 'ticker 1.0))

> (ticker)
Tue Apr 12 20:44:48 2005        ; first execution of ticker
→ ticker                              ; return value from ticker

> Tue Apr 12 20:44:49 2005      ; first timer event
Tue Apr 12 20:44:50 2005        ; second timer event ...
Tue Apr 12 20:44:51 2005
Tue Apr 12 20:44:52 2005

The example shows an event handler, ticker, which starts the timer again after
each event.

Note that a timer cannot interrupt an ongoing built-in function. The timer
interrupt gets registered by newLISP, but a timer handler cannot run until one
expression is evaluated and the next one starts. To interrupt an ongoing I/O
operation with timer, use the following pattern, which calls net-select to test
if a socket is ready for reading:

define (interrupt)
    (set 'timeout true))

(set 'listen (net-listen 30001))
(set 'socket (net-accept listen))

(timer 'interrupt 10)
;; or specifying the function directly
(timer (fn () (set 'timeout true)) 10)

(until (or timeout done)
    (if (net-select socket "read" 100000)
        (begin
            (net-receive socket buffer 1024)
            (set 'done true)))
)

(if timeout
    (println "timeout")
    (println buffer))

(exit)

In this example, the until loop will run until something can be read from
socket, or until ten seconds have passed and the timeout variable is set.[/text])

(set '_title-case "{title-case utf8\n\nsyntax: (title-case str [bool])\n\nReturns a copy of the string in str with the first character converted to\nuppercase. When the optional bool parameter evaluates to any value other than\nnil, the rest of the string is converted to lowercase.\n\n(title-case \"hello\")       \226\134\146 \"Hello\"\n(title-case \"hELLO\" true)  \226\134\146 \"Hello\"\n(title-case \"hELLO\")       \226\134\146 \"HELLO\"\n\nSee also the lower-case and upper-case functions.")

(set '_trace "{trace\n\nsyntax: (trace [bool])\n\nTracing is switched on when bool evaluates to anything other than nil. When no\nargument is supplied, trace evaluates to true or nil depending on the current\ntrace mode. If trace mode is switched on, newLISP goes into debugging mode\nafter entering the next user defined function, displaying the function and\nhighlighting the current expression upon entry and exit.\n\nHighlighting is done by bracketing the expression between two # (number sign)\ncharacters. This can be changed to a different character using trace-highlight.\nUpon exit from the expression, the result of its evaluation is also reported.\n\nIf an expression occurs more than once in a function, the first occurrence of\nthe executing function will always be highlighted (bracketed).\n\nnewLISP execution stops with a prompt line at each entry and exit of an\nexpression.\n\n[-> 2] s|tep n|ext c|ont q|uit >\n\nAt the prompt, an s, n, c, or q can be entered to step into or merely execute\nthe next expression. Any expression can be entered at the prompt for\nevaluation. Entering the name of a variable, for example, would evaluate to its\ncontents. In this way, a variable's contents can be checked during debugging or\nset to different values.\n\n;; switches newLISP into debugging mode\n(trace true)  \226\134\146 true\n\n;; the debugger will show each step\n(my-func a b c)\n\n;; switched newLISP out of debugging mode\n(trace nil)  \226\134\146 nil\n\nTo set break points where newLISP should interrupt normal execution and go into\ndebugging mode, put (trace true) statements into the newLISP code where\nexecution should switch on the debugger.\n\nUse the debug function as a shortcut for the above example.")

(set '_trace-highlight "{trace-highlight\n\nsyntax: (trace-highlight str-pre str-post [str-header str-footer])\n\nSets the characters or string of characters used to enclose expressions during\ntrace. By default, the # (number sign) is used to enclose the expression\nhighlighted in trace mode. This can be changed to different characters or\nstrings of up to seven characters. If the console window accepts terminal\ncontrol characters, this can be used to display the expression in a different\ncolor, bold, reverse, and so forth.\n\nTwo more strings can optionally be specified for str-header and str-footer,\nwhich control the separator and prompt. A maximum of 15 characters is allowed\nfor the header and 31 for the footer.\n\n;; active expressions are enclosed in >> and <<\n\n(trace-highlight \">>\" \"<<\")\n\n;; 'bright' color on a VT100 or similar terminal window\n\n(trace-highlight \"\\027[1m\" \"\\027[0m\")\n\nThe first example replaces the default # (number sign) with a >> and <<. The\nsecond example works on most Linux shells. It may not, however, work in console\nwindows under Win32 or CYGWIN, depending on the configuration of the terminal.")

(set '_transpose "{transpose\n\nsyntax: (transpose matrix)\n\nTransposes a matrix by reversing the rows and columns. Any kind of list-matrix\ncan be transposed. Matrices are made rectangular by filling in nil for missing\nelements, omitting elements where appropriate, or expanding atoms in rows into\nlists. Matrix dimensions are calculated using the number of rows in the\noriginal matrix for columns and the number of elements in the first row as\nnumber of rows for the transposed matrix.\n\nThe matrix to transpose can contain any data-type.\n\nThe dimensions of a matrix are defined by the number of rows and the number of\nelements in the first row. A matrix can either be a nested list or an array.\n\n(set 'A '((1 2 3) (4 5 6)))\n(transpose A)                      \226\134\146 ((1 4) (2 5) (3 6))\n(transpose (list (sequence 1 5)))  \226\134\146 ((1) (2) (3) (4) (5))\n\n; any data type is allowed in the matrix\n(transpose '((a b) (c d) (e f)))   \226\134\146 ((a c e) (b d f))\n\n; arrays can be transposed too\n(set 'A (array 2 3 (sequence 1 6)))\n(set 'M (transpose A))\nM \226\134\146 ((1 4) (2 5) (3 6))\n\nThe number of columns in a matrix is defined by the number of elements in the\nfirst row of the matrix. If other rows have fewer elements, transpose will\nassume nil for those missing elements. Superfluous elements in a row will be\nignored.\n\n(set 'A '((1 2 3) (4 5) (7 8 9)))\n\n(transpose A)  \226\134\146 ((1 4 7) (2 5 8) (3 nil 9))\n\nIf a row is any other data type besides a list, the transposition treats it\nlike an entire row of elements of that data type:\n\n(set 'A '((1 2 3) X (7 8 9)))\n\n(transpose A)  \226\134\146 ((1 X 7) (2 X 8) (3 X 9))\n\nAll operations shown here on lists can also be performed on arrays.\n\nSee also the matrix operations det, invert, mat and multiply.")

(set '_trim "{trim utf8\n\nsyntax: (trim str [str-char])\nsyntax: (trim str str-left-char str-right-char)\n\nThe first syntax trims the string str from both sides, stripping the leading\nand trailing characters as given in str-char. If str-char contains no\ncharacter, the space character is assumed. trim returns the new string.\n\nThe second syntax can either trim different characters from both sides or trim\nonly one side if an empty string is specified for the other.\n\n(trim \"   hello  \")              \226\134\146 \"hello\"\n(trim \"----hello-----\" \"-\")      \226\134\146 \"hello\"\n(trim \"00012340\" \"0\" \"\")         \226\134\146 \"12340\"\n(trim \"1234000\" \"\" \"0\")          \226\134\146 \"1234\"\n(trim \"----hello=====\" \"-\" \"=\")  \226\134\146 \"hello\"")

(set '_true? "{true?\n\nsyntax: (true? exp)\n\nIf the expression in exp evaluates to anything other than nil or the empty list\n(), true? returns true; otherwise, it returns nil.\n\n(map true? '(x 1 \"hi\" (a b c) nil ()))\n\226\134\146 (true true true true nil nil)\n(true? nil)  \226\134\146 nil\n(true? '())  \226\134\146 nil\n\ntrue? behaves like if and rejects the empty list ().")

(set '_unicode "{unicode\n\nsyntax: (unicode str-utf8)\n\nConverts ASCII/UTF-8 character strings in str to UCS-4\226\128\147encoded Unicode of\n4-byte integers per character. The string is terminated with a 4-byte integer\n0. This function is only available on UTF-8\226\128\147enabled versions of newLISP.\n\n(unicode \"new\")\n\226\134\146 \"n\\000\\000\\000e\\000\\000\\000w\\000\\000\\000\\000\\000\\000\\000\"\n\n(utf8 (unicode \"new\"))  \226\134\146 \"new\"\n\nOn big endian CPU architectures, the byte order will be reversed from high to\nlow. The unicode and utf8 functions are the inverse of each other. These\nfunctions are only necessary if UCS-4 Unicode is in use. Most systems use UTF-8\nencoding only.")

(set '_unify [text]{unify

syntax: (unify exp-1 exp-2 [list-env])

Evaluates and matches exp-1 and exp-2. Expressions match if they are equal or
if one of the expressions is an unbound variable (which would then be bound to
the other expression). If expressions are lists, they are matched by comparing
subexpressions. Unbound variables start with an uppercase character to
distinguish them from symbols. unify returns nil when the unification process
fails, or it returns a list of variable associations on success. When no
variables were bound, but the match is still successful, unify returns an empty
list. newLISP uses a modified J. Alan Robinson unification algorithm with
correctly applied occurs check. See also Peter Norvig's paper about a common
unifcation algorithm bug, which is not present in this implementation.

Since version 10.4.0 the underscore symbol _ (ASCII 95) matches any atom, list
or unbound variable and never binds.

Like match, unify is frequently employed as a parameter functor in find, ref,
ref-all and replace.

(unify 'A 'A)  → ()  ; tautology

(unify 'A 123)  → ((A 123))  ; A bound to 123

(unify '(A B) '(x y))  → ((A x) (B y))  ; A bound to x, B bound to y

(unify '(A B) '(B abc))  → ((A abc) (B abc))  ; B is alias for A

(unify 'abc 'xyz)  → nil  ; fails because symbols are different

(unify '(A A) '(123 456))  → nil  ; fails because A cannot be bound to different values

(unify '(f A) '(f B))  → ((A B))  ; A and B are aliases

(unify '(f A) '(g B))  → nil  ; fails because heads of terms are different

(unify '(f A) '(f A B))  → nil  ; fails because terms are of different arity

(unify '(f (g A)) '(f B))  → ((B (g A)))  ; B bound to (g A)

(unify '(f (g A) A) '(f B xyz))  → ((B (g xyz)) (A xyz))  ; B bound to (g xyz) A to xyz

(unify '(f A) 'A)  → nil  ; fails because of infinite unification (f(f(f …)))

(unify '(A xyz A) '(abc X X))  →  nil ; indirect alias A to X doesn't match bound terms

(unify '(p X Y a) '(p Y X X))  → '((Y a) (X a)))  ; X alias Y and binding to 'a

(unify '(q (p X Y) (p Y X)) '(q Z Z))  → ((Y X) (Z (p X X)))  ; indirect alias

(unify '(A b _) '(x G z)) → ((A x) (G b)) ; _ matches atom z

(unify '(A b c _) '(x G _ z)) → ((A x) (G b)) ; _ never binds, matches c and z

(unify '(A b _) '(x G (x y z))) → ((A x) (G b)) ; _ matches list (x y z)

;; some examples taken from http://en.wikipedia.org/wiki/Unification

unify can take an optional binding or association list in list-env. This is
useful when chaining unify expressions and the results of previous unify
bindings must be included:

(unify '(f X) '(f 123))  → ((X 123))

(unify '(A B) '(X A) '((X 123)))
→ ((X 123) (A 123) (B 123))

In the previous example, X was bound to 123 earlier and is included in the
second statement to pre-bind X.

Use unify with expand

Note that variables are not actually bound as a newLISP assignment. Rather, an
association list is returned showing the logical binding. A special syntax of
expand can be used to actually replace bound variables with their terms:

(set 'bindings (unify '(f (g A) A) '(f B xyz)))
→ ((B (g xyz)) (A xyz))

(expand '(f (g A) A) bindings)  → (f (g xyz) xyz)

; or in one statement
(expand '(f (g A) A) (unify '(f (g A) A) '(f B xyz)))
→ (f (g xyz) xyz)

Use unify with bind for de-structuring

The function bind can be used to set unified variables:

(bind (unify '(f (g A) A) '(f B xyz)))

A → xyz
B → (g xyz)

This can be used for de-structuring:

(set 'structure '((one "two") 3 (four (x y z))))
(set 'pattern '((A B) C (D E)))
(bind (unify pattern structure))

A → one
B → "two"
C → 3
D → four
E → (x y z)

unify returns an association list and bind binds the associations.

Model propositional logic with unify

The following example shows how propositional logic can be modeled using unify
and expand:

; if somebody is human, he is mortal -> (X human) :- (X mortal)
; socrates is human -> (socrates human)
; is socrates mortal? -> ?  (socrates mortal)

(expand '(X mortal)
         (unify '(X human) '(socrates human)))
→ (socrates mortal)

The following is a more complex example showing a small, working PROLOG
(Programming in Logic) implementation.

;; a small PROLOG implementation

(set 'facts '(
    (socrates philosopher)
    (socrates greek)
    (socrates human)
    (einstein german)
    (einstein (studied physics))
    (einstein human)
))

(set 'rules '(
    ((X mortal) <- (X human))
    ((X (knows physics)) <- (X physicist))
    ((X physicist) <- (X (studied physics)))
))


(define (query trm)
    (or  (if (find trm facts) true) (catch (prove-rule trm))))

(define (prove-rule trm)
    (dolist (r rules)
        (if (list? (set 'e (unify trm (first r))))
            (if (query (expand (last r) e))
                (throw true))))
    nil
)

; try it

> (query '(socrates human))
true
> (query '(socrates (knows physics)))
nil
> (query '(einstein (knows physics)))
true

The program handles a database of facts and a database of simple A is a fact if
B is a fact rules. A fact is proven true if it either can be found in the facts
database or if it can be proven using a rule. Rules can be nested: for example,
to prove that somebody (knows physics), it must be proved true that somebody is
a physicist. But somebody is only a physicist if that person studied physics.
The <- symbol separating the left and right terms of the rules is not required
and is only added to make the rules database more readable.

This implementation does not handle multiple terms in the right premise part of
the rules, but it does handle backtracking of the rules database to try out
different matches. It does not handle backtracking in multiple premises of the
rule. For example, if in the following rule A if B and C and D, the premises B
and C succeed and D fails, a backtracking mechanism might need to go back and
reunify the B or A terms with different facts or rules to make D succeed.

The above algorithm could be written differently by omitting expand from the
definition of prove-rule and by passing the environment, e, as an argument to
the unify and query functions.

A learning of proven facts can be implemented by appending them to the facts
database once they are proven. This would speed up subsequent queries.

Larger PROLOG implementations also allow the evaluation of terms in rules. This
makes it possible to implement functions for doing other work while processing
rule terms. prove-rule could accomplish this testing for the symbol eval in
each rule term.[/text])

(set '_union "{union\n\nsyntax: (union list-1 list-2 [list-3 ... ])\n\nunion returns a unique collection list of distinct elements found in two or\nmore lists.\n\n(union '(1 3 1 4 4 3) '(2 1 5 6 4))  \226\134\146  (1 3 4 2 5 6)\n\nLike the other set functions difference, intersect and unique, union maintains\nthe order of elements as found in the original lists.")

(set '_unique "{unique\n\nsyntax: (unique list)\n\nReturns a unique version of list with all duplicates removed.\n\n(unique '(2 3 4 4 6 7 8 7))  \226\134\146 (2 3 4 6 7 8)\n\nNote that the list does not need to be sorted, but a sorted list makes unique\nperform faster.\n\nOther set functions are difference, intersect and union.")

(set '_unless "{unless\n\nsyntax: (unless exp-condition body)\n\nThe statements in body are only evaluated if exp-condition evaluates to nil or\nthe empty list (). The result of the last expression in body is returned or nil\nor the empty list () if body was not executed.\n\nBecause unless does not have an else condition as in if, the statements in body\nneed not to be grouped with begin:\n\n(unless (starts-with (read-line) \"quit\")\n        (process (current-line))\n        ...\n        (finish)\n)\n\nSee also the function when.")

(set '_unpack [text]{unpack

syntax: (unpack str-format str-addr-packed)
syntax: (unpack str-format num-addr-packed)

syntax: (unpack struct num-addr-packed)
syntax: (unpack struct str-addr-packed)

When the first parameter is a string, unpack unpacks a binary structure in
str-addr-packed or pointed to by num-addr-packed into newLISP variables using
the format in str-format. unpack is the reverse operation of pack. Using
num-addr-packed facilitates the unpacking of structures returned from imported,
shared library functions.

If the number specified in num-addr-packed is not a valid memory address, a
system bus error or segfault can occur and crash newLISP or leave it in an
unstable state.

When the first parameter is the symbol of a struct definition, unpack uses the
format as specified in struct. While unpack with str-format literally unpacks
as specified, unpack with struct will skip structure aligning pad-bytes
depending on data type, order of elements and CPU architecture. Refer to the
description of the struct function for more detail.

The following characters may define a format:

format    description
c         a signed 8-bit number
b         an unsigned 8-bit number
d         a signed 16-bit short number
u         an unsigned 16-bit short number
ld        a signed 32-bit long number
lu        an unsigned 32-bit long number
Ld        a signed 64-bit long number
Lu        an unsigned 64-bit long number
f         a float in 32-bit representation
lf        a double float in 64-bit representation
sn        a string of n null padded ASCII characters
nn        n null characters
>         switches to big endian byte order
<         switches to little endian byte order


(pack "c c c" 65 66 67)  → "ABC"
(unpack "c c c" "ABC")   → (65 66 67)

(set 's (pack "c d u" 10 12345 56789))
(unpack "c d u" s)  → (10 12345 56789)

(set 's (pack "s10 f" "result" 1.23))
(unpack "s10 f" s)  → ("result\000\000\000\000" 1.230000019)

(set 's (pack "s3 lf" "result" 1.23))
(unpack "s3 f" s)  → ("res" 1.23)

(set 's (pack "c n7 c" 11 22))
(unpack "c n7 c" s)  → (11 22))

The > and < specifiers can be used to switch between little endian and big
endian byte order when packing or unpacking:

;; on a little endian system (e.g., Intel CPUs)
(set 'buff (pack "d" 1))  → "\001\000"

(unpack "d" buff)   → (1)
(unpack ">d" buff)  → (256)

Switching the byte order will affect all number formats with 16-, 32-, or
64-bit sizes.

The pack and unpack format need not be the same, as in the following example:

(set 's (pack "s3" "ABC"))
(unpack "c c c" s)  → (65 66 67)

The examples show spaces between the format specifiers. Although not required,
they can improve readability.

If the buffer's size at a memory address is smaller than the formatting string
specifies, some formatting characters may be left unused.

See also the address, get-int, get-long, get-char, get-string, and pack
functions.[/text])

(set '_until "{until\n\nsyntax: (until exp-condition [body])\n\nEvaluates the condition in exp-condition. If the result is nil or the empty\nlist (), the expressions in body are evaluated. Evaluation is repeated until\nthe exp-condition results in a value other than nil or the empty list. The\nresult of the last expression evaluated in body is the return value of the\nuntil expression. If body is empty, the result of last exp-condition is\nreturned. until works like (while (not \226\128\166)).\n\nuntil also updates the system iterator symbol $idx.\n\n(device (open \"somefile.txt\" \"read\"))\n(set 'line-count 0)\n(until (not (read-line)) (inc line-count))\n(close (device))\n(print \"the file has \" line-count \" lines\\n\")\n\nUse the do-until function to test the condition after evaluation of the body\nexpressions.")

(set '_upper-case "{upper-case utf8\n\nsyntax: (upper-case str)\n\nReturns a copy of the string in str converted to uppercase. International\ncharacters are converted correctly.\n\n(upper-case \"hello world\")  \226\134\146 \"HELLO WORLD\"\n\nSee also the lower-case and title-case functions.")

(set '_utf8 "{utf8\n\nsyntax: (utf8 str-unicode)\n\nConverts a UCS-4, 4-byte, Unicode-encoded string (str) into UTF-8. This\nfunction is only available on UTF-8\226\128\147enabled versions of newLISP.\n\n(unicode \"new\")\n\226\134\146 \"n\\000\\000\\000e\\000\\000\\000w\\000\\000\\000\\000\\000\\000\\000\"\n\n(utf8 (unicode \"new\"))  \226\134\146 \"new\"\n\nThe utf8 function can also be used to test for the presence of UTF-8\226\128\147enabled\nnewLISP:\n\n(if utf8 (do-utf8-version-of-code) (do-ascii-version-of-code))\n\nOn big endian CPU architectures, the byte order will be reversed from highest\nto lowest. The utf8 and unicode functions are the inverse of each other. These\nfunctions are only necessary if UCS-4 Unicode is in use. Most systems use UTF-8\nUnicode encoding only.")

(set '_utf8len "{utf8len\n\nsyntax: (utf8len str)\n\nReturns the number of characters in a UTF-8\226\128\147encoded string. UTF-8 characters\ncan be encoded in more than one 8-bit byte. utf8len returns the number of UTF-8\ncharacters in a string. This function is only available on UTF-8\226\128\147enabled\nversions of newLISP.\n\n(utf8len \"\230\136\145\232\131\189\229\144\158\228\184\139\231\142\187\231\146\131\232\128\140\228\184\141\228\188\164\232\186\171\228\189\147\227\128\130\")    \226\134\146 12\n(length \"\230\136\145\232\131\189\229\144\158\228\184\139\231\142\187\231\146\131\232\128\140\228\184\141\228\188\164\232\186\171\228\189\147\227\128\130\")      \226\134\146 36\n\nSee also the unicode and utf8 functions. Above Chinese text from UTF-8 Sampler.")

(set '_uuid "{uuid\n\nsyntax: (uuid [str-node])\n\nConstructs and returns a UUID (Universally Unique IDentifier). Without a node\nspec in str-node, a type 4 UUID random generated byte number is returned. When\nthe optional str-node parameter is used, a type 1 UUID is returned. The string\nin str-node specifies a valid MAC (Media Access Code) from a network adapter\ninstalled on the node or a random node ID. When a random node ID is specified,\nthe least significant bit of the first node byte should be set to 1 to avoid\nclashes with real MAC identifiers. UUIDs of type 1 with node ID are generated\nfrom a timestamp and other data. See RFC 4122 for details on UUID generation.\n\n;; type 4 UUID for any system\n\n(uuid)  \226\134\146 \"493AAD61-266F-48A9-B99A-33941BEE3607\"\n\n;; type 1 UUID preferred for distributed systems\n\n;; configure node ID for ether 00:14:51:0a:e0:bc\n(set 'id (pack \"cccccc\" 0x00 0x14 0x51 0x0a 0xe0 0xbc))\n\n(uuid  id)  \226\134\146 \"0749161C-2EC2-11DB-BBB2-0014510AE0BC\"\n\nEach invocation of the uuid function will yield a new unique UUID. The UUIDs\nare generated without system-wide shared stable store (see RFC 4122). If the\nsystem generating the UUIDs is distributed over several nodes, then type 1\ngeneration should be used with a different node ID on each node. For several\nprocesses on the same node, valid UUIDs are guaranteed even if requested at the\nsame time. This is because the process ID of the generating newLISP process is\npart of the seed for the random number generator. When type 4 IDs are used on a\ndistributed system, two identical UUID's are still highly unlikely and\nimpossible for type 1 IDs if real MAC addresses are used.")

(set '_wait-pid "{wait-pid\n\nsyntax: (wait-pid int-pid [int-options | nil])\n\nWaits for a child process specified in int-pid to end. The child process was\npreviously started with process or fork. When the child process specified in\nint-pid ends, a list of pid and status value is returned. The status value\ndescribes the reason for termination of the child process. The interpretation\nof the returned status value differs between Linux and other flavors of Unix.\nConsult the Linux/Unix man pages for the waitpid command (without the hyphen\nused in newLISP) for further information.\n\nWhen -1 is specified for int-pid, pid and status information of any child\nprocess started by the parent are returned. When 0 is specified, wait-pid only\nwatches child processes in the same process group as the calling process. Any\nother negative value for int-pid reports child processes in the same process\ngroup as specified with a negative sign in int-pid.\n\nAn option can be specified in int-option. See Linux/Unix documentation for\ndetails on integer values for int-options. As an alternative, nil can be\nspecified. This option causes wait-pid to be non-blocking, returning right away\nwith a 0 in the pid of the list returned. This option used together with an\nint-pid parameter of -1 can be used to continuously loop and act on returned\nchild processes.\n\nThis function is only available on Mac OS X, Linux and other Unix-like\noperating systems.\n\n(set 'pid (fork (my-process))) \226\134\146 8596\n\n(set 'ret (wait-pid pid))  \226\134\146 (8596 0) ; child has exited\n\n(println \"process: \" pid \" has finished with status: \" (last ret))\n\nThe process my-process is started, then the main program blocks in the wait-pid\ncall until my-process has finished.")

(set '_when "{when\n\nsyntax: (when exp-condition body)\n\nThe statements in body are only evaluated if exp-condition evaluates to\nanything not nil and not the empty list (). The result of the last expression\nin body is returned or nil or the empty list () if body was not executed.\n\nBecause when does not have an else condition as in if, the statements in body\nneed not to be grouped with begin:\n\n(when (read-line)\n        (set 'result (analyze (current-line)))\n        (report result)\n        (finish)\n)\n\nSee also the function unless working like (when (not ...) ...).")

(set '_while "{while\n\nsyntax: (while exp-condition body)\n\nEvaluates the condition in exp-condition. If the result is not nil or the empty\nlist (), the expressions in body are evaluated. Evaluation is repeated until an\nexp-condition results in nil or the empty list (). The result of the body's\nlast evaluated expression is the return value of the while expression.\n\nwhile also updates the system iterator symbol $idx.\n\n(device (open \"somefile.txt\" \"read\"))\n(set 'line-count 0)\n(while (read-line) (inc line-count))\n(close (device))\n(print \"the file has \" line-count \" lines\\n\")\n\nUse the do-while function to evaluate the condition after evaluating the body\nof expressions.")

(set '_write "{write !\n\nsyntax: (write int-file str-buffer [int-size])\nsyntax: (write str str-buffer [int-size])\n\nThe function write writes int-size bytes from a buffer in str-buffer to a file\nspecified in int-file, previously obtained from a file open operation. If\nint-size is not specified, all data in sym-buffer or str-buffer is written.\nwrite returns the number of bytes written or nil on failure.\n\nwrite is a shorter writing of write-buffer. The longer form still works but is\ndeprecated and should be avoided in new code.\n\n(set 'handle (open \"myfile.ext\" \"write\"))\n(write handle data 100)\n(write handle \"a quick message\\n\")\n\nThe code in the example writes 100 bytes to the file myfile.ext from the\ncontents in data.\n\nIn the second syntax, write can be used for destructive string appending:\n\n(set 'str \"\")\n(write str \"hello world\")\n\nstr   \226\134\146 \"hello world\"\n\nSee also the read function.")

(set '_write-char "{write-char\n\nsyntax: (write-char int-file int-byte1 [int-byte2 ... ])\n\nWrites a byte specified in int-byte to a file specified by the file handle in\nint-file. The file handle is obtained from a previous open operation. Each\nwrite-char advances the file pointer by one 8-bit byte.\n\nwrite-char returns the number of bytes written.\n\n(define (slow-file-copy from-file to-file)\n    (set 'in-file (open from-file \"read\"))\n    (set 'out-file (open to-file \"write\"))\n    (while (set 'chr (read-char in-file))\n        (write-char out-file chr))\n     (close in-file)\n    (close out-file)\n    \"finished\")\n\nUse the print and device functions to write larger portions of data at a time.\nNote that newLISP already supplies a faster built-in function called copy-file.\n\nSee also the read-char function.")

(set '_write-file "{write-file\n\nsyntax: (write-file str-file-name str-buffer)\n\nWrites a file in str-file-name with contents in str-buffer in one swoop and\nreturns the number of bytes written.\n\nOn failure the function returns nil. For error information, use sys-error when\nused on files. When used on URLs net-error gives more error information.\n\n(write-file \"myfile.enc\"\n    (encrypt (read-file \"/home/lisp/myFile\") \"secret\"))\n\nThe file myfile is read, encrypted using the password secret, and written back\ninto the new file myfile.enc in the current directory.\n\nwrite-file can take an http:// or file:// URL in str-file-name. When the prefix\nhttp:// is used, write-file works exactly like put-url and can take the same\nadditional parameters:\n\n(write-file \"http://asite.com/message.txt\" \"This is a message\" )\n\nThe file message.txt is created and written at a remote location, http://\nasite.com, with the contents of str-buffer. In this mode, write-file can also\nbe used to transfer files to remote newLISP server nodes.\n\nSee also the append-file and read-file functions.")

(set '_write-line "{write-line !\n\nsyntax: (write-line [int-file [str]])\nsyntax: (write-line str-out [str]])\n\nThe string in str and the line termination character(s) are written to the\ndevice specified in int-file. When the string argument is omitted write-line\nwrites the contents of the last read-line to int-file If the first argument is\nomitted too then it writes to to standard out (STDOUT) or to whatever device is\nset by device.\n\nIn the second syntax lines are appended to a string in str-out.\n\nwrite-line returns the number of bytes written.\n\n(set 'out-file (open \"myfile\" \"write\"))\n(write-line out-file \"hello there\")\n(close out-file)\n\n(set 'myFile (open \"init.lsp\" \"read\")\n(while (read-line myFile) (write-line))\n\n(set 'str \"\")\n(write-line str \"hello\")\n(write-line str \"world\")\n\nstr  \226\134\146  \"hello\\nworld\\n\"\n\nThe first example opens/creates a file, writes a line to it, and closes the\nfile. The second example shows the usage of write-line without arguments. The\ncontents of init.lsp are written to the console screen.\n\nSee also the function write for writing to a device without the\nline-terminating character.")

(set '_xfer-event "{xfer-event\n\nsyntax: (xfer-event sym-event-handler | func-event-handler)\n\nRegisters a function in symbol sym-event-handler or in lambda function\nfunc-event-handler to monitor HTTP byte transfers initiated by get-url,\npost-url or put-url or initiated by file functions which can take URLs like\nload, save, read-file, write-file and append-file.\n\nE.g. whenever a block of data requested with get-url arrives the function in\nsym or func will be called with the number of bytes transferred. Likewise when\nsending data with post-url or any of the other data sending functions, sym or\nfunc will be called with the number of bytes transferred for each block of data\ntransferred.\n\n(xfer-event (fn (n) (println \"->\" n)))\n(length (get-url \"http://newlisp.org\"))\n\n->73\n->799\n->1452\n->351\n->1093\n->352\n->211\n->885\n->564\n->884\n->561\n->75\n->812\n->638\n->1452\n->801\n->5\n->927\n11935\n\n\nThe computer output is shown in bold. Whenever a block of data is received its\nbyte size is printed. Instead of defining the handler function directory with a\nlambda function in func, a symbol containing a function definition could have\nbeen used:\n\n(define (report n) (println \"->\" n))\n(xfer-event 'report)\n\nThis can be used to monitor the progress of longer lasting byte transfers in\nHTTP uploads or downloads.")

(set '_xml-error "{xml-error\n\nsyntax: (xml-error)\n\nReturns a list of error information from the last xml-parse operation;\notherwise, returns nil if no error occurred. The first element contains text\ndescribing the error, and the second element is a number indicating the last\nscan position in the source XML text, starting at 0 (zero).\n\n(xml-parse \"<atag>hello</atag><fin\")  \226\134\146 nil\n\n(xml-error)  \226\134\146 (\"expected closing tag: >\" 18)")

(set '_xml-parse [text]{xml-parse

syntax: (xml-parse string-xml [int-options [sym-context [func-callback]]])

Parses a string containing XML 1.0 compliant, well-formed XML. xml-parse does
not perform DTD validation. It skips DTDs (Document Type Declarations) and
processing instructions. Nodes of type ELEMENT, TEXT, CDATA, and COMMENT are
parsed, and a newLISP list structure is returned. When an element node does not
have attributes or child nodes, it instead contains an empty list. Attributes
are returned as association lists, which can be accessed using assoc. When
xml-parse fails due to malformed XML, nil is returned and xml-error can be used
to access error information.

(set 'xml
  "<person name='John Doe' tel='555-1212'>nice guy</person>")

(xml-parse xml)
→ (("ELEMENT" "person"
    (("name" "John Doe")
     ("tel" "555-1212"))
    (("TEXT" "nice guy"))))

Modifying the translation process.

Optionally, the int-options parameter can be specified to suppress whitespace,
empty attribute lists, and comments. It can also be used to transform tags from
strings into symbols. Another function, xml-type-tags, serves for translating
the XML tags. The following option numbers can be used:

option description
1      suppress whitespace text tags
2      suppress empty attribute lists
4      suppress comment tags
8      translate string tags into symbols
16     add SXML (S-expression XML) attribute tags


Options can be combined by adding the numbers (e.g., 3 would combine the
options for suppressing whitespace text tags/info and empty attribute lists).

The following examples show how the different options can be used:


XML source:

<?xml version="1.0" ?>
<DATABASE name="example.xml">
<!--This is a database of fruits-->
    <FRUIT>
        <NAME>apple</NAME>
        <COLOR>red</COLOR>
        <PRICE>0.80</PRICE>
    </FRUIT>

    <FRUIT>
        <NAME>orange</NAME>
        <COLOR>orange</COLOR>
        <PRICE>1.00</PRICE>
    </FRUIT>

    <FRUIT>
       <NAME>banana</NAME>
       <COLOR>yellow</COLOR>
       <PRICE>0.60</PRICE>
    </FRUIT>
</DATABASE>

Parsing without any options:

(xml-parse (read-file "example.xml"))
→ (("ELEMENT" "DATABASE" (("name" "example.xml")) (("TEXT" "\r\n\t")
    ("COMMENT" "This is a database of fruits")
    ("TEXT" "\r\n\t")
    ("ELEMENT" "FRUIT" () (("TEXT" "\r\n\t\t") ("ELEMENT" "NAME" ()
       (("TEXT" "apple")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "COLOR" () (("TEXT" "red")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "PRICE" () (("TEXT" "0.80")))
      ("TEXT" "\r\n\t")))
    ("TEXT" "\r\n\r\n\t")
    ("ELEMENT" "FRUIT" () (("TEXT" "\r\n\t\t") ("ELEMENT" "NAME" ()
       (("TEXT" "orange")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "COLOR" () (("TEXT" "orange")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "PRICE" () (("TEXT" "1.00")))
      ("TEXT" "\r\n\t")))
    ("TEXT" "\r\n\r\n\t")
    ("ELEMENT" "FRUIT" () (("TEXT" "\r\n\t\t") ("ELEMENT" "NAME" ()
       (("TEXT" "banana")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "COLOR" () (("TEXT" "yellow")))
      ("TEXT" "\r\n\t\t")
      ("ELEMENT" "PRICE" () (("TEXT" "0.60")))
      ("TEXT" "\r\n\t")))
    ("TEXT" "\r\n"))))

The TEXT elements containing only whitespace make the output very confusing. As
the database in example.xml only contains data, we can suppress whitespace,
empty attribute lists and comments with option (+ 1 2 4):

Filtering whitespace TEXT, COMMENT tags, and empty attribute lists:

(xml-parse (read-file "example.xml") (+ 1 2 4))
→ (("ELEMENT" "DATABASE" (("name" "example.xml")) (
     ("ELEMENT" "FRUIT" (
       ("ELEMENT" "NAME" (("TEXT" "apple")))
       ("ELEMENT" "COLOR" (("TEXT" "red")))
       ("ELEMENT" "PRICE" (("TEXT" "0.80")))))
     ("ELEMENT" "FRUIT" (
       ("ELEMENT" "NAME" (("TEXT" "orange")))
       ("ELEMENT" "COLOR" (("TEXT" "orange")))
       ("ELEMENT" "PRICE" (("TEXT" "1.00")))))
     ("ELEMENT" "FRUIT" (
       ("ELEMENT" "NAME" (("TEXT" "banana")))
       ("ELEMENT" "COLOR" (("TEXT" "yellow")))
       ("ELEMENT" "PRICE" (("TEXT" "0.60"))))))))

The resulting output looks much more readable, but it can still be improved by
using symbols instead of strings for the tags "FRUIT", "NAME", "COLOR", and
"PRICE", as well as by suppressing the XML type tags "ELEMENT" and "TEXT"
completely using the xml-type-tags directive.

Suppressing XML type tags with xml-type-tags and translating string tags into
symbol tags:

;; suppress all XML type tags for TEXT and ELEMENT
;; instead of "CDATA", use cdata and instead of "COMMENT", use !--

(xml-type-tags nil 'cdata '!-- nil)

;; turn on all options for suppressing whitespace and empty
;; attributes, translate tags to symbols

(xml-parse (read-file "example.xml") (+ 1 2 8))
→ ((DATABASE (("name" "example.xml"))
     (!-- "This is a database of fruits")
     (FRUIT (NAME "apple") (COLOR "red") (PRICE "0.80"))
     (FRUIT (NAME "orange") (COLOR "orange") (PRICE "1.00"))
     (FRUIT (NAME "banana") (COLOR "yellow") (PRICE "0.60"))))

When tags are translated into symbols by using option 8, a context can be
specified in sym-context. If no context is specified, all symbols will be
created inside the current context.

(xml-type-tags nil nil nil nil)
(xml-parse "<msg>Hello World</msg>" (+ 1 2 4 8 16) 'CTX)
→ ((CTX:msg "Hello World"))

Specifying nil for the XML type tags TEXT and ELEMENT makes them disappear. At
the same time, parentheses of the child node list are removed so that child
nodes now appear as members of the list, starting with the tag symbol
translated from the string tags "FRUIT", "NAME", etcetera.

Parsing into SXML (S-expressions XML) format:

Using xml-type-tags to suppress all XML-type tags—along with the option numbers
1, 2, 4, 8, and 16—SXML formatted output can be generated:

(xml-type-tags nil nil nil nil)
(xml-parse (read-file "example.xml") (+ 1 2 4 8 16))
→ ((DATABASE (@ (name "example.xml"))
    (FRUIT (NAME "apple") (COLOR "red") (PRICE "0.80"))
    (FRUIT (NAME "orange") (COLOR "orange") (PRICE "1.00"))
    (FRUIT (NAME "banana") (COLOR "yellow") (PRICE "0.60"))))

Note that using option number 16 causes an @ (at symbol) to be added to
attribute lists.

See also the xml-type-tags function for further information on XML parsing.

Parsing into a specified context

When parsing XML expressions, XML tags are translated into newLISP symbols. The
sym-context option specifies the target context for the symbol creation:

(xml-type-tags nil nil nil nil)
(xml-parse (read-file "example.xml") (+ 1 2 4 8 16) 'CTX)
→((CTX:DATABASE (@ (CTX:name "example.xml"))
    (CTX:FRUIT (CTX:NAME "apple") (CTX:COLOR "red") (CTX:PRICE "0.80"))
    (CTX:FRUIT (CTX:NAME "orange") (CTX:COLOR "orange") (CTX:PRICE "1.00"))
    (CTX:FRUIT (CTX:NAME "banana") (CTX:COLOR "yellow") (CTX:PRICE "0.60"))))

If the context does not exist, it will be created. If it exists, the quote can
be omitted or the context can be referred to by a variable.

Using a call back function

Normally, xml-parse will not return until all parsing has finished. Using the
func-callback option, xml-parse will call back after each tag closing with the
generated S-expression and a start position and length in the source XML:

;; demo callback feature
(define (xml-callback s-expr start size)
    (if (or (= (s-expr 0) 'NAME) (= (s-expr 0) 'COLOR) (= (s-expr 0) 'PRICE))
        (begin
            (print "parsed expression:" s-expr)
            (println ", source:" (start size example-xml))
        )
    )
)

(xml-type-tags nil 'cdata '!-- nil)
(xml-parse  (read-file "example.xml") (+ 1 2 8) MAIN xml-callback)

The following output will be generated by the callback function xml-callback:

parsed expression:(NAME "apple"), source:<NAME>apple</NAME>
parsed expression:(COLOR "red"), source:<COLOR>red</COLOR>
parsed expression:(PRICE "0.80"), source:<PRICE>0.80</PRICE>
parsed expression:(NAME "orange"), source:<NAME>orange</NAME>
parsed expression:(COLOR "orange"), source:<COLOR>orange</COLOR>
parsed expression:(PRICE "1.00"), source:<PRICE>1.00</PRICE>
parsed expression:(NAME "banana"), source:<NAME>banana</NAME>
parsed expression:(COLOR "yellow"), source:<COLOR>yellow</COLOR>
parsed expression:(PRICE "0.60"), source:<PRICE>0.60</PRICE>

The example callback handler function filters the tags of interest and
processes them as they occur.[/text])

(set '_xml-type-tags "{xml-type-tags\n\nsyntax: (xml-type-tags [exp-text-tag exp-cdata-tag exp-comment-tag \nexp-element-tags])\n\nCan suppress completely or replace the XML type tags \"TEXT\", \"CDATA\",\n\"COMMENT\", and \"ELEMENT\" with something else specified in the parameters.\n\nNote that xml-type-tags only suppresses or translates the tags themselves but\ndoes not suppress or modify the tagged information. The latter would be done\nusing option numbers in xml-parse.\n\nUsing xml-type-tags without arguments returns the current type tags:\n\n(xml-type-tags)  \226\134\146 (\"TEXT\" \"CDATA\" \"COMMENT\" \"ELEMENT\")\n\n(xml-type-tags nil 'cdata '!-- nil)\n\nThe first example just shows the currently used type tags. The second example\nspecifies suppression of the \"TEXT\" and \"ELEMENT\" tags and shows cdata and !--\ninstead of \"CDATA\" and \"COMMENT\".")

(set '_zero? "{zero?\n\nsyntax: (zero? exp)\n\nChecks the evaluation of exp to see if it equals 0 (zero).\n\n(set 'value 1.2)\n(set 'var 0)\n(zero? value)  \226\134\146 nil\n(zero? var)    \226\134\146 true\n\n(map zero? '(0 0.0 3.4 4))  \226\134\146 (true true nil nil)\n\nzero? will return nil on data types other than numbers.")

(set '_| "{|\n\nsyntax: (| int-1 int-2 [int-3 ... ])\n\nA bitwise or operation is performed on the number in int-1 with the number in\nint-2, then successively with int-3, etc.\n\n(| 0x10 0x80 2 1)  \226\134\146 147")

(set '_~ "{~\n\nsyntax: (~ int)\n\nA bitwise not operation is performed on the number in int, reversing all of the\nbits.\n\n(format \"%X\" (~ 0xFFFFFFAA))  \226\134\146 \"55\"\n(~ 0xFFFFFFFF)                \226\134\146 0")


(context MAIN)

