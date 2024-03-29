# Structure and Interpretation of Computer Programs

![A wizard shooting lightning](wizard-gif-5.gif)

The full book is available online from MIT Press for free [here](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), though I prefer the formatting and footnotes of [this](https://sarabander.github.io/sicp/) version.

## Setup

There are other ways to work through SICP, like using Emacs or repl.it but I have found DrRacket to work best for me.

1. Install [DrRacket](https://download.racket-lang.org), the Racket language IDE.
1. The DrRacket SICP [support package](https://github.com/sicp-lang/sicp) may be required.
1. Use `#lang sicp` at the top of any new .rkt file.

## Organisation

One file per chapter, or sub-chapter, seems to be a good compromise: if you try to fit more than that you get too many naming conflicts.

## Testing

Doing TDD for the exercises can be done with some test helpers:

```scheme
; Exercise x.y: Try to write a Fibonacci function.

;; My function (not a very good one, at that).
(define (my-fib n)
    7)

;; A test helper. It is easy to write similar ones for lists, symbols, etc.
(define (assert-numbers-equal expected actual)
  (if (= expected actual)
      ; If expected == actual, the test has passed and we return #t...
      #t
      ; ...else, the test has failed.
      ; Fist, we print helpful output...
      (begin
        (display "expected: ")
        (display expected)
        (display "  actual: ")
        (display actual)
        (newline)
        ; ...and only then return #f or raise an error, etc.
        #f)))


;; Optionally, use a string to name the test.
"Test 1: Check Fib(9) == 21"
(assert-numbers-equal (my-fib 9) 21)
```

The above will print something along these lines, in the output

```text
"Test 1: Check Fib(9) == 21"
expected: 7  actual: 21
#f
```

## Imports

It took some trial and error to find that you can import functions defined in other files like so

**provide-hello.rkt**

```scheme
#lang sicp

(#%provide hello)

(define (hello)
  "Hello, World"
  )
```


and use it like this

**require-hello.rkt**

```scheme
#lang sicp

; Remember to start DrRacket from the right directory for this to work
(#%require "provide-hello.rkt")

(hello)
```

See also, https://docs.racket-lang.org/sicp-manual/SICP_Language.html
