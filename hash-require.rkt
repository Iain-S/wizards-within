#lang sicp

; As long as you start drracket from this source code directory,
; you should be able to import another SICP file like so
; https://docs.racket-lang.org/sicp-manual/SICP_Language.html
(#%require "Practice.rkt")



; Now we can reference variables and functions from Practice.rkt
balance
(null? '())
((bnew-withdraw) 1)

; This must be a Racket lang function, because it is not recognised.
;(current-directory)