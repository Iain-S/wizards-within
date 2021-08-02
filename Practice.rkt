#lang sicp

; Moving around

;; M-> : Move to end of file

;; M-C-f / M-C-b : move forward/back one S expressions.

;; M-C-u : move up, out of an S expression.

;; M-C-SPACE : select forward an S expression.

; So that we can import them in hash-require.rkt
(#%provide balance)
(#%provide bnew-withdraw)

(define balance 101)

(define (withdraw amount)
  (if (> amount balance)
      "Insufficient funds!"
      (begin (set! balance (- balance amount))  ; set <name> <new-value>
             balance)))

; Editing

;; M-d : delete forward one word

;; M-Backspace : delete backwards one word

;; ESC,t : transpose words (no need to highlight)
;me swap

;; M-( / [ / { : wrap selection in parens

;;
(+ 1 2)

;try to delete these words

(define bbalance 500)
(define (bnew-withdraw) 
    (lambda (amount)
      (if (>= bbalance amount)
          (begin (set! bbalance 
                       (- bbalance amount))
                 bbalance)
          "Insufficient funds")))
((bnew-withdraw) 50)


; M-< : Move to start of file 