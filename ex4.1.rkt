; Chapter 4.1 The Metacircular Evaluator
#lang sicp

; Note that eval needs an environment, for which you could
; use (scheme-report-environment 5)


; Ex 4.1 Do the exercise here before errors are thrown for
;        unbound identifiers, below
(define (list-of-values-left exps env)
  (define first 0)
  ; Return a list of the evaluated expressions from left to right
  (if (null? exps)
      '()
      (begin
        (set! first (eval (car exps) env))
        (cons first
              (list-of-values-left (cdr exps)
                                   env)))))


(list-of-values-left '((display 1) (display 2) (display 3)) (scheme-report-environment 5))
; Should display 1 2 3



(define (list-of-values-right exps env)
  ; Return a list of the evaluated expressions from right to left
  (if (null? exps)
      '()
      (let ((rest (list-of-values-right (cdr exps)
                                   env)))
        (cons (eval (car exps) env)
              rest))))


(list-of-values-right '((display 1) (display 2) (display 3)) (scheme-report-environment 5))
; Should display 3 2 1

(define (begin-body)
  (begin
    (define a 1)
    a))

(begin-body)

(define (begin-epxression)
  (if #t
      (begin
        ; (define a 1) ; <- not allowed!
        ;a
        1)
      0))

