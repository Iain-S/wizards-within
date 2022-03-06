; Chapter 4.1 The Metacircular Evaluator
#lang sicp

; Note that eval needs an environment, for which you could
; use (scheme-report-environment 5)

; 4.1.1 Core of the Evaluator

(define (eval exp env)
  ; Evaluate an expression in an environment
  (cond ((self-evaluating? exp)
         ; Such as numbers
         exp)
        ((variable? exp)
         ; Such as x
         (lookup-variable-value exp env))
        ((quoted? exp)
         ; Such as (quote a)
         (text-of-quotation exp))
        ((assignment? exp)
         ; todo Such as (set! x 1)
         (eval-assignment exp env))
        ((definition? exp)
         ; todo Such as (define x 1)
         (eval-definition exp env))
        ((if? exp)
         ; Such as (if #t 4 5)
         (eval-if exp env))
        ((lambda? exp)
         ; Such as (lambda (x) (+ x 1))
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         ; Such as (begin 1 2)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         ; Such as (cond (#t 4) (else 5)) 
         (eval (cond->if exp) env))
        ((application? exp)
         ; Such as (myproc 88 99)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression type." exp))))


(define (apply proc args)
  ; Apply procedure to arguments
  (cond ((primative-procedure? proc)
         ; Such as *
         (apply-primative-procedure
          proc
          args))
        ((compound-procedure? proc)
         ; Such as myproc
         (eval-sequence
          ; Evaluate the procedure body...
          (procedure-body proc)
          (extend-environment
           ; ...in an extended environment
           (procedure-parameters proc)
           args
           (procedure-environent proc))))
        (else (error "Unknown procedure type" proc))))


(define (list-of-values exps env)
  ; Get list of arguments to which we can apply a procedure 
  (if (no-operands? exps)
      '()
      ; Note that the order of evaluation of the interpreter language
      ; determines the order of evaluation of the args to cons,
      ; which determines the order of the interpreted language
      ; if we use this list-of-values func (see also Exercise 4.1)
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps)
                            env))))


(define (eval-if exp env)
  ; Handle an if expression
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  ; Evaluate a sequence of expressions
  ; such as you get in a (begin ...) or a (define (myproc) ...)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  ; Such as (set! x 1)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)


(define (eval-definition exp env)
  ; Such as (define x 1)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; Exercise 4.1 See ex4.1.rkt

; 4.1.2 Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

; We still don't have enough at this point to run
; (eval (quote (+ 1 9)) (scheme-report-environment 5))

; Exercise 4.2
;
; 1. If Louis were to reorder the cond in apply so that
;    application? came before assignment?, assignment?
;    and definition? would never be hit as assignments
;    & definitions look like function applications.
;
; 2. If we introduct the special form
;    (call funcname args), can we then test for
;    application earlier?
;    
;    self-evaluating? 1
;    variable? y
;    quoted? (quote ( a b))
;    application?
;      (call display 1) -> (apply display 1)
;      (call display (call + 1 2))
;      (define (my_apply somefunc x y)
;        (call somefunc x y))
;      (call my_apply + 5 6)
;
;    (define
;(define (factorial x)
;  (if (= x 1)
;      1
;      (* x (factorial (- x 1)))))
;
;(factorial 9)
;
;(define (factorial x)
;  (if (call = x 1)
;      1
;      (call * x (call factorial (call - x 1)))))
;
;(call factorial 9)


; Exercise 4.3
; Rewrite eval so that dispatch is does in a "data-
; directed" style. Compare with d-d differentiation
; in 2.73. 
; See ex4.3.rkt















