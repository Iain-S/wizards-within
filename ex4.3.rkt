#lang sicp
; Exercise 4.3
; Rewrite eval so that dispatch is does in a "data-
; directed" style. Compare with d-d differentiation
; in 2.73.

; For d-d style, we need a table
; to use the table, we need lookup and insert funcs
(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; Now, we can make our table
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; Then we can make an actual table and define
; lookup and insert in terms of that
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 9 0.5)
(= (get 'a 9) 0.5)

; dummy funcs for testing
(define (variable? exp)
  (begin
    (display "in variable?")
    #f))
(define (self-evaluating? exp)
  (begin
    (display "in self-evaluating?")
    #f))
(define (text-of-quotation exp)
  (begin
    (display "in text-of-quotation")
    #f))
(define (eval-assignment exp)
  (begin
    (display "in eval-assignment")
    #f))
(define (eval-definition exp)
  (begin
    (display "in eval-definition")
    #f))
(define (eval-if exp)
  (begin
    (display "in eval-if")
    #f))
(define (make-procedure exp)
  (begin
    (display "in make-procedure")
    #f))
(define (lambda-parameters exp)
  (begin
    (display "in lambda-parameters")
    #f))
(define (lambda-body exp)
  (begin
    (display "in lambda-body")
    #f))
(define (eval-sequence exp)
  (begin
    (display "in eval-sequence")
    #f))
(define (begin-actions exp)
  (begin
    (display "in begin-actions")
    #f))
(define (cond->if exp)
  (begin
    (display "in cond->if")
    #f))
(define (lookup-variable-value exp)
  (begin
    (display "in lookup-variable-value")
    #f))


(define (eval exp env)
  (define operation-table (make-table))
  (define get (operation-table 'lookup-proc))
  (define put (operation-table 'insert-proc!))
  (define (partial f a b)
    (define (new a)
      (f a b))
    new)

  ; It's a 2D table but I think we only
  ; need 1D of it so use 1 as the first key
  (put 1 'quote text-of-quotation)

  ; Make all stored procedures funcs of one arg
  ; (exp) only.
  (put 1 'define (partial eval-assignment env))
  (put 1 'set! (partial eval-definition env))
  (put 1 'if (partial eval-if env))
  (define (my-lambda exp env)
    (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
  (put 1 'lambda (partial my-lambda env))
  (define (my-begin exp env)
    (eval-sequence
     (begin-actions exp)
     env))
  (put 1 'begin (partial my-begin env))
  (define (my-cond exp env)
    (eval (cond->if exp) env))
  (put 1 'cond (partial my-cond env))
  
  ; Evaluate an expression in an environment
  (cond ((self-evaluating? exp)
         ; Such as numbers
         exp)
        ((variable? exp)
         ; Such as x
         (lookup-variable-value exp env))
        ((get 1 (car exp))
         ((get 1 (car exp)) exp))
        ((application? exp)
         ; Such as (myproc 88 99)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression type." exp))))