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
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (text-of-quotation exp)
  (cadr exp))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (assignment-variable exp) 
  (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  ; Such as (set! x 1)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body
(define (eval-definition exp env)
  ; Such as (define x 1)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)
(define (true? x)
  (not (eq? x false)))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (false? x)
  (eq? x false))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
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
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))
(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))
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
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (application? exp) (pair? exp))
(define (operator exp)
  (car exp))
(define (list-of-values exps env)
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
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
(define (operands exp)
  (cdr exp))

(define (eval exp env)
  ; This function is the solution to Exercise 4.3
  
  (define (partial f b)
    ; Return a new function, which is f with one arg (b) applied
    (define (new a)
      (f a b))
    new)
  
  ; It's a 2D table but I think we only
  ; need 1D of it so use 'op as the first key
  (put 'op 'quote text-of-quotation)
  
  ; Make all stored procedures funcs of one arg
  ; (exp) only.
  (put 'op 'define (partial eval-assignment env))
  (put 'op 'set! (partial eval-definition env))
  (put 'op 'if (partial eval-if env))
  (define (my-lambda exp env)
    (make-procedure
     (lambda-parameters exp)
     (lambda-body exp)
     env))
  (put 'op 'lambda (partial my-lambda env))
  (define (my-begin exp env)
    (eval-sequence
     (begin-actions exp)
     env))
  (put 'op 'begin (partial my-begin env))
  (define (my-cond exp env)
    (eval (cond->if exp) env))
  (put 'op 'cond (partial my-cond env))
  
  ; Evaluate an expression in an environment
  (cond ((self-evaluating? exp)
         ; Such as numbers
         exp)
        ((variable? exp)
         ; Such as x
         (lookup-variable-value exp env))
        ((get 'op (car exp))
         ; This is not very clever, we simply look up
         ; the first item in the expression in our table
         ((get 'op (car exp)) exp))
        ((application? exp)
         ; Such as (myproc 88 99)
         (mapply (eval (operator exp) env)
                 (list-of-values
                  (operands exp)
                  env)))
        (else
         (error "Unknown expression type." exp))))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters 
              procedure)
             arguments
             (procedure-environment 
              procedure))))
        (else
         (error "Unknown procedure 
                 type: APPLY" 
                procedure))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;⟨more primitives⟩
        (list '+ +)
        ; These are the solution to Exercise 4.4
        (list 'and (lambda (x y) (if x y #f)))
        (list 'or (lambda (x y) (if x x y)))
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))
(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))
(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")


(define (driver-loop)
  ;; You can use this to start a REPL
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
           (eval input 
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(eval '"a" the-global-environment)
(eval '(+ 11 99) the-global-environment)

; not sure why this works for 'true and 'false but not for '#t and '#f
(eq? (eval '(and false true) the-global-environment) false)
(eq? (eval '(or 9 false) the-global-environment) 9)

"Exercise 4.5"
; Add the following special cond form to our evaluator
(cond (12 => +)
        (else "what?"))