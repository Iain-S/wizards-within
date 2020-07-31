#lang sicp
;; if item is in x, return item and all later elements
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'i '(o p i t))

;; Ex 2.54
;; compare two lists
(define (equals? first_list second_list)
  (cond ((null? first_list) (null? second_list))
        ((null? second_list) (null? first_list))
        ((eq? (car first_list) (car second_list))
         (equals? (cdr first_list) (cdr second_list)))
        (else #f)))

(equals? '(9 u) '())
(equals? '(9 u) '(9 u))

;; note that the first item of this string? list? is itself a '
;; I believe it is because it expands to (quote 'abcde)
(car ''abcde)

;; define some functions so that we can calculate derivatives
(define (multiplicand p) (caddr p))

(define (multiplier p) (cadr p))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (augend s) (caddr s))

(define (addend s) (cadr s))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; (define (exponentiation? exp)
  ;; (and (pair? x) (eq? (car x) '+)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)

;; Ex 2.56
;; handle exponentiation
(define (make-exponentiation base exponent)
  (list '** base exponent))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (exponentiation? exponentiation)
  (and (pair? exponentiation) (eq? (car exponentiation) '**)))

(make-exponentiation 'y 'x)
(base (make-exponentiation 'y 'x))
(exponent (make-exponentiation 'y 'x))
(exponentiation? (make-exponentiation 'y 'x))
;; (= (make-exponentiation 'y 0) 1)
(deriv (make-exponentiation 'y 'x) 'x)
(equals? '(* x (** y (+ x -1))) (deriv (make-exponentiation 'y 'x) 'x))
(deriv '(* (* x y) (+ x 3)) 'x)

(define (deriv2 exp var)
  (display exp)(newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv2 (addend2 exp) var)
                   (deriv2 (augend2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier2 exp)
                        (deriv2 (multiplicand2 exp) var))
          (make-product (deriv2 (multiplier2 exp) var)
                        (multiplicand2 exp))))
        ;; ((exponentiation? exp)
         ;; (make-product (exponent exp)
                       ;; (make-exponentiation (base exp)
                                            ;; (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type -- DERIV2" exp))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (multiplicand2 p)
  (cond ((number? (cdr (cdr p))) p)
        (else (cons '* (cdr (cdr p))))))

(define (multiplier2 p) (cadr p))

;; note that A & S seem to have the augend/addend
;; definition backwards
(define (augend2 p)
   (cond ((number? (car (cdr (cdr p)))) (car (cdr (cdr p))))
         (else (cons '+ (cdr (cdr p))))))

;; define (augend2 s) (caddr s))

(define (addend2 s) (cadr s))

(equals? '(* 3 2) (multiplicand2 '(* 4 3 2)))
(eq? 2 (augend2 '(+ 4 2)))
(equals? '(+ 1 2 3 4) (augend2 '(+ 1 1 2 3 4)))

;; (equals? '(+ 3 2) (augend2 '(+ 4 3 2)))
;; (deriv2 '(* x y (+ x 3)) 'x)
;; (deriv2 '(* x x x) 'x)

;; sets
