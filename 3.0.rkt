#lang sicp

(define balance 101)

(define (withdraw amount)
  (if (> amount balance)
      "Insufficient funds!"
      (begin (set! balance (- balance amount))  ; set <name> <new-value>
             balance)))

(withdraw 80)
(withdraw 500)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 10)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define another-withdraw (make-withdraw 2000))
(another-withdraw 1004)

; test of define
; so whether bnew-withdraw is a function or a function-that-returns-a-function
; depends only on whether we do (define bnew-withdraw) or (define (bnew-withdraw))
(define bbalance 500)
(define (bnew-withdraw) 
    (lambda (amount)
      (if (>= bbalance amount)
          (begin (set! bbalance 
                       (- bbalance amount))
                 bbalance)
          "Insufficient funds")))
((bnew-withdraw) 50)


; deposits as well as withdrawals
(define (make-account balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else "unknown")))
  dispatch)

(define acc (make-account 5000))
((acc 'd) 5000)

"Exercise 3.1: Accumulators"
(define (make-accumulator amount)
  (define (accumulator addend)
    (set! amount (+ amount addend))
    amount)
  accumulator)
(define A (make-accumulator 5))

(A 10)  ; should be 15
(A 10)  ; should be 25


"Exercise 3.2: Function call counting"
; Write a procedure make-monitored that takes as input a procedure,
; f, that itself takes one input.
(define (make-monitored f)
  (let ((calls 0))
    (define (inner arg)
      (cond ((number? arg)
             (begin (set! calls (+ calls 1))
                    (f arg)))
            ((eq? arg 'how-many-calls?)
             calls)
            ((eq? arg 'reset-count)
             (set! calls 0))
            (else (error "no!"))))
  inner))

; Test our function
(define (afunc arg)
  (+ arg 999))

(define monitored-afunc (make-monitored afunc))
(monitored-afunc 6)
(monitored-afunc 2)
(monitored-afunc 4)
(= (monitored-afunc 'how-many-calls?) 3)
(define monitored-afunc2 (make-monitored afunc))
(monitored-afunc2 10)
(= (monitored-afunc2 'how-many-calls?) 1)
(monitored-afunc2 'reset-count)
(= (monitored-afunc2 'how-many-calls?) 0)

"Exercise 3.3 : password protection"
; modify make-account to accept a password
(define (make-account-2 balance password)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else "unknown")))
  (define (wrong-password arg) ; a little ugly to have this unused arg
    "Incorrect password")
  (define (check-password-and-dispatch pwd message)
    (if (eq? pwd password)
        (dispatch message)
        wrong-password))
  check-password-and-dispatch)

(define acc-2
  (make-account-2 100 'qwerty))

; try to withdraw 40 with correct password
(eq? ((acc-2 'qwerty 'w) 40) 60)
; try to withdraw 50 with incorrect password
(eq? ((acc-2 'some-other-password 'd) 50) "Incorrect password")

"Exercise 3.4: call-the-cops if the wrong password is used too often"
; The exercise says 7 but we'll do 3
(define (call-the-cops a)
        "cops have been called")

(define (make-account-3 balance password)
  (define tries 0)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else "unknown")))
  (define (wrong-password arg) ; a little ugly to have this unused arg
    "Incorrect password")
  (define (check-password-and-dispatch pwd message)
      (if (eq? pwd password)
        (begin
          (set! tries 0)
          (dispatch message))
        (begin (display tries) (if (= tries 2)
            call-the-cops
            (begin
              (set! tries (+ tries 1))
              wrong-password)))))
  check-password-and-dispatch)

(define acc-3
  (make-account-3 100 'qwerty))
(eq? ((acc-3 'some-other-password 'd) 1) "Incorrect password")
(eq? ((acc-3 'some-other-password 'd) 1) "Incorrect password")
(eq? ((acc-3 'qwerty 'd) 1) 51)
(eq? ((acc-3 'some-other-password 'd) 1) "Incorrect password")
(eq? ((acc-3 'some-other-password 'd) 1) "Incorrect password")
(eq? ((acc-3 'some-other-password 'd) 1) "cops have been called")

; 3.1.2 Benefits of Assignment

"Exercise 3.5 : Monte Carlo Integration"

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  1)

(define (Pcircle x y)
  ; Check that the distance to the centre is < the radius
  (< (sqrt (+
            (expt (abs (- x 1)) 2)
            (expt (abs (- y 1)) 2)))
     1))

(Pcircle 1 1) ; should be #t
(Pcircle 1.5 1.5) ; should be #t
(not (Pcircle 2 2)) ; should be #f
(not (Pcircle 0 0)) ; should be #f

(define (estimate-pi trials)
  ; area = pi * r^2 so
  ; pi = area / r^2 and
  ; we are doing a unit circle so R^2 = 1 so
  ; pi = area

  ; note: a unit circle fits in a 2 x 2 box, not a 1 x 1 box
  (estimate-integral Pcircle 0 2 0 2 trials) 
  )

(estimate-pi 1000)