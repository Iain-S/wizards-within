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
        (begin
          ;(display tries)
          (if (= tries 2)
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
  (define (integral-test)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo trials integral-test))

(define (Pcircle x y)
  ; Check that the distance to the centre is < the radius
  (< (sqrt (+
            (expt (abs (- x 1.0)) 2.0)
            (expt (abs (- y 1.0)) 2.0)))
     1))

(define (Prect x y)
  (< x 1))

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
  (* 4
     ; Remember to use floats throughout so that (random) returns floats.
     (estimate-integral Pcircle 0.0 2.0 0.0 2.0 trials)) 
  )

(estimate-pi 100000)  ; 3.14170...

"Exercise 3.6 : A RNG that you can reset"
(define (make-new-rand)
  (define x 44)
  (lambda (msg)
    (if (eq? msg 'generate)
        (begin (set! x (modulo (+ (* 44 x)
                                  9) 223456))
               x)
        (lambda (reset-to)
          (set! x reset-to)))))

(define new-rand (make-new-rand))
(new-rand 'generate) ; a)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 44) ; Should be the same as a)
(new-rand 'generate)

"Exercise 3.7 : Joint accounts"
(define (make-joint account orig-pwd new-pwd)
  (if (number? ((account orig-pwd 'd) 0))
      ; then orig-pwd is correct
      (lambda (pwd action)
        (if (eq? pwd new-pwd)
            ; pwd is correct
            (account orig-pwd action)
            "Incorrect pwd"
            ))
      (lambda (b c)
        "Incorrect orig-pwd")))

(define peter-acc
  (make-account-3 100 'mynameispeter))
((peter-acc 'mynameispeter 'd) 20)

(define paul-acc (make-joint peter-acc
                             'mynameispeter
                             'mynameispaul))

(= 110 ((paul-acc 'mynameispaul 'w) 10))
(eq? (paul-acc 'mynamezzispeter 'w) "Incorrect pwd")

"Exercise 3.8 : Order of evaluation"
; define a simple procedure, f, such that (+ (f 0) (f 1)) will return
; 0 if the order of evaluation is left to right and 1 if right to left
(define (make-f x)
  (lambda (a)
    (if (< a x)
        (begin (set! x a)
               x)
        x)))

(define f (make-f 1))

(+ (f 0) (f 1))

 (define f2 
   (let ((called #f)) 
     (lambda (x) 
       (if called 
           0 
           (begin 
             (set! called #t) 
             x)))))

(+ (f2 0) (f2 1))

f2

"3.9 Environments and Factorials"
; From 1.2.1, we have to methods to compute factorials
; Show the environment structures created by evaluating the fact of 6
; for both methods.

; recursive
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
; global env has factorial function that points to global env
; E1 created by call to (factorial 6) with n=6. E1 points to global env.
; E2 created by call to (factorial (- n 1)) with n=5. E2 points to global env.
; ...
; E6 created with n=1.  E6 points to global env.  Function returns 1.

; and iterative
(define (fact-iter product 
                   counter 
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (fact2 n)
  (fact-iter 1 1 n))

; global env has fact2 function that points to global env
; E1 created by call to (fact2 6) with n=5. E1 points to global env.
; E2 created by call to (fact-iter 1 1 6). E2 points to global env.
; E3 created by call to (fact-iter 1 2 6). E3 points to global env.
; ...
; E8 created by call to (fact-iter x 7 6). E8 points to global env.
; function returns x

"Exercise 3.10 Let and make-withdraw"
; One way of making make-withdraw
(define (make-withdraw-1 balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))

; An alternative
(define (make-withdraw-2 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

; Recall that
; (let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩)
; is interpreted as an alternate syntax for
; (lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)

; Illustrate how the two make-withdraw procedures above would handle
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))

; Show that the two versions create the same objects.
; How do the environment structures differ?

; 1
; -------------
; | global                                         <------------
; | make-withdraw-1 ----> params: initial-amount, body..., env |
; | W1
; | W2
; -------------



"Exercise 3.12 Append!ing"
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;z

(cdr x)

(define w (append! x y))

;w

(cdr x)

"Exercise 3.13"
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; Q) Draw a diagram to show the structure of 
(define zz (make-cycle (list 'a 'b 'c))) ; done on paper

; and explain what will happen if we were to run
;(last-pair zz)?
; A) It will never terminate

"Exercise 3.14: mysteries"
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
; Q) What does mystery do?
; A) It reverses a list
(define vv (list 'a 'b 'c 'd))
(define ww (mystery vv))
vv ; Note that the first call to loop does (set-cdr! '(a b c d) '())
ww

"Exercise 3.16: counting pairs"
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
; Q) Why is this implementation wrong? Show inputs of 3 pairs for which
;    it will return 3, 4, 7 or never return.
(count-pairs (list (list 1 2))) ; =3

(define pair-z (list 3)) ; 1 pair
(define pair-a (cons 2 pair-z)) ; 2 pairs -> (1 (2 null))
(define pair-b (cons pair-a pair-z)) ; now 3 pairs (pair-a pair-a)
(count-pairs pair-b) ; =4
;(count-pairs pair-a)

(define pair-3 pair-z)
(define pair-2 (cons pair-z pair-z))
(define pair-1 (cons pair-2 pair-2))
(count-pairs pair-1) ;= 7

(define pair-i (list 3)) ; 3,null
(define pair-j (cons 2 pair-i)) ;2, pair-i
(define pair-k (cons 1 pair-j)) ; 1, pair-j
(count-pairs pair-k)
(set-cdr! pair-i pair-k) ; the end of pair-i now wraps around to pair-k
;(count-pairs pair-k) ; will never return

"Exercise 3.17 : A correct version of count-pairs"
; Hint: maintain a list of visited pairs

(define (count-pairs-2 x)
  (define (already-visited? a b)
    ; is a in b?
    (if (null? b)
        #f
        (if (eq? a (car b))
            #t
            (already-visited? a (cdr b)))))
  (define visited (list 999))
  (define (count-pairs-visited y)
      (if (not (pair? y))
          0
          (let ((been-here (already-visited? y visited)))
            (append! visited (list y)) ; changes visited
            (if been-here
                0
                (+ (count-pairs-visited (car y))
                   (count-pairs-visited (cdr y))
                   1))
            )))
  (count-pairs-visited x))

; These all really have 3 pairs in them
(= 3 (count-pairs-2 pair-k))
(= 3 (count-pairs-2 pair-1))
(= 3 (count-pairs-2 pair-b))

"Exercise 3.18 : Contains-cycle"
(define (cycles? a_list)
  (define (already-visited? a b)
    ; is a in b?
    (if (null? b)
        #f
        (if (eq? a (car b))
            #t
            (already-visited? a (cdr b)))))
  (define visited (list))
  (define (inner-cycles? a_list)
    (if (null? a_list)
        #f
        (if (already-visited? a_list visited)
            #t
            (begin (set! visited (cons a_list visited))
                   (inner-cycles? (cdr a_list))))))
  (inner-cycles? a_list))

(cycles? (list 1))
(define list_cycles (list 1 2 3))
(make-cycle list_cycles)
(cycles? list_cycles)

"Exercise 3.19 : Constant-space contains-cycle"
; Above, we make use of the "visited" list, which adds linear memory.
(define (make-cheeky-cycle a_list)
  ; makes a list with a cycle on the last element
  ; from  1,-> 2,-> 3,()
  ; to    1,-> 2,-> 3,|
  ;                 ^--
  (if (null? (cdr a_list))
      (set-cdr! a_list a_list)
      (make-cheeky-cycle (cdr a_list))))

(define list_cheeky_cycles (list 1 2 3))
(make-cheeky-cycle list_cheeky_cycles)
list_cheeky_cycles
(cycles? list_cheeky_cycles)

(define (cs-cycles? a_list)
  ; constant-space cycles
  ;  a_list: a list that may, or may not, cycle
  ;  returns: #t or #f
  (define (in-list? the-list stop-at item)
    ; go step through the-list
    ; if we encounter item, return #t
    ; if we get to stop-at, return #f
    (cond ((eq? the-list item) #t)
          ((eq? the-list stop-at) #f)
          (else (in-list? (cdr the-list) stop-at item))))
  (define (inner-cycles? c_list)
    (cond ((null? (cdr c_list)) #f)
          ((in-list? a_list c_list (cdr c_list)) #t)
          (else (inner-cycles? (cdr c_list)))))
  (inner-cycles? a_list))

(cs-cycles? list_cheeky_cycles)
(cs-cycles? list_cycles)
(cs-cycles? (list 1 2 3 4 5))

;; Queues

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) 
  (set-car! queue item))

(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cdr (front-ptr queue)))
              queue)))

"Exercise 3.21"
(define (print-queue queue)
  (display (front-ptr queue))
  (display "\n"))


(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)

(print-queue q1)
(delete-queue! q1)
(delete-queue! q1)
(print-queue q1)


"Exercise 3.22"
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((null? front-ptr)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)))
        )
      front-ptr)
    (define (delete)
      ; remove the first item
      (set! front-ptr (cdr front-ptr))
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'front) (car front-ptr))
            ((eq? m 'isempty) (null? front-ptr))
            ((eq? m 'delete) delete)
            (else (error "Undefined operation: CONS" m)))) 
    dispatch))

(define q2 (make-queue2))
;(define q3 (make-queue2))

;; we won't bother with convenience functions for these
((q2 'insert) 77)
((q2 'insert) 79)
(q2 'front)
(q2 'isempty)
((q2 'delete))
((q2 'delete))
(q2 'isempty)
"done"

"Exercise 3.23"
"No, I am not writing a deque"

