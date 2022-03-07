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
(not (cs-cycles? (list 1 2 3 4 5)))

; ToDo There's a much smarter way to do this in constant space and linear time.


"3.20 - done on paper"
; More environment diagrams


; Queues

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

; make a new pair with the item to be inserted and the empty list '()
; if the queue was empty, set both pointers to this new pair
; else, set the cdr of the last item to this new pair and move the last item pointer
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

; to take an item from the front of the queue, modify the front pointer and let the
; garbage collector do the rest
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
                    (set! rear-ptr new-pair))))
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

;; we won't bother with convenience functions for these
((q2 'insert) 77)
((q2 'insert) 79)
(q2 'front)
(q2 'isempty)
((q2 'delete))
((q2 'delete))
(q2 'isempty)
"done"

"Exercise 3.23 Deques"
; Operations on deques are the constructor make-deque,
; the predicate empty-deque?, selectors front-deque and rear-deque,
; and mutators front-insert-deque!, rear-insert-deque!,
; front-delete-deque!, rear-delete-deque!. Show how to represent
; deques using pairs, and give implementations of the operations.
; All operations should be accomplished in Θ(1) steps.

; Deques are often implemented as doubly-linked lists so
; we'll keep the front and rear ptrs and each item will be
; '(previous-ptr item next-ptr)

;(define deqf (cons 'a '()))
;(define deqe (cons 'b deqf))
;(set-cdr! deqf deqe)
;(cons deqf deqe)

; We're doing a pair-based implementation
(define (make-deque)
  (cons '() '()))

; If the front is empty then the deque is empty
(define (empty-deque? deque)
  (null? (car deque)))

; Helpers
(define (get-front-deque deque)
  (car deque))

(define (get-rear-deque deque)
  (cdr deque))

; More helpers
(define (set-front-deque! deque triple)
  (set-car! deque triple))

(define (set-rear-deque! deque triple)
  (set-cdr! deque triple))

; Add an item to the rear of the deque
(define (rear-insert-deque! deque item)
  ; '(previous-rear . item . () . ())
  (let ((new-triple (list (get-rear-deque deque) item '())))
    (begin
      (if (empty-deque? deque)
          ; then
          (set-front-deque! deque new-triple)  
          ; else
          (set-car! (cddr (get-rear-deque deque)) new-triple))
      (set-rear-ptr! deque new-triple)))
      deque)

; Add an item to the front of the deque
(define (front-insert-deque! deque item)
  ; '(() . item . previous-front . ())
  (let ((new-triple (list '() item (get-front-deque deque))))
    (begin
      (if (empty-deque? deque)
        (set-rear-deque! deque new-triple)  
        (set-car! (get-front-deque deque) new-triple))
      (set-front-ptr! deque new-triple)))
      deque)


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Cannot delete from an empty deque"))
        ((null? (caddr (get-front-deque deque)))
         ; only one element
         (begin (set-front-deque! deque '())
                (set-rear-deque! deque '())))
        (else (begin (set-rear-deque! deque (car (get-rear-deque deque)))
                     (set-car! (cddr (get-rear-deque deque)) '())))))

; ToDo ...
(define (front-delete-deque! deque)
  1)

; Testing

;; May be useful to have a simple way to print the deque contents 
(define (display-deque deque)
  (define (deque-to-list node)
    (if (null? (caddr node))
        (list (cadr node))
        (cons (cadr node) (deque-to-list (caddr node)))))
  (display (deque-to-list (car deque)))
  (display "\n"))


(define my-deque (make-deque))
(empty-deque? my-deque)
(rear-insert-deque! my-deque 4)
(display-deque my-deque)
(rear-insert-deque! my-deque 99)
(front-insert-deque! my-deque 100)
;(get-rear-deque my-deque)
(display-deque my-deque)
(rear-delete-deque! my-deque)
(rear-delete-deque! my-deque)
(rear-delete-deque! my-deque)
my-deque


; 3.3.3. Representing Tables

;; Two-dimensional "local" tables
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

;; This is provided in 3.3.3.
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 4 5 "a value")
(put 'a 'b "another value")
(get 4 5)

;; Exercise 3.24
;; Define a make-table procedure that takes same-key? as an argument.
(define (my-assoc key records is-equal?)
  (cond ((null? records) false)
        ((is-equal? key (caar records)) 
         (car records))
        (else (my-assoc key (cdr records) is-equal?))))

(define (make-my-table same-key?)
  (let ((local-table (list '*table*))
        (asso (lambda (key records) (my-assoc key records same-key?))))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (asso key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (asso key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (asso key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (asso key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define (plus-minus-one? a b)
  (display "plus-minus-one?")
  (display " ")
  (display a)
  (display " ")
  (display b)
  (display "\n")
  (< (abs (- a b)) 1))

(plus-minus-one? 4.9 4)
(plus-minus-one? 4 4.9)
(not (plus-minus-one? 5 4))
(not (plus-minus-one? 4 5))


(define my-op-table (make-my-table plus-minus-one?))
(define my-get (my-op-table 'lookup-proc))
(define my-put (my-op-table 'insert-proc!))

"putting"
(my-put 4 5 "a value")
;(my-put 'a 'b "another value")

"getting"
(my-get 4.1 5.7)

;; Exercise 3.25
;; N-dimensional tables

;; Note, I don't really know why you'd want to do it this way, when you
;; could just treat the list of keys as a key and use a 1-d table...
;; I guess it's more space-efficient for lots of similar, very long, keys

;; We'll assume that *table* has been stripped from the beginning of table.
;; Get the value or sub-table associated with key
;(define (recursive-lookup key table)
;  (cond ((null? table) false)
;        ((equal? (caar table) key) (cdr (car table)))
;        (else (recursive-lookup key (cdr table)))))

;(newline)
;"Recursive Lookup"
;(equal? (recursive-lookup 'a (list (cons 'a 44))) 44)
;(equal? (recursive-lookup 'a (list (cons 'b 55) (cons 'a 'z))) 'z)
;(equal? (recursive-lookup 1 (list (list 1 (cons 2 "end")))) (list (cons 2 "end")))
;(recursive-lookup 1 (list (list 1 (cons 2 "end"))))
;(not (recursive-lookup 'c (list (cons 'b 55) (cons 'a 'z))))

;(define (recursive-lookups keys table)
;  (cond ((null? table) false)
;        ((null? keys) table)
;        ((equal? (caar table) (car keys)) (recursive-lookups (cdr keys) (cdr (car table))))))


;; A recursive lookup returning the first match or false
(define (lookup-2 keys table)
  (cond ((null? table) false)
        ((null? keys) table)
        ((equal? (caadr table) (car keys)) (lookup-2 (cdr keys) (car (cdr table))))
        (else false)))

(newline)
"Test lookup-2"
(equal? (lookup-2 '() (cons 'math 43)) (cons 'math 43))
(equal? (lookup-2 (list 2) (list 1 (cons 2 "oo"))) (cons 2 "oo"))

;; Like lookup-2 but to be used at the root level of the table
;; from the second element onwards
(define (lookup-1 keys table)
  (cond ((null? table) false) ; table is completely empty
        ((null? keys) (error "Must have one or more key"))
        ((equal? (caar table) (car keys)) (lookup-2 (cdr keys) (car table)))
        (else (lookup-1 keys (cdr table))))) ; didn't match 

(newline)
"Test lookup-1"
(equal? false (lookup-1 (list "some key") '()))
(equal? (cons 2 "yes") (lookup-1 (list 1 2) (list (list 1 (cons 2 "yes")))))
(equal? (cons 3 "yes") (lookup-1 (list 3) (list (cons 'math 1) (cons 3 "yes"))))
(equal? (cons 6 "yes") (lookup-1 (list 4 5 6) (list (cons 1 "no")
                                                    (cons 2 "no")
                                                    (list 4 (list 5 (cons 6 "yes"))))))

(define (insert-2! keys value table)
  (let ((found (lookup-2 (list (car keys)) table)))
    (if found
        (insert-2! (cdr keys) value found)
        (let ((new-subtable (create-subtable keys value)))
          (set-cdr! table  ; insert it behind '*table*
                    (cons new-subtable
                          (cdr table)))))))

;; for level 
(define (create-subtable keys value)
  (if (null? (cdr keys))
      (cons (car keys) value)
      (list (car keys) (create-subtable (cdr keys) value))))

(newline)
"Test create-subtable"
(equal? (create-subtable (list 1) 'a) (cons 1 'a))
(equal? (create-subtable (list 1 2) 'a) (list 1 (cons 2 'a)))

; Insert value into table at the position given by keys
; (a position that must not exist already)
(define (insert-1! keys value table)
  (let ((found (lookup-1 (list (car keys)) (cdr table))))
    (if found
        (insert-2! (cdr keys) value found) ; the first key is already there
        (let ((new-subtable (create-subtable keys value)))
          (set-cdr! table  ; insert it behind '*table*
                    (cons new-subtable
                          (cdr table)))))))

(newline)
"Test insert-1!"
(define test-table-1 (list '*table*))
(insert-1! (list 1) "yes" test-table-1)
(equal? test-table-1 (list '*table* (cons 1 "yes")))

;(equal? (lookup-2 (list 1) (list (cons 1 "end")))) 

(define (make-nd-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((answer (lookup-1 keys (cdr local-table))))
        (if answer
            (cdr answer)
            false)))
    (define (insert! keys value)
      (let ((answer (lookup-1 keys (cdr local-table))))
        (if answer
            (begin (set-cdr! answer value) 'overwritten)
            (begin (insert-1! keys value local-table) 'ok))))  
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(newline)
"Test make-nd-table"

(define my-nd-table (make-nd-table))
(define put-nd! (my-nd-table 'insert-proc!))
(define get-nd (my-nd-table 'lookup-proc))

; Insert into empty table
(put-nd! (list 1) "yes")
(equal? (get-nd (list 1)) "yes")
(get-nd (list 1))

(newline)
; Overwrite value
(put-nd! (list 1) "nope")
(equal? (get-nd (list 1)) "nope")

"multiline
coment?"
; Insert into populated table
(equal? 'ok (put-nd! (list 'a 'b 'c) "yellow"))
(equal? (get-nd (list 'a 'b 'c)) "yellow")

(display "multiline\ncomment\n")
(put-nd! (list 'a "b" 3) 999)
(= (get-nd (list 'a "b" 3)) 999)

(put-nd! (list 'a "b") -90)
(= (get-nd (list 'a "b")) -90)

;; summary
;; As per the "Two-dimensional tables" diagram in chapter 3 but with variable number of keys
;; without resorting to using lists as keys. Trying to store '(1 2): "a" and '(1 3): "b" would work
;; but trying to store '(1 2): "a" and '(1): "b" will overwrite the first (which is sensible).
;; It isn't very production ready though, checks are minimal.

;; Exercise 3.26...
;; Binary-tree tables
;                      table
;                        |
;                        v
;                *table* ,  |   
;                           |
;                           v
;                       key, value,  /, \
;                                   /    \
;                                  /      \___________> higher key, value, l, r
;                                 v
;                           lower key, value, l, r
;
;
;; Each node in our table is
;; (list key value left right)
;; where left and right are (possibly empty) trees
;; and value is either a single value or a tree

; find node traverses a binary tree to find node with a certain key,
; creating it if it doesn't already exist

; helper methods

;; get the key
(define (bt-get-key tree)
  (car tree))

;; get the value
(define (bt-get-value tree)
  (cadr tree))

;; get the lh branch
(define (bt-get-left tree)
  (caddr tree))

;; get the rh branch
(define (bt-get-right tree)
  (cadddr tree))

;; set/reset the value
(define (bt-set-value! tree value)
  (set-car! (cdr tree) value))

;; ToDo Make a helper to return an empty node (list somekey 'empty-value '() '())

(define (make-empty-node key)
  (list key 'empty-value '() '()))

; find a matching node or create one
(define (bt-find-or-create! key tree)
  (cond
    ; if key = tree.key, return the tree
    ((= key (bt-get-key tree)) tree)
    ; else if key < tree.key and lh is null, create and return an new lh
    ((and (null? (bt-get-left tree)) (< key (bt-get-key tree)))
     (let ((new-left (make-empty-node key)))
       (set-car! (cddr tree) new-left)
       new-left))
    ; else if key < tree.key and lh is not null, call ourselves again
    ((< key (bt-get-key tree))
     (bt-find-or-create! key (bt-get-left tree)))
    ; else, if key > tree.key and rh is null, create and return new rh
    ((and (null? (bt-get-right tree)) (> key (bt-get-key tree)))
     (let ((new-right (make-empty-node key)))
       (set-car! (cdddr tree) new-right)
       new-right))
    ; else if key > tree.key and rh is not null, call ourselves again
    ((> key (bt-get-key tree))
     (bt-find-or-create! key (bt-get-right tree)))
    (else (error "bt-find-or-create! this should not happen"))))

"Test bt-find-or-create!"
(equal? (list 1 "xx" '() '()) (bt-find-or-create! 1 (list 1 "xx" '() '())))
(equal? (list 2 'empty-value '() '()) (bt-find-or-create! 2 (list 1 "xx" '() '())))
(define test-tree-1 (list 1 "xx" '() '()))
(define expected-tree-1 (list 1 "xx" '() (list 2 'empty-value (list 1.1 'empty-value '() '()) '())))
(bt-find-or-create! 2 test-tree-1)
(bt-find-or-create! 1.1 test-tree-1)
(equal? test-tree-1 expected-tree-1)


; find a matching node with many keys (or create one)
(define (bt-find-or-create-rec! keys tree)
  (let ((found-or-created (bt-find-or-create! (car keys) tree)))
    (if (null? (cdr keys))
        ; only one item in keys so return our node
        found-or-created
        ; else
        (begin
          (if (not (pair? (bt-get-value found-or-created)))
              ; this "value" needs to be overwritten with an empty node
              (bt-set-value! found-or-created (make-empty-node (cadr keys))))
          (bt-find-or-create-rec! (cdr keys) (bt-get-value found-or-created))))))

"Test bt-find-or-create-rec!"
(let ((answer (bt-find-or-create-rec! (list 1) (list 2 "a val" '() '()))))
  (if (equal? answer (list 1 'empty-value '() '()))
      #t
      (begin (display answer)
             (newline)
             #f)))

"Test recursive find"
(let ((second-level (list 9 'winning '() '())))
  (let ((first-level (list 2 second-level '() '())))
    (let ((answer (bt-find-or-create-rec! (list 2 9) first-level)))
      (if (equal? answer second-level)
          #t
          (begin (display answer)
                 (newline)
                 #f)))))

"Test recursive create"
(let ((expected (list 9 'empty-value '() '())))
  (let ((test-tree (list 2 "any non-pair" '() '())))
    (let ((answer (bt-find-or-create-rec! (list 2 9) test-tree)))
      (if (equal? answer expected)
          #t
          (begin (display answer)
                 (newline)
                 (display expected)
                 (newline)
                 #f)))))


;(eq? (bt-find-or-create-rec! (list 1) (list 2 "a val" '() '()))
;     (list 1 'empty-value '() '()))

; Our nodes are in the form
; (list key value left right)
; where value, left and right can all be nodes themselves.
; You SHOULD NOT try to store a list in value (as we use pair? to determine
; whether value is a subtree).
(define (bt-make-table)
  (let ((local-btree '()))
    (define (is-empty?)
      (null? local-btree))
    (define (lookup keys)
      (let ((found-value (bt-get-value (bt-find-or-create-rec! keys local-btree))))
        (if (equal? found-value 'empty-value)
            ; the key wasn't in the tree and we just created it (oops)!
            'error_key_not_already_in_table
            ; else, we found our entry
            found-value)))      
    (define (get-raw-tree)
      local-btree)
    (define (insert! keys value)
      (if (is-empty?)
          ; if empty, add a root node
          (set! local-btree (make-empty-node (car keys))))
      ; now use our procedure to find/create an appropriate node
      (let ((found-or-created (bt-find-or-create-rec! keys local-btree)))
        (begin
          (if (eq? (bt-get-value found-or-created) 'empty-value)
              (display "creating\n")
              (display "overwriting\n"))
          (bt-set-value! found-or-created value)
          'inserted)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'is-empty?) is-empty?)
            ((eq? m 'get-raw) get-raw-tree)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))


"Test binary tree table"
(define my-btable-1 (bt-make-table))
((my-btable-1 'insert-proc!) (list 1 2) "myvalB")
(equal? ((my-btable-1 'get-raw)) (list 1 (list 2 "myvalB" '() '()) '() '()))

(let ((expected (list 1 (list 2 "myvalB" '() '()) '() '())))
  (let ((test-tree (bt-make-table)))
    ((test-tree 'insert-proc!) (list 1 2) "myvalB")
    (let ((answer ((test-tree 'get-raw))))
      (if (equal? answer expected)
          #t
          (begin (display answer)
                 (newline)
                 (display expected)
                 (newline)
                 #f)))))


"Test is-empty?"
(define my-btable (bt-make-table))
((my-btable 'is-empty?))

(equal? ((my-btable 'insert-proc!) (list 1) "a") 'inserted)
(equal? ((my-btable 'insert-proc!) (list 2) "a") 'inserted)
((my-btable 'insert-proc!) (list 1) "a")
((my-btable 'insert-proc!) (list 2) "b")
((my-btable 'insert-proc!) (list 3) "c")
((my-btable 'insert-proc!) (list 3 4) "cd")
(equal? ((my-btable 'lookup-proc) (list 3 4 )) "cd")

(equal? ((my-btable 'lookup-proc) (list 3 4 5)) 'error_key_not_already_in_table)

((my-btable 'insert-proc!) (list 3 4 9 9 9 10001) "abra kadabra")
(equal? ((my-btable 'lookup-proc) (list 3 4 9 9 9 10001)) "abra kadabra")
(equal? ((my-btable 'lookup-proc) (list 2)) "b")

;; Exercise ...


