#lang sicp

(define (test-equal a b)
  (if (equal? a b)
      #t
      (error "Not equal" a b))) 
             
; (test-equal "44" "55")


"sets as unordered lists"
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))


"2.59"
"Implement union-set for an unordered list set representation"
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (if (element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)
          (cons (car set1) (union-set (cdr set1) set2)))))

(test-equal (union-set (list 1 2 3) (list 2 3 4)) (list 1 2 3 4))


"2.60"
"What if we allow duplicates?"
; I think I have done this problem (or a derivative of it) elsewhere

; element-of-set? doesn't change, it still goes through the whole list. 
(test-equal (element-of-set? 1 (list 1)) #t)


; adjoin-set is just a call to cons, without needing element-of-set
(define (adjoin-set-d x set)
  (cons x set))

(test-equal (adjoin-set-d 3 (list 2)) (list 3 2))

; union-set is just a call to append, without needing element-of-set
(define (union-set-d set1 set2)
  (if (null? set1)
      set2
      (cons (car set1) (union-set-d (cdr set1) set2))))
  
(test-equal '(1 2 3 4) (union-set-d '(1 2) '(3 4)))


; intersection-set doesn't change, it still goes through every element of set1
(test-equal '(1 1 3) (intersection-set '(1 1 2 3) '(3 1)))

;; ToDo Calculate the time complexity.

;; Sets as ordered lists
"2.61"
"Adjoin-set for ordered representations."
(define (adjoin-set-o x set)
  (cond ((= x (car set)) set)  ; already a member
        ((< x (car set)) (cons x set))  ; x < first element
        (else (cons (car set) (adjoin-set-o x (cdr set))))))  ; x > first element


(test-equal '(1 2 3 4) (adjoin-set-o 3 '(1 2 4)))


"2.62"
"Give an Θ(n) implementation of union-set for sets represented as ordered lists."
(define (union-set-o set1 set2)
  (cond ((null? set1)
         set2)
        ((null? set2)
         set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set-o (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set-o (cdr set1) set2)))
        (else  ; (> (car set1) (car set2))
         (cons (car set2) (union-set-o set1 (cdr set2))))))


(test-equal '(0 1 2 4 5) (union-set-o '(0 2 4 5) '(1 2 4)))
(test-equal '(1) (union-set-o '() '(1)))
(test-equal '(1) (union-set-o '(1) '()))

; Sets using trees.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-t? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-t? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set-t? 
          x 
          (right-branch set)))))

"2.63"
"Comparing tree-to-list procedures."
(define tree-2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

; 1. Do the two procedures produce the same result for every tree?
;    If not, how do the results differ?
;    What lists do the two procedures produce for the trees in Figure 2.16?
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(tree->list-1 tree-2-16-1)
(tree->list-1 tree-2-16-2)
(tree->list-1 tree-2-16-3)
(tree->list-2 tree-2-16-1)
(tree->list-2 tree-2-16-2)
(tree->list-2 tree-2-16-3)

"2.64"
"partial-tree"
(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
          