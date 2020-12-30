#lang sicp

(define (test-equal a b)
  (if (equal? a b)
      #t
      (error "Not equal" a b))) 
             
;(test-equal "44" "55")


"Huffman Trees"
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

"2.67"
"Decode the message using the tree."
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; ADABBCA

"2.68"
"Re-encode the sample message."

; Is element a member of set?
(define (contains? element set)
  (if (null? set)
      #f
      (if (equal? (car set) element)
          #t
          (contains? element (cdr set)))))

(contains? 'A '(A B))

; Encode symbol according to the Huffman tree tree.
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      ; leaf
      (if (equal? symbol (symbol-leaf tree))
          '()
          (error "symbol not in tree" symbol))
      ; not leaf
      (if (contains? symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))))


(test-equal (encode-symbol 'A '(leaf A 8)) '())
(test-equal (encode-symbol 'A sample-tree) '(0))
(test-equal (encode-symbol 'B sample-tree) '(1 0))

; Encode message according to the Huffman tree tree.
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

; Should be '(0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode '(A D A B B C A) sample-tree)


"2.69: generating Huffman trees"

; Insert node into the correct slot in nodelist
(define (insert-ordered node nodelist)
  (if (null? nodelist)
      (list node)
      (if (< (weight node) (weight (car nodelist)))
          (cons node nodelist)
          (cons (car nodelist) (insert-ordered node (cdr nodelist))))))


(test-equal (insert-ordered '(leaf C 2) '())
            '((leaf C 2)))
(test-equal (insert-ordered '(leaf C 2) '((leaf A 1) (leaf B 3)))
            '((leaf A 1) (leaf C 2) (leaf B 3)))


; Turn a list of nodes into a huffman tree.
(define (successive-merge node-set)
  (if (= (length node-set) 1)
      node-set  ; if the length is 1 then we're done
      (successive-merge (insert-ordered (make-code-tree (car node-set)
                                                        (cadr node-set))
                                        (cddr node-set)))))                

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;(define (successive-merge node-set)
;  (if (= (length node-set) 1)
;      node-set  ; if len(node-set) == 1 then we're done
;      (cons (merge (car node-set) (cadr node-set))
;            ( (cddr node-set)))))

; the real deal
;(define (successive-merge node-set)
;  (if (= (length node-set) 1)
;      node-set  ; if the length is 1 then we're done
;      (cons (merge (car node-set) (cadr node-set))
;            ( (cddr node-set)))))

; Sorts smallest to largest.
;(make-leaf-set '((H 1) (G 1) (F 10) (E 5) (D 1) (C 1) (B 3) (A 8)))

; Should be as shown in Fig 2.18.
(generate-huffman-tree '((H 1) (G 1) (F 1) (E 1) (D 1) (C 1) (B 3) (A 8)))

"Exercise 2.70 1950s rock songs"
(define msg-tree 
        (generate-huffman-tree
         '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 1) (YIP 9) (WAH 1))))

; Should the message be a list of symbols instead of a string?
(define song '(GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP
                   YIP YIP YIP YIP
                   SHA BOOM))
;msg-tree
(define msg-tree-ii (car msg-tree))
msg-tree-ii
sample-tree

; Took 83 bits.
(encode song msg-tree-ii)

; Q: How many bits would be needed to encode this song using a fixed-length code?
; A: There are 8 symbols (A, SHA, BOOM, etc) so we would need log2(8)=3 bits/symbol.
;    And the song is 12 + 12 + 12 symbols long.  Therefore, the encoded message
;    would be 36 * 3 = 90 + 18 = 108 bits long.

; TODO Finish ex 2.70 and learn how to use the IDE better.
; See https://www.youtube.com/watch?v=L0HoLCFPzcA&list=PLD0EB7BC8D7CF739A&index=4 
; See F1 for manuals.


"Exercise 2.71: More Huffman trees."
; Q: Suppose we have a H-T for an alphbt of n symbols and that their relative
;    freqs are 1, 2, 4, ..., 2^n-1.  Sketch the tree for n=5 and n=10.  In general,
;    how many bits are required to encode the most freq symbol and least freq symbol.
;
; A:
(define sketch-tree 
        (car (generate-huffman-tree
              '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))))
(encode-symbol 'j sketch-tree)
(encode-symbol 'a sketch-tree)

"Exercise 2.72: "
