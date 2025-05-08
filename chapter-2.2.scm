(load "lib.scm")

;; Section 2.2.1
(define nil (list))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (-1+ n))))

(define (length items)
  (if (null? items)
      0
      (1+ (length (cdr items)))))

(define (length items)
  (define (iter n l)
    (if (null? l)
	n
	(iter (1+ n) (cdr l))))
  (iter 0 items))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (append (cdr l1) l2))))

;; ---- Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;; ---- Exercise 2.18
(define (reverse l)
  (define (iter reversed remaining)
    (if (null? remaining)
	reversed
	(iter (cons (car remaining) reversed)
	      (cdr remaining))))
  (iter (list) l))

;; ---- Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount all-coins)
  (define (no-more? coins)
    (null? coins))
  (define (first-denomination coins)
    (car coins))
  (define (except-first-denomination coins)
    (cdr coins))
  
  (define (cc remaining-amount coins)
    (cond ((= remaining-amount 0) 1)
	  ((< remaining-amount 0) 0)
	  ((no-more? coins) 0)
	  (else (+ (cc remaining-amount (except-first-denomination coins))
		   (cc (- remaining-amount (first-denomination coins))
		       coins)))))
  (cc amount all-coins))
;; The order does not matter, since it calculates all possible combination simply in a different order.

;; ---- Exercise 2.20
(define (same-parity n . rest)
  (define (same-parity? n m)
    (= (remainder n 2) (remainder m 2)))
  (define (iter result remaining)
    (cond ((null? remaining) result)
	  ((same-parity? n (car remaining))
	   (iter (cons (car remaining) result)
		 (cdr remaining)))
	  (else (iter result (cdr remaining)))))
  (cons n (reverse (iter nil rest))))
 

;; ----

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(define (my-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; ---- Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
	    (square-list (cdr items)))))

(define(square-list items)
  (map square items))

;; ---- Exercise 2.22
;; This procedure generates a reversed result, since at each step it takes the answer calculated until now
;; and puts it in the cdr of the next step answer, whereas the next step answer should be appended to it.
(define (square-list-wrong items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

;; This procedure does not generate a list as expected, since at every step it takes the answer calculated until now, which is a list,
;; and puts it in the car position, and then cons-es it with an element, which is not a list.
;; The result is that answer at step n is the cons between the previous answer and a number, which is not a list.
(define (square-list-wrong items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

;; ---- Exercise 2.23
(define (for-each proc items)
  (define (iter things)
    (cond ((not (null? things))
	   (proc (car things))
	   (iter (cdr things)))))
  (iter items))


;; --------
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;; ---- Exercise 2.24
(list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))

;; [*|*]->[*|/]
;;  |      |
;;  V      V
;; [1]    [*|*]->[*|/]
;;         |      |
;;         V      V
;;        [2]    [*|*]->[*|/]
;;                |      |
;;                V      V
;;               [3]    [4]

;; (1 (2 (3 4))) *
;;              / \
;;           1 *   * (2 (3 4))
;;                / \
;;             2 *   * (3 4)
;;                  / \
;;               3 *   * 4

;; ---- Exercise 2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;; ---- Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

;; ---- Exercise 2.27
(define (deep-reverse l)
  (define (iter reversed remaining)
    (cond ((null? remaining) reversed)
	  ((pair? (car remaining))
	   (iter (cons (deep-reverse (car remaining)) reversed)
		 (cdr remaining)))
	  (else
	   (iter (cons (car remaining) reversed)
		 (cdr remaining)))))
  (iter (list) l))

;; ---- Exercise 2.28
(define (fringe x)
  (define (iter result remaining)
    (cond ((null? remaining) result)
	  ((pair? (car remaining))
	   (iter (append result (fringe (car remaining)))
		 (cdr remaining)))
	  (else
	   (iter (append result (list (car remaining)))
		 (cdr remaining)))))
  (iter nil x))

;; ---- Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
	(total-weight structure)
	structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (= (* (branch-length left)
	  (branch-weight left))
       (* (branch-length right)
	  (branch-weight right)))))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; I need to change the selectors for right and structure. Everything else remains unchanged.
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))


;; ---- Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; ---- Exercise 2.31
(define (tree-map proc)
  (define (rec tree)
    (cond ((null? tree) nil)
	  ((not (pair? tree)) (proc tree))
	  (else (cons (rec (car tree))
		      (rec (cdr tree))))))
  rec)
(define square-tree (tree-map square))

;; ---- Exercise 2.32
;; Suppose that we have the list of n elements (0 1 2 ... n-1).
;; At the final step, n, we have the empty list, and its only subset is the empty list itself.
;; At step i, we can assume that we have the subsets of the list (i+1 ... n-1).
;; All of these subsets are also subsets of the list (i ... n-1).
;; In addition, we need to calculate the subsets containing i: they are simply all the subsets not containing i, with i added.
;; Thus, by induction we have found a method to calculate all the subsets of (0 1 2 ... n-1), and the procedure is the recursive implementation of this induction.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))


;; Section 2.2.3
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (define (iter a b step)
    (if (= step n)
	b
	(iter (+ a b)
	      a
	      (+ 1 step))))
  (iter 1 0 0))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
	nil
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+1 k)))
	      (next (+1 k))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

(define (salary-of-highest-paid-programmer record)
  (accumulate max
	      0
	      (map salary
		   (filter programmer? records))))

;; ---- Exercise 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

;; ---- Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

;; ---- Exercise 2.35
(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x)
		     (if (pair? x)
			 (count-leaves x)
			 1))
		   t)))

;; Alternative, found online: flatten and then add 1 for each element.
(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x) 1)
		   (enumerate-tree t))))

;; ---- Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;; ---- Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))


;; ---- Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)
;; To produce the same value for every sequence, op needs to be commutative and associative. For example:
(fold-right + 0 (list 1 2 3)) ; 6
(fold-left + 0 (list 1 2 3)) ; 6
    
;; ---- Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse '(1 2 3 4))


;; --------
(define (pairs n)
  (accumulate append
	      nil
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (-1+ i))))
		   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (-1+ i))))
	   (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (-1+ i))))
		  (enumerate-interval 1 n)))))
			 
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

;; ---- Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (-1+ i))))
	   (enumerate-interval 1 n)))
		       
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

;; ---- Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k) (list i j k))
			     (enumerate-interval 1 (-1+ j))))
		      (enumerate-interval 1 (-1+ i))))
	   (enumerate-interval 1 n)))

(define (triples-summing-to n s)
  (filter (lambda (t) (= (+ (car t) (cadr t) (caddr t)) s))
	  (unique-triples n)))

;; ---- Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (-1+ k))))))
  (queen-cols board-size))

(define empty-board (list))

(define (adjoin-position row col rest)
  (append rest (list (list col row))))

(define (safe? col positions)
  (let ((row (cadar (filter
		     (lambda (x)
		       (= (car x) col))
		     positions))))
    (= (length (filter
		(lambda (x) (= (cadr x) row))
		positions))
       1)))


;; ---- Exercise 2.43
(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (-1+ k))))
	    (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; This version is slower because it does the recursive call in the inner lambda, calculating the queens of the previous steps board-size times at each step.
;; To compare the number of steps required, let's analyze the two cases.
;; In both algorithms, the base step is (queen-cols 0), which takes a constant time c.

;; In the first version, step k:
;; - computes the queens of step k-1
;; - adjoins n positions to the board
;; - filters the safe ones
;; The time is T_a(n, k) = T_a(n, k-1) + nT(adj + safe?).
;; Recursively, we can see that T_a(n, k) = T(0) + knT(adj + safe?).

;; In the second version, step k:
;; - computes the queens of step k-1 n times
;; - adjoins n positions to the board
;; - filters the safe ones
;; The time is T_b(n, k) = nT_b(n, k-1) + nT(adj + safe?).
;; Recursively, we can see that T_b(n, k) = n^kT(0) + n^kT(adj + safe?), since T_b(n, k-1) = nT-b(n, k-2) + nT(adj + safe?).

;; Thus, comparing the two expressions, we obtain
;; T_b(n, k) = n^k [T_a(n, k) - (n, kn - 1)T(adj + safe?)]
;; Under the (probably false) assumption that T(adj + safe?) is constant (in particular, T(safe?) could not be O(1)), T_a(n, k) = O(kn), therefore T_b(n, k) = O(n^k kn) = O(n^k T_a(n, k))

