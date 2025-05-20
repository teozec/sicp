(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)
(car '(a b c))
(cdr '(a b c))
(list 'car (list 'quote '(a b c)))

(define (memq item x)
  (cond ((null? x) false)
	((eq? (car x) item) x)
	(else (memq item (cdr x)))))

;; ---- Exercise 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socke)) ; (red shoes blue socks)

;; ---- Exercise 2.54
(define (atom? x)
  (not (pair? x)))

(define (equal? a b)
  (or (and (atom? a) (atom? b) (eq? a b))
      (and (pair? a) (pair? b)
	   (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))))
 
;; ---- Exercise 2.55
(car ''abracadabra)
;; The quote character ' is symbolic sugar for the special form quote. The above expression is equivalent to
(car '(quote abracadabra))
;; and the first element of the list (quote abracadabra) is the symbol quote.


;; Section 2.3.2
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
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))
	
;; ---- Exercise 2.56
;; This definition uses the arbitrary-arity product defined in the next exercise.
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
					    (-1+ (exponent exp)))
		       (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))

;; An expression is an exponentiation if it is a list, the first symbol is ** and the third symbol (the exponent) is a number (otherwise, we need function composition rule).
(define (exponentiation? exp)
  (and (pair? exp)
       (= (length exp) 3)
       (eq? (car exp) '**)
       (number? (caddr exp))))

(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (if (= exponent 1)
      base
      (list '** base exponent)))

;; ---- Exercise 2.57
(define (make-sum . a)
  (cons '+ a))
(define (addend s)
  (if (equal? s '(+))
      0
      (cadr s)))
(define (augend s)
  (if (equal? s '(+))
      0
      (apply make-sum (cddr s))))

(define (make-product . a)
  (cons '* a))
(define (multiplier p)
  (if (equal? p '(*))
      1
      (cadr p)))
(define (multiplicand p)
  (if (equal? p '(*))
      1
      (apply make-product (cddr p))))

;; To make things more complicated, let's simplify the expressions.
(define (make-sum . a)
  (let* ((numbers (filter number? a))
	 (expressions (remove number? a))
	 (numbers-sum (apply + numbers)))
    (cond ((null? expressions) numbers-sum)
	  ((and (= numbers-sum 0)
		(null? (cdr expressions)))
	   (car expressions))
	  ((= numbers-sum 0) (cons '+ expressions))
	  (else (cons '+ (cons numbers-sum expressions))))))
	
(define (make-product . a)
  (let* ((numbers (filter number? a))
	 (expressions (remove number? a))
	 (numbers-product (apply * numbers)))
    (cond ((= numbers-product 0) 0)
	  ((null? expressions) numbers-product)
	  ((and (= numbers-product 1)
		(null? (cdr expressions)))
	   (car expressions))
	  ((= numbers-product 1) (cons '* expressions))
	  (else (cons '* (cons numbers-product expressions))))))

;; Ideas for the future: simplify expressions like (+ 2 x (+ 4 x)) to become (+ 6 x x)


;; ---- Exercise 2.58
;; a.
(define (sum? x)
  (and (pair? x)
       (= (length x) 3)
       (eq? (cadr x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (addend a) (car a))
(define (augend a) (caddr a))

(define (product? x)
  (and (pair? x)
       (= (length x) 3)
       (eq? (cadr x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))


;; b.
(define (sum? a)
  (not (not (memq '+ a))))

(define (product? a)
  (and (not (sum? a))
       (not (not (memq '* a)))))

(define (up-to item l)
  (define (iter acc remaining)
    (if (or (null? remaining)
	    (eq? item (car remaining)))
	acc
	(iter (append acc (list (car remaining)))
	      (cdr remaining))))
  (iter '() l))

(define (extract-from-parens expr)
  (if (null? (cdr expr))
      (car expr)
      expr))

(define (addend s)
  (extract-from-parens (up-to '+ s)))

(define (augend s)
  (extract-from-parens (cdr (memq '+ s))))

(define (multiplier p)
  (extract-from-parens (up-to '* p)))

(define (multiplicand p)
  (extract-from-parens (cdr (memq '* p))))


;; Section 2.2.3

;; Sets as unordered lists

;; T(n) = \Theta(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

;; T(n) = \Theta(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; T(n, m) = \Theta(nm) => if the size is the same, T(n) = T(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; ---- Exercise 2.59
;; T(n, m) = \Theta(nm)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))

;; ---- Exercise 2.60
;; element-of-set remains the same as before. T(n) = \Theta(n), like before.

;; T(n) = \Theta(1), constant while before it was linear.
(define (adjoin-set x set)
  (cons x set))

;; T(n, m) = \Theta(n + m) => if the size is the same, T(n) = \Theta(n), linear instead of quadratic.
(define (union-set set1 set2)
  (append set1 set2))

;; intersection-of-sets can be defined like before. T(n, m) = \Theta(nm).

;; From a simple order-of-growth comparison, this representation seems way better than the previous one.
;; However, two considerations are needed. First of all, the parameters n and m are not the number of elements in the set,
;; but the number of items stored in the list, some of which may be repeated, thus they are always >= than the n and m from the previous representation.
;; In addition, this representation potentially needs more memory, since it keeps redundant information instead of discarding it when building new sets.
;; Thus, I would consider it in an application which performs intersections much more often than unions, since unions may create sets with many duplicates.
;; I would also consider it if I need to adjoin elements to set much more often than I need to retrieve them or check if they are in the set.

;; Sets as ordered lists
;; T(n) = \Theta(n) but on average halves the steps of the unordered implementation.
(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else 
	 (element-of-set? x (cdr set)))))
		     
;; T(n, m) = \Theta(n + m)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;; ---- Exercise 2.62
;; If x is less than the firse element of the set, we append it straight away, thus on average we perform half of the steps.
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	((> x (car set))
	 (cons (car set)
	       (adjoin-set x (cdr set))))))

;; ---- Exercise 2.63
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set set1 (cdr set2)))))))))
	 
;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set x (left-branch set)))
	((> x (entry set))
	 (element-of-set x (rihgt-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))
	    
	    
;; ---- Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
			    
		     
(define tree1
  '(7 (3 (1 () ())
	 (5 () ()))
      (9 ()
	 (11 () ()))))

(define tree2
  '(3 (1 () ())
      (7 (5 () ())
	 (9 ()
	    (11 () ())))))

(define tree3
  '(5 (3 (1 () ())
	 ())
      (9 (7 () ())
	 (11 () ()))))

;; The two procedures produce the same output for all trees.
;; They traverse the tree the same number of times.
;; However, procedure 1 calls append, which is \Theta(n).

;; Let's assume T_cons(n) + T_null?(n) = c, and T_append(n)=an+b.
;; Let's also assume, for simplicity, that T_1(1) = c, and T_append(1) = a+b
;; Then, for proocedure 1, assuming that each branch has n/2 nodes:
;; T_1(n) = T_null?(n) + T_cons(n) + 2T_1(n/2) + T_append(n) =
;;        = 2T_1(n/2) + T_append(n) + c
;;        = 4T_1(n/4) + T_append(n) + 2T_append(n/2) + 3c =
;;        = ... =
;;        = cn + \Sum_{i=0}^{lg_2(n)} 2^i T_append(n / 2^i) + (n-1)c =
;;        = 2cn + \Sum_{i=0}^{lg_2(n)} (2^i a n/2^i + b) - c =
;;        = 2cn + \Sum_{i=0}^{lg_2(n)} (an+b) -c =
;;        = 2cn + lg_2(n) an + lg_2(n) b - c =
;;        = \Theta(n lg(n))

;; On the other hand, procedure 2 does not call append.
;; T_2(n) = T_null?(n) + T_cons(n) + 2T_2(n/2) =
;;        = 2T_2(n/2) + c =
;;        = 4T_2(n/4) + 3c =
;;        = ... =
;;        = nc + (n-1)c =
;;        = 2nc - n =
;;        = \Theta(n)

;; ---- Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
      (cons '() elements)
      (let ((left-size (quotient (-1+ n) 2)))
	(let ((left-result (partial-tree elements left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elements (cdr left-result))
		(right-size (- n (1+ left-size))))
	    (let ((this-entry (car non-left-elements))
		  (right-result (partial-tree (cdr non-left-elements)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elements (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elements))))))))

;; a.

;; Let us explain how partial-tree works distinguishing recursion step and base case.
;; In the recursive case, let's first assume that if elements has length l and m < n, (partial-tree elements m) correctly produces a list
;; whose car is the tree representation of the first m elements, and whose cdr is the list of remaining l-m elements.
;; The procedure (partial-tree elements n) then conceptually divides the list of elements in four parts:
;; - left: the first a = floor((n-1)/2) elements;
;; - entry: the a+1 th element;
;; - right: the subsequent n-(a+1) elements;
;; - rest: the remaining l-n elements.
;; The elements in left are all < key, while the elements in right and rest are > key.
;; partial-tree is called on the list of elements with a as second argument, thus, since a<n, the recursive call produces a list left-result:
;; (car left-result) is a balanced tree of elements < key, i.e. a left branch for the tree having entry as entry.
;; (cdr left-result) is the list of remaining l-a elements. 
;; The car of (cdr left-result) is the smallest of the remaining elements: i.e., entry.
;; The cdr of (cdr left-result) is the list of the elements greater than entry. The procedure applies partial-tree to it, with n-(a+1) as second argument.
;; This call produces a list whose car is a right-branch for the key contianing n-(a+1) elements, and the cdr is the list of the remaining l-n elements.
;; Finally, the procedure makes a tree from entry, left-branch and right-branch, and returns a list of this tree and the remaining elements.
;; The recursive case is thus explained.

;; The base case is when n = 0. In this case, the procedure returns a list of '() and the remaining elements.
;; '() is a balanced tree containing the first 0 elements of the list, thus the result satisfies the requirements.

;; Having prooved that partial-tree works as expected, we can see that list->tree produces the tree we desire since it calls partial-tree with the full length of the list as second argument,
;; thus obtaining a list of a tree with all the original elements and an empty list, and it returns the aforementioned tree.

(list->tree '(1 3 5 7 9 11))
'(5 (1 ()
       (3 () ()))
    (9 (7 () ())
       (11 () ())))

;; b.
;; I tried solving the recurrent equation, but I found it a bit too confusing. Therefore, I will make a (hopefully) educated guess. 
;; At each step, let's call l the length of the list of elements, and n the number of elements to put in the tree.
;; At each step, we are splitting the problem in two subproblems:
;; - the first one leaves l unchanged, and halves n.
;; - the second one maps (approximately) l to l-n/2, and halves n.
;; We can see that the elements of the list that exceed n are not considered in the computation, as they are simly cons-ed after the tree.
;; Furthermore, in the first step l = n.
;; Therefore, we can argue that the two subproblems effectively halve the original problem.
;; Since at each step we are generating two subproblems each operating on n/2, the number of steps is \Theta(n).

;; ---- Exercise 2.65
;; Since we have \Theta(n) functions to convert trees to ordered lists and viceversa, and \Theta(n) functions to calculate unions and intersections of ordered lists, we can combine them.
(define (union-set-ordered-list set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set-ordered-list (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set-ordered-list (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set-ordered-list set1 (cdr set2)))))))))

(define (union-set tree1 tree2)
  (list->tree
   (union-set-ordered-list
    (tree->list-2 tree1)
    (tree->list-2 tree2))))

(define (intersection-set-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set-ordered-list (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-ordered-list (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-ordered-list set1 (cdr set2)))))))

(define (intersection-set tree1 tree2)
  (list->tree
   (intersection-set-ordered-list
    (tree->list-2 tree1)
    (tree->list-2 tree2))))

;; Sets and information retrieval
;; Data base represented by an unordered list
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))

;; ---- Exercise 2.66
;; Data base represented by a binary tree
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((current-entry (entry set-of-records)))
	(let ((current-key (key current-entry)))
	  (cond ((= given-key current-key) current-entry)
		((< given-key current-key)
		 (lookup given-key
			 (left-branch set-of-records)))
		((> given-key current-key)
		 (lookup given-key
			 (right-branch set-of-records))))))))

(define key car)
(define tree '((4 d) ((2 b) ((1 a) () ()) ((3 c) () ())) ((6 f) ((5 e) () ()) ((7 g) () ((8 h) () ())))))

(lookup 1 tree) ; (1 a)
(lookup 2 tree) ; (2 b)
(lookup 3 tree) ; (3 c)
(lookup 4 tree) ; (4 d)
(lookup 5 tree) ; (5 e)
(lookup 6 tree) ; (6 f)
(lookup 7 tree) ; (7 g)
(lookup 8 tree) ; (8 h)
(lookup 9 tree) ; #f
