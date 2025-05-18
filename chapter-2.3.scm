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
	 
