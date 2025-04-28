; Section 1.3.1

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (1+ a) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (1+ a) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ 2 a))) (pi-sum (+ 4 a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (identity x) x)
(define (sum-integers-sum a b)
  (sum identity a 1+ b))

(define (sum-cubes-sum a b)
  (sum cube a 1+ b))

(define (pi-sum-sum a b)
  (define (term n) (/ 1.0 (* n (+ 2 n))))
  (define (next n) (+ 4 n))
  (sum term a next b))

(define (integral f a b dx)
  (define (next x) (+ x dx))
  (* (sum f
       (+ a (/ dx 2.0))
       next
       b)
     dx))
(integral cube 0 1 0.001)

; ---- Exercise 1.29
(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define a1 (+ a h))
  (define an (+ a (* n h)))
  (define y0 (f a))
  (define yn (f an))
  
  (define (term a) (+ (* 4 (f a))
		      (* 2 (f (+ a h)))))
  (define (next a) (+ a (* 2 h)))

  (* h
     (/ 1.0 3)
     (+ y0
	yn
	(sum term a1 next an))))

(simpson-rule cube 0 1 100)
(simpson-rule cube 0 1 1000)

; ---- Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (+ result (term a)))))
  (iter a 0))

					;
; ---- Exercise 1.31
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product-rec term
		  (next a)
		  next
		  b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (* result (term a)))))
  (iter a 1))

(define (factorial-rec n)
  (define (identity x) x)
  (product-rec identity 1 1+ n))
(define (factorial-iter n)
  (define (identity x) x)
  (product-iter identity 1 1+ n))

(define (pi-wallis n)
  (define (term i)
    (* (/ (* 2 i)
	  (- (* 2 i) 1.0))
       (/ (* 2 i)
	  (+ (* 2 i) 1.0))))
  (* 2 (product-iter term 1 1+ n)))

; ---- Exercise 1.32
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner
			    null-value
			    term
			    (next a)
			    next
			    b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner result (term a)))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate-rec + 0 term a next b))

(define (product-acc term a next b)
  (accumulate-iter * 1 term a next b))

; ---- Exercise 1.33
(define (filtered-accumulate-rec combiner null-value filter term a next b)
  (cond ((> a b) null-value)
	((filter a) (combiner (term a)
			      (filtered-accumulate-rec combiner
						       null-value
						       filter
						       term
						       (next a)
						       next
						       b)))
	(else (filtered-accumulate-rec combiner
				       null-value
				       filter
				       term
				       (next a)
				       next
				       b))))

(define (filtered-accumulate-iter combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((filter a) (iter (next a)
			    (combiner result (term a))))
	  (else (iter (next a) result))))
  (iter a null-value))

(define (filtered-sum filter term a next b)
  (filtered-accumulate-rec + 0 filter term a next b))

(define (filtered-product filter term a next b)
  (filtered-accumulate-iter * 1 filter term a next b))

(define (sum-square-of-primes a b)
  (filtered-sum prime? square a 1+ b))

(define (product-of-coprimes n)
  (define (coprime? m)
    (= (gcd n m) 1))
  (filtered-product coprime? identity 1 1+ n))


; Section 1.3.2
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
	(* y b)
	(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; ---- Exercise 1.34
(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))

; (f f)
; Using the substitution model, we obtain at each step:
; (f 2)
; (2 2)
; The object 2 is not applicable: it is not a procedure.

; Section 1.3.3
(define (average x y)
  (/ (+ x y) 2.0))

(define (half-interval-method f a b)
  (define (search neg-point pos-point)
    (define (close-enough? x y)
      (< (abs (- x y)) 0.001))
    (let ((midpoint (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
	  midpoint
	  (let ((test-value (f midpoint)))
	    (cond ((> test-value 0)
		   (search neg-point midpoint))
		  ((< test-value 0)
		   (search midpoint pos-point))
		  (else midpoint))))))

  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (> a-value 0) (< b-value 0))
	   (search b a))
	  ((and (< a-value 0) (> b-value 0))
	   (search a b))
	  (else
	   (error "Values are not of opposite sign" a b)))))


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  guess
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

; ---- Exercise 1.35
; \phi^2 = \phi + 1
; Dividing by \phi, \phi = 1 + 1 / \phi, meaning that \phi is the fixed point of the transformation x -> 1 + 1 / x.
(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

; ---- Exercise 1.36
(define (fixed-point-traced f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  guess
	  (try next))))
  (try first-guess))

; Without average-damping. 33 steps.
(fixed-point-traced
 (lambda (x) (/ (log 1000)
		(log x)))
 2.0)

------------------------------------------------------------; With average-damping. 8 steps.
(fixed-point-traced
 (lambda (x) (average x
		      (/ (log 1000)
			 (log x))))
 2.0)

; ---- Exercise 1.37
(define (cont-frac-rec n d k)
  (define (rec i)
    (let ((ni (n i))
	  (di (d i)))
      (if (= i k)
	  (/ ni di)
	  (/ ni (+ di (rec (1+ i)))))))
  (rec 1))

(define (phi-rec k)
  (/ 1
     (cont-frac-rec (lambda (i) 1.0)
		    (lambda (i) 1.0)
		    k)))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
	result
	(iter (-1+ i)
	      (/ (n i)
		 (+ (d i) result)))))
  (iter k 0))

(define (phi-iter k)
  (/ 1
     (cont-frac-iter (lambda (i) 1.0)
		    (lambda (i) 1.0)
		    k)))

; ---- Exercise 1.38
(define (e-euler k)
  (+ 2
     (cont-frac-iter (lambda (i) 1.0)
		     (lambda (i)
		       (cond ((= i 1) 1)
			     ((= i 2) 2)
			     ((not (= (remainder i 3) 2)) 1)
			     (else (* 2 (1+ (quotient i 3))))))
		     k)))

; ---- Exercise 1.39
(define (tan-cf x k)
  (cont-frac-iter (lambda (i)
		    (if (= i 1)
			x
			(- (square x))))
		  (lambda (i)
		    (-1+ (* 2 i)))
		  k))

; Section 1.3.4
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  guess
	  (try next))))
  
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
	  (f x))
       dx)))
(define dx 0.00001)
(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-damp x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(define (sqrt-newton x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

; ---- Exercise 1.40
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; ---- Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) 1+) 5) ; 5 + 16 = 21
; First of all, we consider (double (double double)).
; (double double) is a procedure that takes a procedure as argument. Its double is the following (by substitution):
(lambda (f)
  ((double double) ((double double) f)))
; Then, we analyze (double double), which is
(lambda (g)
  (double (lambda (x) (g (g x)))))
(lambda (g)
  (lambda (x) (g (g (g (g x))))))
; Substituting the external (double double) in the previous expression, which has as argument the procedure ((double double) f), we obtain
(lambda (f)
  (lambda (x)
    (((double double) f)
     (((double double) f)
      (((double double) f)
       (((double double) f) x))))))
; Finally, we can substitute the remaining (double double), all of which have as argument f.
(lambda (f)
  (lambda (x)
    (f (f (f (f
	      (f (f (f (f
			(f (f (f (f
				  (f (f (f (f x))))))))))))))))))
; Which applies the f function 16 times. In our example, f is 1+, so ((double (double double)) 1+) is equal to
(lambda (x) (+ x 16))
; Which, applied to 5, gives 21.

; ---- Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; ---- Exercise 1.43
(define (repeated f n)
  (define (loop i result)
    (if (= i n)
	result
	(loop (1+ i)
	      (compose f result))))
  (loop 0 (lambda (x) x)))

; ---- Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3.0)))
(define (n-folded-smooth f n)
  (lambda (x)
    ((repeated smooth n) x)))

; ---- Exercise 1.45
; The number of needed damps increases by 1 everytime n doubles.
(define (root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (-1+ n))))
			    (repeated average-damp (truncate (log2 n)))
			    1.0))

; ---- Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? next guess)
	  guess
	  (iter next))))
  iter)

(define (sqrt x)
  (define (good-enough? next guess)
    (< (/ (abs (- next guess))
	  guess)
       0.000001))
  (let ((improve (average-damp (lambda (guess) (/ x guess)))))
    (let ((improve-loop (iterative-improve good-enough? improve)))
      (improve-loop 1.0))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (let ((improve-loop (iterative-improve close-enough? f)))
    (improve-loop first-guess)))


