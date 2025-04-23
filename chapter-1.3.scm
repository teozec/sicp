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
