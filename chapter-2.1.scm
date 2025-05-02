; Section 2.1.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (equal-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; ---- Exercise 2.1
; This implementation assumes that gcd is always positive, even if one of the two numbers is negative (i.e. it uses euclidean-remainder).
(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((g (gcd n d)))
	(cons (/ n g) (/ d g)))))

; Section 2.1.2
(define (make-rat-defer n d)
  (cons n d))
(define (numer-defer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom-defer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

; ---- Exercise 2.2
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint p q)
  (make-point (/ (+ (x-point p) (x-point q))
		 2)
	      (/ (+ (y-point p) (y-point q))
		 2)))
(define (midpoint-segment segment)
  (midpoint (start-segment segment) (end-segment segment)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; ---- Exercise 2.3
; I assume rectangles aligned to the grid, for simplicity.
; TODO: allow rotations
;   *---------* q
;   |         |
;   |         |
; p *---------*
(define (make-rectangle p q)
  (cons p q))
(define (p-rect r)
  (car r))
(define (q-rect r)
  (cdr r))

(define (perimeter-rect r)
  (* 2 (+ (- (x-point (q-rect r)) (x-point (p-rect r)))
	  (- (y-point (q-rect r)) (y-point (p-rect r))))))

(define (area-rect r)
  (* (- (x-point (q-rect r)) (x-point (p-rect r)))
     (- (y-point (q-rect r)) (y-point (p-rect r)))))

(define (make-rectangle p q)
  (make-segment p q))
(define (p-rect r)
  (start-segment r))
(define (q-rect r)
  (end-segment r))

; Section 2.1.3
(define (cons-f x y)
  (lambda (m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- CONS" m)))))
(define (car-f z) (z 0))
(define (cdr-f z) (z 1))

; ---- Exercise 2.4
(define (cons-l x y)
  (lambda (m) (m x y)))
(define (car-l z)
  (z (lambda (p q) p)))
; (car (cons x y))
; ((cons x y) (lambda (p q) p))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x
(define (cdr-l z)
  (z (lambda (p q) q)))

; ---- Exercise 2.5
(define (cons-p a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (split p)
  (define (loop a-part b-part)
    (if (even? b-part)
	(loop (* a-part 2) (/ b-part 2))
	(cons a-part b-part)))
  (loop 1 p))

(define (car-p p)
  (log2 (car (split p))))

(define (cdr-p p)
  (/ (log (cdr (split p)))
     (log 3)))

; ---- Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; In this representation, the number n is a function of a functional argument f, which returns the function that applies f n times.

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x)))) 
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
; (lambda (f) (lambda (x) (f x)))
; (lambda (f) f)
(define one (lambda (f) (lambda (x) (f x)))) ; I kept the version with (lambda (x) ...) for comparison with zero and two.
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))



; Section 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

; --- Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; ---- Exercise 2.8
(define (sub-interval x y)
  (add-interval x
		(make-interval (- (upper-bound y))
			       (- (lower-bound y)))))

; ---- Exercise 2.9
; I will use standard notation instead of s-expressions

; 2 * width([x1, x2]) = x1 - x2
; 2 * width([y1, y2]) = y1 - y2

; 2 * width([x1, x2] + [y1, y2]) = 
; = 2 * width([x1+y1, x2+y2]) =
; = x1 + y1 - x2 -y2 =
; = x1 - x2 + y1 -y2 =
; = 2 * width([x1, x2]) + 2 * width([y1, y2])

; 2 * width([x1, x2] - [y1, y2]) =
; = 2 * width([x1-y2, x2-y1]) =
; = x1 - y2 - x2 + y1 =
; = x1 - x2 - y1 + y2 =
; = 2 * width([x1, x2]) - 2 * width([y1, y2])



; ----- Exercise 2.10
(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
	   (> (upper-bound y) 0))
      (error "Division by an inteval containing 0 - DIV-INTERVAL")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

; ---- Exercise 2.11
(define (mul-interval x y)
  (let ((xl (lower-bound x))
	(xu (upper-bound x))
	(yl (lower-bound y))
	(yu (upper-bound y)))
    (cond ((> xl 0)
	   (cond ((> yl 0) (make-interval (* xl yl) (* xu yu)))
		 ((> yu 0) (make-interval (* xu yl) (* xu yu)))
		 (else (make-interval (* xu yu) (* xl yl)))))
	  ((> xu 0)
	   (cond ((> yl 0) (make-interval (* xl yu) (* xu yu)))
		 ((> yu 0)
		  (let ((p1 (* xl yl))
			(p2 (* xl yu))
			(p3 (* xu yl))
			(p4 (* xu yu)))
		    (make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))
		 (else (make-interval (* xu yl) (* xl yu)))))
	  (else
	   (cond ((> yl 0) (make-interval (* xu yu) (* xl yl)))
		 ((> yu 0) (make-interval (* xu yu) (* xu yl)))
		 (else (make-interval (* xl yl) (* xu yu))))))))

	    
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; ---- Exercise 2.12
(define (make-center-percentage c p)
  (make-center-width c (* c p)))

(define (percentage-width i)
  (/ (width i) (center i)))

; ---- Exercise 2.13
; a (1 \pm x) b (1 \pm y) =
; = ab (1 \pm x) (1 \pm y) =
; = ab (1 \pm x \pm y \pm xy)
; \approx ab (1 \pm x \pm y)
; The approximate formula for the percentage, if the two original percentages are x and y, is x+y.


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		   (add-interval (div-interval one r1)
				 (div-interval one r2)))))

; ---- Exercise 2.14
(par1 (make-interval 100 200) (make-interval 20 40)) ; [8.33, 66.67]
(par2 (make-interval 100 200) (make-interval 20 40)) ; [16.67, 33.33]

(define (print-center-percentage i)
  (newline)
  (display (center i))
  (display " [")
  (display (* 100 (percentage-width i)))
  (display "%]"))

(let ((a (make-center-percentage 100 0.05))
      (b (make-center-percentage 20 0.01)))
  (print-center-percentage (div-interval a a))
  (print-center-percentage (div-interval a b))
  (print-center-percentage (div-interval b a))
  (print-center-percentage (div-interval b b)))

; When dividing two intervals, the percentage width is approximately the sum of the two percentage widths.
; Therefore, when dividing an interval by itself the percentage witdh of the result is the double of the original interval one.

; ---- Exercise 2.15
; per2 is a better program because it considers the interval uncertainty only once, for each interval,
; whereas par1 treats the two R1 as different intervals even though they have the same value, this increasing the interval width unnecessarily.

; ---- Exercise 2.16
; If the same interval appears more than once in an expression, we are treating it as if it could have different values in the interval each time.
; For example, A/A will be 1 with a percentage width about double of the width of A, while in reality it should be exactly 1: we are not sure of the value of A, but dividing it by itself is surely 1.
; I won't try to make a general interval-arithmetic package that does not have this shortcoming :-) Maybe some other time.
