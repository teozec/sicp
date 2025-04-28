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
