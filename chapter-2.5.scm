(load "chapter-2.4.scm")

;; Section 2.5.1
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; ---- Scheme number
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; ---- Rational number
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; ---- Complex numbers
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)
	  (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
	  (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag-polar x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (m a) (tag (make-from-mag-ang m a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang m a)
  ((get 'make-from-mag-ang 'complex) m a))
				    

;; ---- Exercise 2.77
(define (install-complex-accessors)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;; This works because we have already defined the four generic procedures, which currently dispatch on the two representations, rectangular and polar.
;; Adding support for the complex type means that apply-generic will first strip 'complex, and call again the operation on the internal object.
;; Afterwards, apply-generic strips the 'rectangular or 'polar symbol and dispatches to the correct procedure depending on the representation.
;; For example:
;; (magnitude '(complex rectangular 3 . 4))
;; (apply-generic 'magnitude '(complex rectangular 3 . 4)) ; dispatch to the generic magnitude procedure
;; (magnitude '(rectangular 3 . 4))
;; (apply-generic 'magnitude '(rectangular 3 . 4))
;; (<internal-rectangular-package-procedure> '(3 . 4)) ; dispatch to the internal procedure installed by rectangular package

;; ---- Exercise 2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "Bad tagged datum -- CONTENTS" datum))))

;; ---- Exercise 2.79
(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-scheme-number-equ)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (install-rational-equ)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
			  (= (denom x) (denom y)))))
  'done)

(define (install-complex-equ)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
			    (= (imag-part z1) (imag-part z2)))))
  'done)

(define (install-equ)
  (install-scheme-number-equ)
  (install-rational-equ)
  (install-complex-equ))

;; ---- Exercise 2.80
(define (=zero? x)
  (apply-generic '=zero? x))

(define (install-scheme-number-=zero)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-=zero)
  (define (numer x) (car x))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  'done)

(define (install-complex-=zero)
  (put '=zero? '(complex)
       (lambda (z) (= (magnitude z) 0)))
  'done)

;; Section 2.5.2
(define (install-cross-complex-scheme-number-package)
  (define (add-complex-to-scheme-number z x)
    (make-from-real-imag (+ (real-part z) x)
			 (imag-part z)))
  (put 'add '(complex scheme-number)
       (lambda (z x) (tag (add-complex-to-scheme-number z x)))))
 

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
		 (let ((type1 (car type-tags))
		       (type2 (cadr type-tags))
		       (a1 (car args))
		       (a2 (cadr args)))
		   (let ((t1->t2 (get-coercion type1 type2))
			 (t2->t1 (get-coercion type2 type1)))
		     (cond (t1->t2
			    (apply-generic op (t1->t2 a1) a2))
			   (t2->t1
			    (apply-generic op a1 (t2->t1 a2)))
			   (else
			    (error "No method for these types"
				   (list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))
				 
;; ---- Exercise 2.81
(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-exp)
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (attach-tag 'scheme-number (expt x y))))
  'done)

(define (install-identity-coercions)
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  'done)

;; a.
;; If these procedures are installed, using an operation that is not installed on arguments of the same type, such as (exp z1 z2) with complex z1 and z2,
;; reuslts in an infinite recursion, since the apply-generic will not find the procedure but will find the coercions, therefore it recursively apply itself on the same arguments it has been called with.

;; b.
;; The procedure is working only if no identity coercions are installed: in that case, neither the generic procedure nor the coercions are found, and the procedure fails as it should.
;; If the identity coercions are installed, on the other hand, the procedure will enter an infinite loop.
;; Therefore, we can say that the procedure works if the coercions are configured correctly, but it can be made more robust.

;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
		 (let ((type1 (car type-tags))
		       (type2 (cadr type-tags))
		       (a1 (car args))
		       (a2 (cadr args)))
		   (let ((t1->t2 (get-coercion type1 type2))
			 (t2->t1 (get-coercion type2 type1)))
		     (cond ((eq? type1 type2)
			    (error "No method for these types"
				   (list op type-tags)))
			   (t1->t2
			    (apply-generic op (t1->t2 a1) a2))
			   (t2->t1
			    (apply-generic op a1 (t2->t1 a2)))
			   (else
			    (error "No method for these types"
				   (list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))


;; ---- Exercise 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))

    (define (get-coerced-args)
      (define (get-coercions remaining-types)
	(if (null? remaining-types)
	    false
	    (let ((to-type (car remaining-types)))
	      (let ((coercions
		     (map (lambda (type)
			    (if (eq? type to-type)
				(lambda (x) x)
				(get-coercion type to-type)))
			  type-tags)))
		(if (not (memq false coercions))
		    coercions
		    (get-coercions (cdr remaining-types)))))))
      
      (let ((coercions (get-coercions type-tags)))
	(if coercions
	    (fold-right (lambda (coercion arg previous) (cons (coercion arg) previous)) '() coercions args)
	    false)))
	
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((coerced-args (get-coerced-args)))
	    (if (and coerced-args (not (equal? args coerced-args)))
		(apply apply-generic (cons op coerced-args))
		(error "No method for these types"
		       (list op type-tags))))))))

;; This procedure is not suitable in cases where, for example, type A can be coerced to B and both types A and B can be coerced to C, and a mixed operation A C C exists while C C C does not.
;; For example, suppose that the operation of ternary sum is defined for (scheme-number complex complex) but not for (complex complex complex).
;; Suppose that we call the procedure with (scheme-num rational complex).
;; The only possible coerced and not coerced combinations are
;; - (scheme-num rational complex)
;; - (scheme-num complex complex)
;; - (rational rational complex)
;; - (rational complex complex)
;; - (complex rational complex)
;; - (complex complex complex)
;; If we tried all of them, we would be able to evaluate the expression using the (scheme-number complex complex) generic operation.
;; However, our procedure only tries the (complex complex complex) combination, which is not available.


;; ---- Exercise 2.83
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag (inexact->exact (round x)))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))


(define (raise x)
  (apply-generic 'raise x))

(define (make-real x) (attach-tag 'real x))

(define (install-raise)
  (put 'raise '(integer)
       (lambda (n)
	 (make-rational n 1)))
  (put 'raise '(rational)
       (lambda (r)
	 (make-real (/ (car r) (cdr r)))))
  (put 'raise '(real)
       (lambda (x)
	 (make-complex-from-real-imag x 0)))
  'done)

; ---- Exercise 2.84
(define (raise-up-to x type)
  (cond
   ((eq? (type-tag x) type) x)
   ((get 'raise (list (type-tag x)))
    (raise-up-to (raise x) type))
   (else #f)))
  
(define (apply-generic-tower op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
		 (let ((type1 (car type-tags))
		       (type2 (cadr type-tags))
		       (a1 (car args))
		       (a2 (cadr args)))
		   (let ((raised1 (raise-up-to a1 type2))
			 (raised2 (raise-up-to a2 type1)))
		     (cond (raised1
			    (apply-generic op raised1 a2))
			   (raised2
			    (apply-generic op a1 raised2))
			   (else
			    (error "No method for these types"
				   (list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define (install)
  (install-integer-package)
  (install-real-package)
  (install-rational-package)
  (install-polar-package)
  (install-rectangular-package)
  (install-complex-package)
  (install-complex-accessors)
  (install-raise)
  (install-equ))

;; ---- Exercise 2.85
(define (project x)
  (apply-generic 'project x))

(define (numer x) (cadr x))
(define (denom x) (cddr x))

(define (install-project)
  (put 'project '(complex)
       (lambda (z) (make-real (real-part z))))
  (put 'project '(real) ; This loses accuracy, but a correct version requires a more complicated algorithm.
       (lambda (x) (make-rational (inexact->exact (round (contents x))) 1)))
  (put 'project '(rational)
       (lambda (x) (make-integer (round (/ (numer x) (denom x))))))
  'done)

(define (drop x)
  (let ((projection (get 'project (list (type-tag x)))))
    (if projection
	(let ((projected (projection x)))
	  (if (equ? x (raise projected))
	      (drop projected)
	      x))
	x)))

(define (apply-generic-tower op . args)
  (let ((result
	(let ((type-tags (map type-tag args)))
	  (let ((proc (get op type-tags)))
	    (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)
		    (let ((type1 (car type-tags))
			  (type2 (cadr type-tags))
			  (a1 (car args))
			  (a2 (cadr args)))
		      (let ((raised1 (raise-up-to a1 type2))
			    (raised2 (raise-up-to a2 type1)))
			(cond (raised1
			       (apply-generic op raised1 a2))
			      (raised2
			       (apply-generic op a1 raised2))
			      (else
			       (error "No method for these types"
				      (list op type-tags))))))
		    (error "No method for these types"
			   (list op type-tags))))))))
    (drop result)))
