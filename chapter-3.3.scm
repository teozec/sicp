;; Section 3.3

;; 3.3.1
;; a
;; Exercise 3.12
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z ; (a b c d)
(cdr x) ; (b)


;;    
;; x->|*|*|->|*|/|
;;     |      |
;;     V      V
;;     a      b
;;
;;     +-------------+
;;     |             |
;;     V             |
;; y->|*|*|->|*|/|   |
;;     |      |      |
;;     V      V      |
;;     c      d      |
;;                   |
;;                   |
;; z->|*|*|->|*|*|---+
;;     |      |
;;     V      V
;;     a      b

(define w (append! x y))

w ; (a b c d)
(cdr x) ; (b c d)


;;     w
;;     |
;;     V
;; x->|*|*|->|*|*|---+
;;     |      |      |
;;     V      V      |
;;     a      b      |
;;                   |
;;     +-------------+
;;     |
;;     V
;; y->|*|*|->|*|/|
;;     |      |
;;     V      V
;;     c      d


;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;;
;;     +--------------------+
;;     |                    |
;;     V                    |
;; z->|*|*|->|*|*|->|*|*|---+
;;     |      |      |
;;     V      V      V
;;     a      b      c

;; (last-pair z) would result in a infinite loop

;; Exercise 3.14
(define (mistery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;; The procedure reverses the x list without cloning data or creating new pairs,
;; but destroying the original list (leaving it equal to the cdr of its last pair).
(define v (list 'a 'b 'c 'd))
(define w (mistery v))

v ; (d)
w ; (d c b a)

;;                          v
;;                          |
;;                          V
;; w->|*|*|->|*|*|->|*|*|->|*|/|
;;     |      |      |      |
;;     V      V      V      V
;;     a      b      c      d


;; b
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1 ; ((a b) a b)
(set-to-wow! z1) ; ((wow b) wow b)
z2 ; ((a b) a b)
(set-to-wow! z2) ; ((wow b) a b)

;; Exercise 3.15
;; z1 -> |*|*|
;;        | |
;;        | ++
;;        |  V
;;       x+>|*|*|
;;           | |
;;           V V
;;           a b

;; z1 -> |*|*|
;;        | |
;;        | ++
;;        |  V
;;       x+>|*|*|
;;           | |
;;           V V
;;         wow b

;; z2 -> |*|*|
;;        | |
;;        | +---------->|*|*|
;;        +>|*|*|        | |
;;           | |         V V
;;           V V         a b
;;           a b

;; z2 -> |*|*|
;;        | |
;;        | +---------->|*|*|
;;        +>|*|*|        | |
;;           | |         V V
;;           V V         a b
;;         wow b

;; Exercise 3.16
;; 3: |*|*|->|*|/|
;;     |
;;     V
;;    |*|/|

;; 4: |*|*|
;;     | |
;;     | +----+
;;     V      V
;;    |/|*|->|*|/|

;; 7: |*|*|
;;     | |
;;     | V
;;     +>|*|*|
;;        | |
;;        | V
;;        +>|*|*|

;; Inf:  +->|*|/|
;;       |   |  
;;       +---+

;; Exercise 3.17
(define (count-pairs l)
  (define count
    (let ((visited '()))
      (lambda (x)
	(cond ((not (pair? x)) 0)
	      ((memq x visited) 0)
	      (else
	       (set! visited (cons x visited))
	       (+ (count (car x))
		  (count (cdr x))
		  1))))))
  (count l))

;; Exercise 3.18
(define (cycle-list? l)
  (define (iter x visited)
    (cond ((null? x) #f)
	  ((memq x visited) #t)
	  (else (iter (cdr x) (cons x visited)))))
  (iter l '()))

;; Exercise 3.19
(define (cycle-list? l)
  (define (nth n l)
    (cond ((null? l) l)
	  ((eq? n 0) l)
	  (else (nth (-1+ n) (cdr l)))))

  (define (iter x n)
    (let ((xn (nth n x)))
      (cond ((null? x) #f)
	    ((eq? x xn) #t)
	    (else (iter xn (1+ n))))))

  (iter l 1))
;; TODO: proof

;; c
(define (cons-1 x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car-1 z) (z 'car))
(define (cdr-1 z) (z 'cdr))

(define (cons-2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car) set-x!)
	  ((eq? m 'set-cdr) set-y!)
	  (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car-2 z) (z 'car))
(define (cdr-2 z) (z 'cdr))
(define (set-car-2! z new-value)
  ((z 'set-car) new-value)
  z)
(define (set-cdr-2! z new-value)
  ((z 'set-cdr) new-value)
  z)

;; Exercise 3.20
;; Diagram on paper for the moment (too complicated).
