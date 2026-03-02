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

;; Section 3.3.2
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))
		      
;; Exercise 3.21
(define q1 (make-queue))

(insert-queue! q1 'a)
;; ((a) a)
;; The queue was empty. Inserting a sets it as the first element of the list structure, and also as the rear ptr.
;; The car is the front-pointer, pointing to list (a), which represents the current list of elements in the queue.
;; The cdr is the rear-pointer, which points to the last element of the queue, a.

(insert-queue! q1 'b)
;; ((a b) b)
;; The queue was not empty. Inserting b puts it at the end of the list structure, and also as the rear ptr.
;; The car is the front-pointer, pointing to list (a b), which represents the current list of elements in the queue.
;; The cdr is the rear-pointer, which points to the last element of the queue, b.

(delete-queue! q1)
;; ((b) b)
;; The resulting queue is not empty. Deleting the first element removes it from the list structure, but does not change the rear ptr.
;; The car is the front-pointer, pointing to list (b), which represents the current list of elements in the queue.
;; The cdr is the rear-pointer, which points to the last element of the queue, b.

(delete-queue! q1)
;; (() b)
;; The resulting queue is empty. Deleting the first element removes it from the list structure, but does not change the rear ptr.
;; The car is the front-pointer, pointing to list (), which represents the current list of elements in the queue.
;; The cdr is the rear-pointer, which has no meaning in a empty queue: no operation on the queue uses it.

(define (print-queue queue)
  (display (car queue)))

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue")
	  (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch))))

    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     dispatch)))

    (define (print-queue)
      (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'print-queue) print-queue)))

    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))
    
(define (print-queue queue)
  ((queue 'print-queue)))

;; Exercise 3.23
(define (front-ptr-dequeue dequeue) (car dequeue))
(define (rear-ptr-dequeue dequeue) (cdr dequeue))
(define (set-front-ptr-dequeue! dequeue item) (set-car! dequeue item))
(define (set-rear-ptr-dequeue! dequeue item) (set-cdr! dequeue item))

(define (empty-dequeue? queue) (null? (front-ptr queue)))
(define (make-dequeue) (cons '() '()))

(define (make-element-dequeue item previous next)
  (cons item (cons previous next)))

(define (item-element element)
  (car element))
(define (previous-element element)
  (cadr element))
(define (next-element element)
  (cddr element))

(define (set-previous-element! element previous)
  (set-car! (cdr element) previous))

(define (set-next-element! element next)
  (set-cdr! (cdr element) next))

(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with an empty dequeue" dequeue)
      (item-element (front-ptr-dequeue dequeue))))

(define (rear-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "REAR called with an empty dequeue" dequeue)
      (item-element (rear-ptr-dequeue dequeue))))

(define (rear-insert-dequeue! dequeue item)
  (let ((new-pair
	 (make-element-dequeue item
			       (rear-ptr-dequeue dequeue)
			       '())))
    (cond ((empty-dequeue? dequeue)
	   (set-front-ptr-dequeue! dequeue new-pair)
	   (set-rear-ptr-dequeue! dequeue new-pair)
	   dequeue)
	  (else
	   (set-previous-element! new-pair (rear-ptr-dequeue dequeue))
	   (set-next-element! (rear-ptr-dequeue dequeue) new-pair)
	   (set-rear-ptr-dequeue! dequeue new-pair)
	   dequeue))))

(define (front-insert-dequeue! dequeue item)
  (let ((new-pair
	 (make-element-dequeue item
			       '()
			       (front-ptr-dequeue dequeue))))
    (cond ((empty-dequeue? dequeue)
	   (set-front-ptr-dequeue! dequeue new-pair)
	   (set-rear-ptr-dequeue! dequeue new-pair)
	   dequeue)
	  (else
	   (set-next-element! new-pair (front-ptr-dequeue dequeue))
	   (set-previous-element! (front-ptr-dequeue dequeue) new-pair)
	   (set-front-ptr-dequeue! dequeue new-pair)
	   dequeue))))

(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "FRONT-DELETE! called with an empty queue" dequeue))
	(else
	 (begin
	   (let ((next (next-element (front-ptr dequeue))))
	     (set-front-ptr-dequeue! dequeue next)
	     (if (null? next) 
		 (set-rear-ptr-dequeue! dequeue '()) ; If removing the last element from the queue, set also the rear
		 (set-previous-element! next '())) ; Otherwise, make it have no previous
	     dequeue)))))

(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "REAR-DELETE! called with an empty queue" dequeue))
	(else
	 (begin
	   (let ((previous (previous-element (rear-ptr dequeue))))
	     (set-rear-ptr-dequeue! dequeue previous)
	     (if (null? previous) 
		 (set-front-ptr-dequeue! dequeue '()) ; If removing the last element from the queue, set also the front
		 (set-next-element! previous '())) ; Otherwise, make it have no next
	     dequeue)))))

(define (print-dequeue dequeue)
  (define (loop q l)
    (if (null? q)
	l
	(loop (next-element q)
	      (append l (list (item-element q))))))
  (display (loop (front-ptr-dequeue dequeue) '())))
