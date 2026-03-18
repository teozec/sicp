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

;; Section 3.3.3
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))

(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup-2 key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
	(let ((record (assoc key2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))

(define (insert-2 key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
	(let ((record (assoc key2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key2
			      (cons key2 value))
			(cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key1
				  (cons key2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))
  
  
;; Exercise 3.24
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))

    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key1
				  (cons key2 value))
			    (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc) insert!)
	    (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define (get key1 key2 table) 
  ((table 'lookup-proc) key1 key2))

(define (put! key1 key2 value table) 
  ((table 'insert-proc) key1 key2 value))


;; Exercise 3.25
(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup keys)
      (define (iter keys table)
	(if (null? keys)
	    (cdr table)
	    (let ((subtable (assoc (car keys) (cdr table))))
	      (if subtable
		  (iter (cdr keys) subtable)
		  false))))
      (iter keys local-table))
      
    (define (insert! keys value)
      (define (iter keys table)
	(if (null? keys)
	    (set-cdr! table value)
	    (begin
	      (unless (pair? (cdr table)) (set-cdr! table '()))
	      (let ((subtable (assoc (car keys) (cdr table))))
		(if subtable
		    (begin 
		      (iter (cdr keys) subtable))
		    (begin
		      (let ((new-subtable (cons (cons (car keys) '())
						(cdr table))))
			(set-cdr! table new-subtable)
			(iter (cdr keys) (car new-subtable)))))))))
      (iter keys local-table)
      'ok)
     
    (define (print)
      (display local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc) insert!)
	    ((eq? m 'display-proc) (print))
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (get keys table) 
  ((table 'lookup-proc) keys))

(define (put! keys value table) 
  ((table 'insert-proc) keys value))

;; Exercise 3.26

;; Tree operations
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (cddr tree))
(define (make-tree entry left right)
  (cons entry (cons left right)))
(define (set-left-branch! tree left)
  (set-car! (cdr tree) left))
(define (set-right-branch! tree right)
  (set-cdr! (cdr tree) right))

(define (key-entry entry) (car entry))
(define (value-entry entry) (cdr entry))
(define (make-entry key value) (cons key value))

(define (lookup key table)
  (define (iter subtable)
    (if (null? subtable)
	false
	(let ((current-entry (entry subtable)))
	  (let ((current-key (key-entry current-entry)))
	    (cond ((= key current-key) (value-entry current-entry))
		  ((< key current-key) (iter (left-branch subtable)))
		  ((> key current-key) (iter (right-branch subtable))))))))
  (iter (cdr table)))

(define (insert! key value table)
  (define (iter! subtable)
    (let ((current-entry (entry subtable)))
      (let ((current-key (key-entry current-entry)))
	(cond ((= key current-key)
	       (set-cdr! current-entry value))
	      ((< key current-key)
	       (if (null? (left-branch subtable))
		   (set-left-branch! subtable (make-tree (make-entry key value) '() '()))
		   (iter! (left-branch subtable))))
	       ((> key current-key)
		(if (null? (right-branch subtable))
		    (set-right-branch! subtable (make-tree (make-entry key value) '() '()))
		    (iter! (right-branch subtable))))))))
  
  (if (null? (cdr table))
      (set-cdr! table (make-tree (make-entry key value) '() '()))
      (iter! (cdr table)))
  'ok)

(define (make-table)
  (list '*table*))

;; Exercise 3.27
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))

;; The diagram is too complicated to draw here.

;; (memo-fib n) recurses n times, since (memo-fib i) gets called only the first time it is needed,
;; and afterwards it is put in the table.
;; If lookup and insert are O(1), memo-fib is O(n).
;; Otherwise, I am not sure.

;; (memoize fib) would not be optimized, since the recursive calls would evaluate fib,
;; which is the standard non-memoized procedure.


;; Section 3.3.4
;; a)
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((not (valid-signal? a1))
	 (error "Invalid signal" a1))
	((not (valid-signal? a2))
	 (error "Invalid signal" a2))
	(else (and a1 a2))))

(define (valid-signal? s)
  (or (= s 0)
      (= s 1)))

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((not (valid-signal? a1))
	 (error "Invalid signal" a1))
	((not (valid-signal? a2))
	 (error "Invalid signal" a2))
	(else (or a1 a2))))


;; Exercise 3.29
;; According to De Morgan law, (or a b) == (not (and (not a) (not b)))
(define (compound-or-gate a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire)) (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)))

;; The delay is and-gate-delay + 2 * inverter-delay, since the signal flows in two inverters and one and.
    
;; Exercise 3.30
(define (ripple-carry a-list b-list s-list c)
    (if (null? a-list)
	'ok
	(let ((next-c (make-wire)))
	  (full-adder (car a-list)
		      (car b-list)
		      next-c
		      (car s-list)
		      c)
	  (ripple-carry (cdr a-list)
			(cdr b-list)
			(cdr s-list)
			next-c))))
			  
;; The delay to get the full carry bit is n * full-adder-carry-delay.
;; The full-adder-carry-delay is the delay introduced chanigng the carry in the full adder.
;; This is the or-delay plus two times the half-adder-carry-delay.
;; The half adder carry delay is 2 *and-delay + inverter-delay.
;; The total delay is then n * (or-delay + 2 * (2 * and-delay + inverter-delay)).
;; To get also the last sum bit, we need to account for the delay introduced by the or-gate in the last half adder.
;; If it is longer than the and-delay + inverter-delay, we need to add the difference between them.

;; b)
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-signal)
		 (call-each action-procedures))
	  'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operaton -- WIRE" m))))

  dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display "  New value = ")
		 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

;; Exercise 3.31
;; The initialization is required to ensure that the function boxes are working correctly.
;; Without it, the output may be incorrect until all their inputs have changed at lease once.
;; For example, supposing that input and output are initially set to 0, calling (inverter input output)
;; would not change the value of output to 1 after the inverter delay, as expected.
;; In the previous half adder with the two inputs set to 0,
;; at a first glance it would seem to work since the outputs are new wires with 0 signal.
;; However, after changing input-1 to 1, only the internal D wire would be updated,
;; but the value of E would not change (since C is not changed). This means that S would still be 0, incorrectly.
;; Only after changing also B to 1, the value of C is updated, causing E to be set correctly and S to be 0.
;; Afterwards, the adder will work correctly on input signal change.

;; c)
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))


;; Exercise 3.32
(define w1 (make-wire))
(define w2 (make-wire))
(define o (make-wire))

(and-gate w1 w2 o)

;; First we setup the gate
(set-signal! w1 0)
(propagate)
(set-signal! w2 1)
(propagate)

;; At this point o has a signal of 0.
;; Now suppose that we change the input signals
(set-signal! w1 1)
(set-signal! w2 0)
(propagate)

;; The set-signal! calls set the signal on the wire immediately, and runs the action procedures attached by the gate to the wires.
;; The first procedure is ran when both w1 and w2 are 1. Therefore, it instructs the agenda to set the output to 1 after the delay.
;; The second procedure is ran when w1 is 1 and w2 is 0. Therefore, it instructs the agenda to set the output to 0 after the delay.
;; If we run them in FIFO order, the and gate will correctly output a 0. If we run them in LIFO order instead, it will output a 1.
;; We need FIFO order because each procedure operates on the signal values that were present when it was inserted in the agenda,
;; therefore new procedures contain updated information that needs to be used later.

;; In this particular case, setting w2 before w1 would have worked also in LIFO order, since first both inputs are 0, and then w1 is changed to 1,
;; meaning that the output is 0 for both procedures. This is however due to chance, and for example a or gate would have reported the wrong value in this situation.


