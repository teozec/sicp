;; Section 3.1.1

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
      
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds!"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds!")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; Exercise 3.1
(define (make-accumulator a)
  (lambda (x)
    (set! a (+ a x))
    a))

;; Exercise 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) count)
	    ((eq? x 'reset-count)
	     (set! count 0)
	     #t)
	    (else
	     (set! count (1+ count))
	     (f x))))))

;; Exercise 3.3
(define (make-password-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (error "Invalid password -- MAKE-PASSWORD-ACCOUNT" p))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-PASSWORD-ACCOUNT" m))))
  dispatch)

;; Exercise 3.4
(define (call-the-cops) (error "call-the-cops"))
(define (make-safe-account balance password)
  (let ((tries 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds!"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
   
    (define (handle-invalid-password)
      (set! tries (1+ tries))
      (if (>= tries 7) (call-the-cops))
      (error "Invalid password"))
	      
    (define (dispatch p m)
      (if (eq? p password)
	  (begin (set! tries 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Unknown request -- MAKE-PASSWORD-ACCOUNT" m))))
	  (handle-invalid-password)))
    dispatch))


;; Section 3.1.2
(define (make-rand a c m seed)
  (define (rand-update x)
    (modulo (+ (* x a) c) m))
  (lambda ()
    (set! seed (rand-update seed))
    seed))

(define knuth-rand (make-rand 69069 1 (expt 2 32) 19380110))
(define rand (make-rand 16807 0 2147483647 19380110))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (-1+ trials-remaining) (1+ trials-passed)))
	  (else
	   (iter (-1+ trials-remaining) trials-passed))))
  (iter trials 0))

	      
;; Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (p x y)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials experiment) area)))

(define (estimate-pi-integral trials)
  (define (unit x y)
    (< (+ (square x) (square y)) 1))
  (estimate-integral unit -1. 1. -1. 1. trials))

;; Exercise 3.6
(define (make-rand-new a c m seed)
  (define (rand-update x)
    (modulo (+ (* x a) c) m))
  (define (generate)
    (set! seed (rand-update seed))
    seed)
  (define (reset new-seed)
    (set! seed new-seed)
    #t)

  (lambda (op)
    (cond ((eq? op 'generate) (generate))
	  ((eq? op 'reset) reset)
	  (else (error "Unknown op -- MAKE-RAND-NEW" op)))))
    

;; Exercise 3.7

(define (make-simple-bank-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-PASSWORD-ACCOUNT" m))))
  dispatch)

(define (make-protected-bank-account account password)
  (let ((tries 0))
    (define (handle-invalid-password)
      (set! tries (1+ tries))
      (if (>= tries 7) (call-the-cops))
      (error "Invalid password"))
    
    (define (dispatch p m)
      (if (eq? p password)
	  (begin (set! tries 0)
		 (cond ((eq? m 'check) #t)
		       ((eq? m 'simple) account)
		       (else (account m))))
	  (handle-invalid-password)))
    dispatch))


(define (make-bank-account balance password)
  (make-protected-bank-account (make-simple-bank-account balance) password))

(define (make-joint account password new-password)
  (if (account password 'check)
      (make-protected-bank-account (account password 'simple) new-password)))

;; Exercise 3.8
(define f
  (let ((next 0))
    (lambda (x)
      (let ((cur next))
	(set! next x)
	cur))))
  
