(define (prime? n)
  (define (miller-rabin-test n a)
    (define (checked x)
      (if (= x 1)
	  0
	  x))
    
    (define (checked-square-remainder n m)
      (if (or (= n 1)
	      (= n (-1+ m)))
	  (remainder (square n) m)
	  (checked (remainder (square n) m))))
    
    (define (checked-expmod base exp m)
      (cond ((= exp 0) 1)
	    ((even? exp)
	     (checked-square-remainder
	      (checked-expmod base (/ exp 2) m)
	      m))
	    (else
	     (remainder (* base (checked-expmod base (-1+ exp) m))
			m))))
    
    (not (= (checked-expmod a (-1+ n) n) 0)))
  
  (define (iter step)
    (cond ((> step (1+ (/ n 2))) true)
	  ((miller-rabin-test n step) (iter (+ 1 step)))
	  (else false)))
  (cond	((= n 2) true)
	((even? n) false)
	(else (iter 2))))


(define (identity x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (loop i result)
    (if (= i n)
	result
	(loop (1+ i)
	      (compose f result))))
  (loop 0 (lambda (x) x)))
