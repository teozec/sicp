; Linear recursive process
(define (factorial-rec n)
  (if (= n 1)
      1
      (* n (factorial-rec (- n 1)))))

; Linear iterative process
(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* product counter) (+ counter 1))))
  (iter 1 1))

; Section 1.2.1 Exercises
; ---- Ex 1.9
(define (plus-rec a b) ; Linear recursive process, the stack mantains information about how many times should the recirsion base be incremented.
  (if (= a 0)
      b
      (inc (plus-rec (dec a) b)))) ; Linear iterative process, at each step the pair (a, b) completely describes the state of the process.
(define (plus-iter a b)
  (if (= a 0)
      b
      (plus-iter (dec a) (inc b))))

; ---- Ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(A 1 10) ; 2^10 = 2048
(A 2 4)  ; 2^(2^(2^2)) = 65536
(A 3 3)  ; 2^(2^(2^2)) = 65536

(define (f n) (A 0 n)) ; f(n) = 2n
(define (g n) (A 1 n)) ; g(n) = 2^n
(define (h n) (A 2 n)) ; h(n) = 2^(2^(...^2)) (n times)
