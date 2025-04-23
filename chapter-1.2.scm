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


; Section 1.2.2

; Tree-recursive, O(e^n)
(define (fib-rec n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib-rec (- n 1))
		 (fib-rec (- n 2))))))

; Linear iterative, O(n)
(define (fib-iter n)
  (define (iter a b step)
    (if (= step n)
	b
	(iter (+ a b)
	      a
	      (+ 1 step))))
  (iter 1 0 0))

; Tree recursive, but much harder to develop an iterative version.
(define (count-change amount)
  (define (cc remaining-amount kinds-of-coins)
    (cond ((= remaining-amount 0) 1)
	  ((< remaining-amount 0) 0)
	  ((= kinds-of-coins 0) 0)
	  (else (+ (cc remaining-amount (- kinds-of-coins 1))
		   (cc (- remaining-amount (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (cc amount 5))


; ---- Ex. 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter a b c step)
    (if (= step n)
	c
	(iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (+ step 1))))
      (iter 2 1 0 0))

; ---- Ex 1.12
; The procedure does not handle invalid input (negative numbers, index > line).
(define (pascal line index)
  (if (or (= index 0) (= index line))
      1
      (+ (pascal (- line 1) (- index 1))
	 (pascal (- line 1) index))))

; ---- Ex 1.13
; I had to get some hints for this one.

; Let \phi = (1 + \sqrt(5)) / 2.
; Let \psi = (1 - \sqrt(5)) / 2.
; It is easy to show that both \phi and \psi solve the equation x^2 = x + 1.
; From this equation, it follows x^(-1) + x^(-2) = 1.

; Let us show that Fib(n) = (\phi^n - \psi^n) / \sqrt(5), using induction.
; For n = 0, Fib(n) = 0, and (\phi^0 - \psi^0) = 1 - 1 = 0.
; For n = 1, Fib(n) = 1, and (\phi^1 - \psi^1) / \sqrt(5) = (1 + \sqrt(5) - 1 + \sqrt(5)) / (2\sqrt(5)) = 1.
; If it holds for n-1 and n-2, then:
;     Fib(n) = Fib(n-1) + Fib(n-2) = (\phi^(n-1) - \psi^(n-1) + \phi^(n-2) - \psi^(n-2)) / \sqrt(5) =
;            = (\phi^n (\phi^(-1) + \phi^(-2)) - \psi^n (\psi^(-1) + \psi^(-2))) / \sqrt(5) = using the equation above
;            = (\phi^n - \psi^n) / \sqrt(5).

; Therefore, \phi^n / \sqrt(5) = Fib(n) + \psi^n / \sqrt(5).
; It can be shown that -1/2 < \psi^n / \sqrt(5) < 1/2.
; Adding Fib(n), we get
;     Fib(n) - 1/2 < Fib(n) + \psi^n / \sqrt(5) < Fib(n) + 1/2, therefore
;     Fib(n) - 1/2 < \phi^n / \sqrt(5) < Fib(n) + 1/2, meaning that Fib(n) is the closest integer to \phi^n / \sqrt(5), q.e.d.


; Section 1.2.3 exercises

; ---- Ex 1.14
; (count-change n m)
; S(n, m) = \Theta(n)
; T(n, m) = \Theta(n^m)
; The solution for time is obtained observing that T(n, 1) = \Theta(n), since the steps are simply reducing the n by one.
; When m=2, the count with 1 coin needs to be repeated an amount of times proportional to n, thus leading to \Theta(n^2).
; This can be repeated until obtaining \Theta(n^m).

; ---- Ex 1.15
(define (sine angle)
  (define (cube x)
    (* x x x))
  (define (p x)
    (- (+ 3 x) (+ 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 1.5)))))
; (p (p (p (p (p (sine 0.5))))))
; (p (p (p (p (p 0.5)))))
; (p (p (p (p x))))
; (p (p (p y)))
; (p (p z))
; (p k)
; -0.89117

; When a = 12.15, p is applied 5 times.

; We need to apply p m times, where m is large enough to have the angle reduced to less than 0.1.
; At every step, we divide the angle by 3, so
; a / 3^m < 0.1
; a/0.1 < 3^m
; lg_3(a/0.1) < m
; m > lg_3(a/01)
; The number of steps required is \Theta(lg(a)), therefore T(a) = \Theta(lg(a))
; Since at every step we need space for keeping track of a p application, also S(a) = \Theta(lg(a)).s


; Section 1.2.4
; Linear recursive: T = \Theta(n), S = \Theta(n)
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt b (-1+ n)))))

; Linear iterative: T = \Theta(n), S = \Theta(1)
(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
	product
	(iter (-1+ counter) (* b product))))
  (iter n 1))


(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

; Recursive, T = \Thera(lg(n)), S = \Theta(lg(n))
(define (fast-expt-rec b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt-rec b (/ n 2))))
	(else (* b (fast-expt-rec b (-1+ n))))))

; Section 1.2.4 exercises
; ---- Exercise 1.16
(define (fast-expt-iter b n)
  (define (iter b i a)
    (cond ((= i 0) a)
	  ((even? i) (iter (square b) (/ i 2) a))
	  (else (iter b (-1+ i) (* b a)))))
  (iter b n 1))

; ---- Exercise 1.17
(define (double n) (* 2 n))
(define (halve n) (/ n 2))
(define (fast-mult-rec a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mult-rec a (halve b))))
	(else (+ a (fast-mult-rec a (-1+ b))))))

; ---- Exercise 1.18
(define (fast-mult-iter a b)
  (define (iter a b x)
    (cond ((= b 0) x)
	  ((even? b) (iter (double a) (halve b) x))
	  (else (iter a (-1+ b) (+ a x)))))
  (iter a b 0))

; ---- Exercise 1.19
; T_pq can be represented by the matrix (p+q p, q p), since T_pq(a, b) = (qa+pa+qb, qa+pb).
; Furthermore, by matrix multiplication T_pq^2 = (2q^2+p^2+2pq q^2+2pq, q^2+2pq q^2+p^2),
; which is T_p'q' if p'=q^2+2pq and q'=q^2+p^2.
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (iter a
		 b
		 (+ (square p) (square q))
		 (+ (square q) (* 2 p q))
		 (/ count 2)))
	  (else (iter (+ (* b q) (* a q) (* a p))
		      (+ (* b p) (* a q))
		      p
		      q
		      (-1+ count)))))
  (iter 1 0 0 1 n))


; Section 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; ---- Exercise 1.20
; Using applicative order, we calculate the remainder at each step. Since 4 steps are needed, we compute remainder 4 times.
; Using normal order, we delay the calculation of the 4 remainders at the end of the process.
; However, at every step we need to evaluate b to check the condition of the if special form, and in turn b will be an expression containing a certain number of remainders, increasing at every step
; (since we compute it in the condition but not in the consequent when we do the recursive call.
; Tracing the process, we see that in the 4 setps we call remainder 1, 2, 4 and 7 times. Adding the final 4 calls in the last consequent, we see that remainders has been called 18 times.

; Section 1.2.6

; Testing fpr primality, method 1: searching for divisors. T = \Theta(n)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (1+ test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (= n (smallest-divisor n))
       (not (= n 1))))

; Testing for primality, method 2: Fermat test. Probabilistic, T = \Theta(lg(n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (-1+ exp) m))
		    m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n (1+ (random (-1+ n))))
	 (fast-prime? n (-1+ times)))
	(else false)))

; ---- Exercise 1.21
(smallest-divisor 199)   ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999) ; 7

; ---- Exercise 1.22
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
	(report-prime (- (runtime) start-time))
	false))
  
  (define (report-prime elapsed-time)
    (display " **** ")
    (display elapsed-time)
    true)
  
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes a n)
  (if (not (= n 0))
      (cond ((even? a) (search-for-primes (1+ a) n))
	    ((timed-prime-test a) (search-for-primes (+ 2 a) (-1+ n)))
	    (else (search-for-primes (+ 2 a) n)))))
; Since modern computers are faster, I needed some more digits...
(search-for-primes 100000000 3)   ; 0.04 s / sqrt(100000000)   = 4e-6
(search-for-primes 1000000000 3)  ; 0.16 s / sqrt(1000000000)  = 5e-6
(search-for-primes 10000000000 3) ; 0.52 s / sqrt(10000000000) = 5e-6
; The ratio between the time and the sqrt of the steps is roughly constant, as expected.

; ---- Exercise 1.23
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (1+ test-divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(search-for-primes 100000000 3)   ; 0.06 s
(search-for-primes 1000000000 3)  ; 0.21 s
(search-for-primes 10000000000 3) ; 0.80 s
; The time does not decrease, it actually increases a little (considering variations).
; More testing is needed.
; I do expect a speedup, not of a factor 2, because while removing half of the steps, we added a funcion call and an additional if clause.

; ---- Exercise 1.24
(define (timed-fast-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 1000)
	(report-prime (- (runtime) start-time))
	false))
  
  (define (report-prime elapsed-time)
    (display " **** ")
    (display elapsed-time)
    true)
  
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-fast-primes a n)
  (if (not (= n 0))
      (cond ((even? a) (search-for-fast-primes (1+ a) n))
	    ((timed-fast-prime-test a) (search-for-fast-primes (+ 2 a) (-1+ n)))
	    (else (search-for-fast-primes (+ 2 a) n)))))

(search-for-fast-primes 100000000 3)   ; 0.27 s / lg_10(100000000)   = 0.034
(search-for-fast-primes 1000000000 3)  ; 0.32 s / lg_10(1000000000)  = 0.035
(search-for-fast-primes 10000000000 3) ; 0.37 s / lg_10(10000000000) = 0.037
; The ratio between the time and the lg of the steps is roughly constant, as expected.

; ---- Exercise 1.25
(define (bad-expmod base exp m)
  (remainder (fast-expt-iter base exp) m))
; The difference between expmod and bad-expmod is that bad-expmod needs to compute the exponentiation result first.
; This result may be very large, and its calculation involves operation with large numbers.
; In theory, the operation is \Theta(lg(n)), but in the CPU operations with very large numbers may not be natively supported, requiring algorothms that may increase the complexity.
; expmod, on the other hand, aways deals with small numbers, computing the reminder at every step.

; ---- Exercise 1.26
; Without using the square function, in applicative order, the (expmod base (/ exp 2) m) needs to be computed twice at every step (when exp is even).
; This means that, if T(n) is the number of steps required to calculate (expmod base n m), then T(2n) = 2T(n) + k, meaning that T(n) = \Theta(n).
; Conversely, by using square we need to calculate (expmod base (/ exp 2) m) only once, thus T(2n) = T(n) + k, and T(n) = \Theta(lg(n)).

; ---- Exercise 1.27
(define (fermat-full n)
  (define (iter a)
    (cond ((= a n) true)
	  ((fermat-test n a) (iter (1+ a)))
	  (else false)))
  (iter 1))
(fermat-full 19) ; True, as expected since 19 is prime.
(fermat-full 10) ; False, as expeced since 10 is not prime.
; the following are Carmichael numbers: they pass the test (the result is true) but they are not primes.
(fermat-full 561)
(fermat-full 1105)
(fermat-full 1729)
(fermat-full 2465)
(fermat-full 2821)
(fermat-full 6601)

; ---- Exercise 1.28

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
	   (remainder (* base (expmod-miller-rabin base (-1+ exp) m))
		      m))))
  
  (not (= (checked-expmod a (-1+ n) n) 0)))

(define (miller-rabin-prime? n)
  (define (iter step)
    (cond ((> step (1+ (/ n 2))) true)
	  ((miller-rabin-test n step) (iter (+ 1 step)))
	  (else false)))
  (cond	((= n 2) true)
	((even? n) false)
	(else (iter 2))))
