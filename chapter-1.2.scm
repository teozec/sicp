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
