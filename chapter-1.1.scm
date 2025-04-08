; # 1.1.4

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; ------------ 1.1.1 - 1.1.6 Exercises

; ---- Ex 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; 3
(define b (+ a 1)) ; 4
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1)) ; 16

; ---- Ex 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7)))

; ---- Ex 1.3
(define (sum-of-square-of-greatest-2 x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> x y) (> z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))
        
; ---- Ex 1.4
; a+b if b > 0, a-b if b < 0 => a + |b|
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; ---- Ex 1.5
(define (p) (p))
(define (text x y)
  (if (= x 0)
      0
      y))
; Applicative order:
; (test 0 (p))
; Will evaluate test, 0 and then enter an infinite loop trying to evaluate (p) before applying test procedure.

; Normal order
; (test 0 (p))
; -> (if (= 0 0) 0 (p))
; -> 0


; # 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; ------------ 1.1.7 Exercises

; ---- Ex 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x)
                     x)))
; Since new-if is not a special form, the interpreter will evaluate all arguments before applying it.
; However, the second argument involves a sqrt-iter-new-form procedure call, which again will need to evaluate all the new-if arguments, entering an infinite loop.

; ---- Ex 1.7
; Our sqrt procedure can fail for small and large numbers.
; For large numbers (for example, 1e29), it can take a huge time to find the solution, since the precision requirement is independent from the original number.
; For small numbers, the procedure is not accurate enough, since the original number may be smaller than 0.001.
; A version that uses the variation between two consecutive guesses is suitable for both small and big numbers.

(define (new-sqrt-iter x guess previous-guess)
  (if (new-good-enough? guess previous-guess)
      guess
      (new-sqrt-iter x (improve guess x) guess)))

(define (new-good-enough? guess previous-guess)
  (< (/ (abs (- guess previous-guess)) guess) 0.000001))

(define (new-sqrt x)
  (new-sqrt-iter x 1.0

					; ---- Ex 1.8
(define (cube-root-iter x guess previous-guess)
  (if (new-good-enough? guess previous-guess)
      guess
      (cube-root-iter x (improve-cube-root guess x) guess)))
(define (improve-cube-root guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))
(define (cube-root x)
  (cube-root-iter x 1.0 0))
