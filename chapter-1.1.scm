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