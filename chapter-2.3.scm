(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)
(car '(a b c))
(cdr '(a b c))
(list 'car (list 'quote '(a b c)))

(define (memq item x)
  (cond ((null? x) false)
	((eq? (car x) item) x)
	(else (memq item (cdr x)))))

;; ---- Exercise 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socke)) ; (red shoes blue socks)

;; ---- Exercise 2.54
(define (atom? x)
  (not (pair? x)))

(define (equal? a b)
  (or (and (atom? a) (atom? b) (eq? a b))
      (and (pair? a) (pair? b)
	   (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))))
 
;; ---- Exercise 2.55
(car ''abracadabra)
;; The quote character ' is symbolic sugar for the special form quote. The above expression is equivalent to
(car '(quote abracadabra))
;; and the first element of the list (quote abracadabra) is the symbol quote.
