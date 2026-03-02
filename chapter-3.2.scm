;; Section 3.2

;; Exercise 3.9
;; Recursive version
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (-1+ n)))))

;; Global env
;;      |
;;      V
;; +------------------------------------------+
;; | factorial                                |
;; |     *                                    |
;; |     |                                    |
;; +-----|------------------------------------+
;;       |       ^
;;       |       |
;;       +-> @=@-+
;;           |
;;           V
;;       param: n
;;       body: (if (...))

(factorial 6)


;; Global env
;;      |
;;      V
;; +-------------------------------------------------------------------+
;; | factorial                                                         |
;; |     *                                                             |
;; |     |                                                             |
;; +-----|-------------------------------------------------------------+
;;       |       ^             ^      ^      ^      ^      ^      ^
;;       |       |             |      |      |      |      |      |
;;       +-> @=@-+           +----+ +----+ +----+ +----+ +----+ +----+
;;           |               |n=6 | |n=5 | |n=4 | |n=3 | |n=2 | |n=1 |
;;           V               +----+ +----+ +----+ +----+ +----+ +----+
;;       param: n              ^      ^      ^      ^      ^      ^
;;       body: (if (...))      |      |      |      |      |      |
;;                             E1     E2     E3     E4     E5     E6
;;
;; (factorial 6) is evaluated in E1. (factorial 5) is evaluated in E2.
;; (factorial 4) is evaluated in E3. (factorial 3) is evaluated in E4.
;; (factorial 2) is evaluated in E5. (factorial 1) is evaluated in E6.
;; E1-E6 have the global environment as their enclosing environment because the lambda expression
;; that defined factorial was evaluated in the global environment (through the define syntactic sugar).

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

;; Global env
;;      |
;;      V
;; +------------------------------------------+
;; | factorial        fact-iter               |
;; |     *                *                   |
;; |     |                |                   |
;; +-----|----------------|-------------------+
;;       |       ^        |   ^
;;       |       |        |   |
;;       +-> @=@-+        |   |
;;           |            +-+ |
;;           V              | |
;;       param: n           | |
;;  body: (fact-iter 1 1 n) | |
;;                          | |
;;       +------------------+ |
;;       |                    |
;;       +-> @=@--------------+
;;           |
;;           V
;;  params: product, counter, max-count
;;      body: (if (...))

(factorial 6)

;; Global env
;;      |
;;      V
;; +------------------------------------------+
;; | factorial        fact-iter               |
;; |     *                *                   |
;; |     |                |                   |
;; +-----|----------------|-------------------+
;;       |       ^        |   ^
;;       |       |        |   |
;;       +-> @=@-+        |   |    +----+ +------------+ +------------+ +------------+ +------------+ +------------+ +------------+ +------------+
;;           |            +-+ |    |n=6 | |product=1   | |product=1   | |product=2   | |product=6   | |product=24  | |product=120 | |product=720 | 
;;           V              | |    +----+ |counter=1   | |counter=2   | |counter=3   | |counter=4   | |counter=5   | |counter=6   | |counter=7   |
;;       param: n           | |      ^    |max-count=6 | |max-count=6 | |max-count=6 | |max-count=6 | |max-count=6 | |max-count=6 | |max-count=6 |
;;  body: (fact-iter 1 1 n) | |      |    +------------+ +------------+ +------------+ +------------+ +------------+ +------------+ +------------+
;;                          | |      |          ^              ^              ^              ^              ^              ^              ^
;;       +------------------+ |      |          |              |              |              |              |              |              |
;;       |                    |      E1         E2             E3             E4             E5             E6             E7             E8
;;       +-> @=@--------------+
;;           |
;;           V
;;  params: product, counter, max-count
;;      body: (if (...))

;; (factorial 6) is evaluated in E1.
;; (fact-iter 1 1 6) is evaluated in E2. (fact-iter 1 2 6) is evaluated in E3.
;; (fact-iter 2 3 6) is evaluated in E4. (fact-iter 6 4 6) is evaluated in E5.
;; (fact-iter 24 5 6) is evaluated in E6. (fact-iter 120 6 6) is evaluated in E7.
;; (fact-iter 720 7 6) is evaluated in E8.
;; E1 have the global environment as its enclosing environment because the lambda expression
;; that defined factorial was evaluated in the global environment (through the define syntactic sugar).
;; E2-E8 have the global environment as their enclosing environment because the lambda expression
;; that defined fact-iter was evaluated in the global environment (through the define syntactic sugar).

;; Exercise 3.10
;; The drawing is quite complex and for the moment I have it on paper.
;; The difference in the structure is as follows:
;; in the "parameter" version, the procedure returned by (make-withdraw x) is associated with an environment made of a frame containing balance=x, enclosed in the global environment;
;; in the "parameter" version, the procedure returned by (make-withdraw x) is associated with an environment made of a frame containing balance=x, pointing to a frame with
;;   inital-amount=x, enclosed in the global environment. The procedure then modifies the balance binding, never accessing the initial-amount binding again.

;; Exercise 3.11
;; The drawing is quite complex and for the moment I have it on paper.
;; Evaluating (make-account x) creates an environment where balance=x, in which then are defined the procedures withdraw, deposit and dispatch, associated with the same environment.
;; The result of the expression is the particular dispatch procedure associated with this environment.
;; The local state is kept in this environment, in particular in the balance variable and the three procedures.
;; Evaluating a second (make-account y) creates a new environment, with its own balance and procedures. The only thing possibly shared is the body of the three procedures.
