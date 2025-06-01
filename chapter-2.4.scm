(load "chapter-2.3.scm")

;; Section 2.4.1
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;; Section 2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a))
		    (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else (error "Unknown type -- REAL-PART" z))))
	      
(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; Section 2.4.3
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)
	  (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
	  (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag-polar x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Copied from Section 3.3.3
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; ---- Exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp)
	       var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a.
;; We distinguish three cases:
;; - if the expression is a number, the derivative is 0.
;; - if the expression is a variable, the derivative is either 0 or 1.
;; - otherwise, the expression is an operation, and we retrieve the derivative rule from the table.
;; We cannot assimilate the predicates in the dispatch, because the dispatch is done on the operator, therefore the expression needs to be an operation.

;; b.
(define (addend operands) (car operands))
(define (augend operands) (cadr operands))
(define (multiplier operands) (car operands))
(define (multiplicand operands) (cadr operands))

(define (make-sum a b) (list '+ a b))
(define (make-product a b) (list '* a b))
(define (make-exponentiation a b) (list '** a b))

(define (install-sum-deriv)
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  (put 'deriv '+ sum-deriv)
  'done)

(define (install-product-deriv)
  (define (product-deriv operands var)
    (make-sum (make-product (deriv (multiplicand operands) var)
			    (multiplier operands))
	      (make-product (multiplicand operands)
			    (deriv (multiplier operands) var))))
  (put 'deriv '* product-deriv)
  'done)
  
;; c.
(define (base operands) (car operands))
(define (exponent operands) (cadr operands))

(define (install-exponentiation-deriv)
  (define (exponentiation-deriv operands var)
    (make-product
     (exponent operands)
     (make-product
      (make-exponentiation (base operands)
			   (-1+ (exponent operands)))
      (deriv (base operands) var))))
  (put 'deriv '** exponentiation-deriv)
  'done)

;; d.
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get (operator exp) 'deriv)
	       (operands exp)
	       var))))

;; The type is now 'deriv, and the operations are '+, '* and '**.
(define (install-deriv)
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  
  (define (product-deriv operands var)
    (make-sum (make-product (deriv (multiplicand operands) var)
			    (multiplier operands))
	      (make-product (multiplicand operands)
			    (deriv (multiplier operands) var))))
  
  (define (exponentiation-deriv operands var)
    (make-product
     (exponent operands)
     (make-product
      (make-exponentiation (base operands)
			   (-1+ (exponent operands)))
      (deriv (base operands) var))))
  
  (put '+ 'deriv sum-deriv)
  (put '* 'deriv product-deriv)
  (put '** 'deriv exponentiation-deriv)

  'done)


;; ---- Exercise 2.74
;; a.
(define (get-record name file)
  ((apply-generic 'get-record file) name))

;; The file needs to be a tagged datum with the records as content.
;; The package needs to install the get-record procedure, which when applied to a file returns a procedure
;; that searchs the employee by name in the file and returns false if it is not found.

;; A simple package for files that contain a single employee.
;; The file is the record itself, structured as (name salary address).
(define (install-one-employee-package)
  (define (get-record name file)
    (if (eq? (employee-name file) name)
	file
	false))

  (define (employee-name record) (car record))
  (define (employee-salary record) (cadr record))
  (define (employee-address record) (caddr record))

  (define (tag x) (attach-tag 'one-employee x))
  (put 'get-record '(one-employee)
       (lambda (file)
	 (lambda (name)
	   (tag (get-record name file)))))
  (put 'get-salary '(one-employee) employee-salary)
  'done)


;; b.
(define (get-salary record)
  (apply-generic 'get-salary record))
					;
;; Each record needs to be tagged with the corresponding type.
;; The package needs to install a get-salary procedure which returns the salary contained in the record.

;; c.
(define (find-employee-record name files)
  (filter (lambda (record) (contents record))
	  (map (lambda (file) (get-record name file))
	       files)))

  
;; No further information from the package is needed.

;; d.
;; When a new file type is added, no modification to the central system is needed, except running the install procedure provided by the package.
;; The package itself, on the other hand, needs to write the install procedure that wraps its internal procedures.


;; ----
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude)
	   (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle)
	   (atan y x))
	  (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;; ---- Exercise 2.75
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
	   (* mag (cos ang)))
	  ((eq? op 'imag-part)
	   (* mag (sin ang)))
	  ((eq? op 'magnitude) mag)
	  ((eq? op 'angle) ang)
	  (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; ---- Exercise 2.76
;; With explicit dispatch, when a new type is added the general centralized dispatch operations need to be modified, to dispatch on it when appropriate.
;; When a new operation is added, it needs to be implemented for each type and a new centralized dispatcher procedure needs to be added.

;; With data-directed style, when a new type is added, a new package needs to be written. No modifications to the centralized dispatchers are needed.
;; When a new operation is added, it needs to be implemented for each type and a new dispatcher procedure needs to be added.

;; With message-passing style, when a new type is added no modifications to the centralizad dispatcher are needed.
;; When a new operation is added, it needs to be implemented for each type and a new dispatcher procedure needs to be added.

;; For systems in which types are added often, probably message-passing style is better since it is additive and does not require mutable global state.
;; When new operations are added often, both data-directed and message-passing style can be appropriate.
