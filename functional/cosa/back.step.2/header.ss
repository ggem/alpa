(let ([ALL (cons 'magic 'number)])
  (let ([TIME_ADDITION 1]
	[TIME_BINDING 1]
	[TIME_BOOLEANOP 1]
	[TIME_CAR 1]
	[TIME_CDR 1]
	[TIME_COND 1]
	[TIME_CONS 1]
	[TIME_CONSTANT 1]
	[TIME_DIVISION 1]
	[TIME_FUNCALL 1]
	[TIME_MULTIPLICATION 1]
	[TIME_NEGATION 1]
	[TIME_NULL 1]
	[TIME_VARREF 1]
	[make-proc1
	  (lambda (proc)
	    (lambda (arg)
	      (if (eq? arg ALL)
		ALL
		(proc arg))))]
	[make-proc2
	  (lambda (proc)
	    (lambda (arg1 arg2)
	      (cond
		[(eq? arg1 ALL)	ALL]
		[(eq? arg2 ALL)	ALL]
		[else (proc arg1 arg2)])))])
    (define car_1  (make-proc1 car))
    (define cdr_1  (make-proc1 cdr))
    (define eq_1   (make-proc2 eq?))
    (define +_1	   (make-proc2 +))
    (define -_1	   (make-proc2 -))
    (define null_1 (make-proc1 null?))
    (define != (lambda (n1 n2) (not (= n1 n2))))
))