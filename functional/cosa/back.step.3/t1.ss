(optimize-level 3)

(define-syntax make-proc
  (lambda (x)
    (syntax-case x ()
      [(_ new-proc old-proc)
       (syntax
	 (define-syntax new-proc
	   (lambda (x)
	     (syntax-case x ()
	       [(_ arg)
		(syntax
		  (if (eq? arg 'ALL)
		    'ALL
		    (old-proc arg)))]
	       [(_ arg1 arg2)
		(syntax
		  (cond
		    [(eq? arg1 'ALL) 'ALL]
		    [(eq? arg2 'ALL) 'ALL]
		    [else (old-proc arg1 arg2)]))]))))])))

(make-proc car_1 car)
(make-proc cdr_1 cdr)
(make-proc eq?_1 eq?)
(make-proc +_1 +)
(make-proc -_1 -)
(make-proc null?_1 null?)

(define timebound
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
        [TIME_VARREF 1])
    (define lub_1
      (lambda (x y)
	(cond
	  [(equal? x y)  x]
	  [(and (pair? x) (pair? y))
	   (cons (lub_1 (car x) (car y))
	     (lub_1 (cdr x) (cdr y)))]
	  [else 'ALL])))
    (define length_1
      (lambda (n)
	(if (= n 0)
	  '()
	  (cons 'ALL (length_1 (- n 1))))))


    (define time_union
      (lambda (l1 l2)
	(+ TIME_COND
	  (+ (+ TIME_NULL TIME_VARREF)
	    (let ((x:1 (null?_1 l1)))
	      (if (eq? x:1 'ALL)
		(max
		  TIME_CONSTANT
		  (+ TIME_BINDING
		    (+ (+ TIME_FUNCALL
			 (+ (time_union
			      (cdr l1)
			      l2)
			   (+ (+ TIME_CDR
				TIME_VARREF)
			     TIME_VARREF)))
		      (+ TIME_COND
			(+ (+ TIME_FUNCALL
			     (+ (time_member?
				  (car l1)
				  l2)
			       (+ (+ TIME_CAR
				    TIME_VARREF)
				 TIME_VARREF)))
			  (let ((x:1
				  (member?
				    (car_1
				      l1)
				    l2)))
			    (if (eq? x:1
				  'ALL)
			      (max
				TIME_VARREF
				(+ TIME_CONS
				  (+ (+ TIME_CAR
				       TIME_VARREF)
				    TIME_VARREF)))
			      (if x:1
				TIME_VARREF
				(+ TIME_CONS
				  (+ (+ TIME_CAR
				       TIME_VARREF)
				    TIME_VARREF))))))))))
		(if x:1
		  TIME_CONSTANT
		  (+ TIME_BINDING
		    (+ (+ TIME_FUNCALL
			 (+ (time_union
			      (cdr l1)
			      l2)
			   (+ (+ TIME_CDR
				TIME_VARREF)
			     TIME_VARREF)))
		      (+ TIME_COND
			(+ (+ TIME_FUNCALL
			     (+ (time_member?
				  (car l1)
				  l2)
			       (+ (+ TIME_CAR
				    TIME_VARREF)
				 TIME_VARREF)))
			  (let ((x:1
				  (member?
				    (car_1
				      l1)
				    l2)))
			    (if (eq? x:1
				  'ALL)
			      (max
				TIME_VARREF
				(+ TIME_CONS
				  (+ (+ TIME_CAR
				       TIME_VARREF)
				    TIME_VARREF)))
			      (if x:1
				TIME_VARREF
				(+ TIME_CONS
				  (+ (+ TIME_CAR
				       TIME_VARREF)
				    TIME_VARREF))))))))))))))))

    (define union
      (lambda (l1 l2)
	(let ((x:1 (null?_1 l1)))
	  (if (eq? x:1 'ALL)
	    (lub_1 '()
	      (let ((rr (union (cdr_1 l1) l2)))
		(let ((x:1
			(member? (car_1 l1) l2)))
		  (if (eq? x:1 'ALL)
		    (lub_1
		      rr
		      (cons (car_1 l1) rr))
		    (if x:1
		      rr
		      (cons (car_1 l1)
			rr))))))
	    (if x:1 '() (let ((rr (union (cdr_1 l1) l2)))
			  (let ((x:1 (member? (car_1 l1) l2)))
			    (if (eq? x:1 'ALL)
			      (lub_1 rr (cons (car_1 l1) rr))
			      (if x:1 rr (cons (car_1 l1) rr))))))))))

    (define time_member?
      (lambda (a l)
	(+ TIME_COND
	  (+
	    (+ TIME_NULL TIME_VARREF)
	    (let ((x:1 (null?_1 l)))
	      (if (eq? x:1 'ALL)
		(max
		  TIME_CONSTANT
		  (+ TIME_COND
		    (+ (+ TIME_BOOLEANOP
			 (+ (+ TIME_CAR
			      TIME_VARREF)
			   TIME_VARREF))
		      (let ((x:1
			      (eq?_1
				(car_1
				  l)
				a)))
			(if (eq? x:1
			      'ALL)
			  (max
			    TIME_CONSTANT
			    (+ TIME_FUNCALL
			      (+ (time_member?
				   a
				   (cdr l))
				(+ TIME_VARREF
				  (+ TIME_CDR
				    TIME_VARREF)))))
			  (if x:1
			    TIME_CONSTANT
			    (+ TIME_FUNCALL
			      (+ (time_member?
				   a
				   (cdr l))
				(+ TIME_VARREF
				  (+ TIME_CDR
				    TIME_VARREF))))))))))
		(if x:1
		  TIME_CONSTANT
		  (+ TIME_COND
		    (+ (+ TIME_BOOLEANOP
			 (+ (+ TIME_CAR
			      TIME_VARREF)
			   TIME_VARREF))
		      (let ((x:1
			      (eq?_1
				(car_1
				  l)
				a)))
			(if (eq? x:1
			      'ALL)
			  (max
			    TIME_CONSTANT
			    (+ TIME_FUNCALL
			      (+ (time_member?
				   a
				   (cdr l))
				(+ TIME_VARREF
				  (+ TIME_CDR
				    TIME_VARREF)))))
			  (if x:1
			    TIME_CONSTANT
			    (+ TIME_FUNCALL
			      (+ (time_member?
				   a
				   (cdr l))
				(+ TIME_VARREF
				  (+ TIME_CDR
				    TIME_VARREF))))))))))))))))

    (define member?
      (lambda (a l)
	(let ((x:1 (null?_1 l)))
	  (if (eq? x:1 'ALL) (lub_1 #f (let ((x:1 (eq?_1 (car_1 l) a)))
					 (if (eq? x:1 'ALL)
					   (lub_1
					     #t
					     (member? a (cdr_1 l)))
					   (if x:1
					     #t
					     (member?
					       a
					       (cdr_1 l))))))
	    (if x:1 #f (let ((x:1 (eq?_1 (car_1 l) a)))
			 (if (eq? x:1 'ALL)
			   (lub_1 #t (member? a (cdr_1 l)))
			   (if x:1 #t (member? a (cdr_1 l))))))))))

    (lambda args
      (time_union
	(length_1 (car args))
	(length_1 (car (cdr args)))))))

(optimize-level 0)
