(define timebound

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
                [(eq? arg1 ALL) ALL]
                [(eq? arg2 ALL) ALL]
                [else (proc arg1 arg2)])))])
    (define car_1 (make-proc1 car))
    (define cdr_1 (make-proc1 cdr))
    (define eq?_1 (make-proc2 eq?))
    (define +_1 (make-proc2 +))
    (define -_1 (make-proc2 -))
    (define null?_1 (make-proc1 null?))
    (define lub_1
      (lambda (x y)
	(cond
	  [(and (eq? x y) (null? x)) x]
	  [(or (eq? x ALL) (eq? y ALL) (atom? x) (atom? y)) ALL]
	  [else (cons (lub_1 (car x) (car y))
		  (lub_1 (cdr x) (cdr y)))])))
    (define length_1
      (lambda (n)
        (if (= n 0)
          '()
          (cons ALL (length_1 (- n 1))))))


    (define time_union
      (lambda ( l1 l2)
        (+ TIME_COND
	  (+ (+ TIME_NULL TIME_VARREF)
	    (if (eq? (null?_1  l1) ALL)
	      (max
		TIME_CONSTANT
		(+ TIME_COND
		  (+ (+ TIME_FUNCALL
		       (+ (time_member?  (car l1) l2)
			 (+ (+ TIME_CAR TIME_VARREF) TIME_VARREF)))
		    (if (eq? (member?  (car_1  l1) l2) ALL)
		      (max
			(+ TIME_FUNCALL
			  (+ (time_union  (cdr l1) l2)
			    (+ (+ TIME_CDR TIME_VARREF)
			      TIME_VARREF)))
			(+ TIME_CONS
			  (+ (+ TIME_CAR TIME_VARREF)
			    (+ TIME_FUNCALL
			      (+ (time_union  (cdr l1) l2)
				(+ (+ TIME_CDR TIME_VARREF)
				  TIME_VARREF))))))
		      (if (member?  (car_1  l1) l2)
			(+ TIME_FUNCALL
			  (+ (time_union  (cdr l1) l2)
			    (+ (+ TIME_CDR TIME_VARREF)
			      TIME_VARREF)))
			(+ TIME_CONS
			  (+ (+ TIME_CAR TIME_VARREF)
			    (+ TIME_FUNCALL
			      (+ (time_union  (cdr l1) l2)
				(+ (+ TIME_CDR TIME_VARREF)
				  TIME_VARREF))))))))))
	      (if (null?_1  l1)
		TIME_CONSTANT
		(+ TIME_COND
		  (+ (+ TIME_FUNCALL
		       (+ (time_member?  (car l1) l2)
			 (+ (+ TIME_CAR TIME_VARREF) TIME_VARREF)))
		    (if (eq? (member?  (car_1  l1) l2) ALL)
		      (max
			(+ TIME_FUNCALL
			  (+ (time_union  (cdr l1) l2)
			    (+ (+ TIME_CDR TIME_VARREF)
			      TIME_VARREF)))
			(+ TIME_CONS
			  (+ (+ TIME_CAR TIME_VARREF)
			    (+ TIME_FUNCALL
			      (+ (time_union  (cdr l1) l2)
				(+ (+ TIME_CDR TIME_VARREF)
				  TIME_VARREF))))))
		      (if (member?  (car_1  l1) l2)
			(+ TIME_FUNCALL
			  (+ (time_union  (cdr l1) l2)
			    (+ (+ TIME_CDR TIME_VARREF)
			      TIME_VARREF)))
			(+ TIME_CONS
			  (+ (+ TIME_CAR TIME_VARREF)
			    (+ TIME_FUNCALL
			      (+ (time_union  (cdr l1) l2)
				(+ (+ TIME_CDR TIME_VARREF)
				  TIME_VARREF)))))))))))))))

    (define union
      (lambda ( l1 l2)
        (if (eq? (null?_1  l1) ALL)
	  (lub_1
	    '()
	    (if (eq? (member?  (car_1  l1) l2) ALL)
	      (lub_1
		(union  (cdr_1  l1) l2)
		(cons (car_1  l1) (union  (cdr_1  l1) l2)))
	      (if (member?  (car_1  l1) l2)
		(union  (cdr_1  l1) l2)
		(cons (car_1  l1) (union  (cdr_1  l1) l2)))))
	  (if (null?_1  l1)
	    '()
	    (if (eq? (member?  (car_1  l1) l2) ALL)
	      (lub_1
		(union  (cdr_1  l1) l2)
		(cons (car_1  l1) (union  (cdr_1  l1) l2)))
	      (if (member?  (car_1  l1) l2)
		(union  (cdr_1  l1) l2)
		(cons (car_1  l1) (union  (cdr_1  l1) l2))))))))

    (define time_member?
      (lambda ( a l)
        (+ TIME_COND
	  (+ (+ TIME_NULL TIME_VARREF)
	    (if (eq? (null?_1  l) ALL)
	      (max
		TIME_CONSTANT
		(+ TIME_COND
		  (+ (+ TIME_BOOLEANOP
		       (+ (+ TIME_CAR TIME_VARREF) TIME_VARREF))
		    (if (eq? (eq?_1  (car_1  l) a) ALL)
		      (max
			TIME_CONSTANT
			(+ TIME_FUNCALL
			  (+ (time_member?  a (cdr l))
			    (+ TIME_VARREF
			      (+ TIME_CDR TIME_VARREF)))))
		      (if (eq?_1  (car_1  l) a)
			TIME_CONSTANT
			(+ TIME_FUNCALL
			  (+ (time_member?  a (cdr l))
			    (+ TIME_VARREF
			      (+ TIME_CDR TIME_VARREF)))))))))
	      (if (null?_1  l)
		TIME_CONSTANT
		(+ TIME_COND
		  (+ (+ TIME_BOOLEANOP
		       (+ (+ TIME_CAR TIME_VARREF) TIME_VARREF))
		    (if (eq? (eq?_1  (car_1  l) a) ALL)
		      (max
			TIME_CONSTANT
			(+ TIME_FUNCALL
			  (+ (time_member?  a (cdr l))
			    (+ TIME_VARREF
			      (+ TIME_CDR TIME_VARREF)))))
		      (if (eq?_1  (car_1  l) a)
			TIME_CONSTANT
			(+ TIME_FUNCALL
			  (+ (time_member?  a (cdr l))
			    (+ TIME_VARREF
			      (+ TIME_CDR
				TIME_VARREF))))))))))))))

    (define member?
      (lambda ( a l)
        (if (eq? (null?_1  l) ALL)
	  (lub_1
	    #f
	    (if (eq? (eq?_1  (car_1  l) a) ALL)
	      (lub_1  #t (member?  a (cdr_1  l)))
	      (if (eq?_1  (car_1  l) a) #t (member?  a (cdr_1  l)))))
	  (if (null?_1  l)
	    #f
	    (if (eq? (eq?_1  (car_1  l) a) ALL)
	      (lub_1  #t (member?  a (cdr_1  l)))
	      (if (eq?_1  (car_1  l) a) #t (member?  a (cdr_1  l))))))))

    (lambda (n m)
      (time_union  (length_1  n) (length_1  m))))))
