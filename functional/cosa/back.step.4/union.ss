(define union
  (lambda (ls1 ls2)
    (if (null? ls1)
      ls2
      (if (member? (car ls1) ls2)
	(union (cdr ls1) ls2)
	(cons (car ls1) (union (cdr ls1) ls2))))))

(define member?
  (lambda (item ls)
    (if (null? ls)
      #f
      (if (eq? item (car ls))
	#t
	(member? item (cdr ls))))))

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
    (define member?-cps
      (lambda (item ls k)
	(if (null? ls)
	  (k #f (+ (+ (+ TIME_NULL TIME_VARREF) 0) TIME_CONSTANT))
	  (if (eq? item (car ls))
	    (k #t (+ (+ (+ TIME_NULL TIME_VARREF) 0)
		    (+ (+ TIME_BOOLEANOP (+ TIME_VARREF (+ TIME_CAR TIME_VARREF)))
		      TIME_CONSTANT)))
	    (member?-cps item (cdr ls)
	      (lambda (val cost)
		(k val
		  (+ cost (+ (+ (+ TIME_NULL TIME_VARREF) 0)
			    (+ (+ TIME_BOOLEANOP (+ TIME_VARREF (+ TIME_CAR TIME_VARREF)))
			      (+ TIME_FUNCALL (+ TIME_VARREF (+ TIME_CDR TIME_VARREF)))))))))))))


    (define union-cps
      (lambda (ls1 ls2 k)
	(if (null? ls1)
	  (k ls2 (+ 0 (+ TIME_COND (+ TIME_NULL TIME_VARREF))))
	  (member?-cps (car ls1) ls2
	    (lambda (val cost)
	      (if val
		(union-cps (cdr ls1) ls2
		  (lambda (val cost2)
		    (k val (+ cost (+ cost2 (+ (+ TIME_COND (+ TIME_NULL TIME_VARREF)) (+ TIME_COND (+ TIME_FUNCALL (+ TIME_CAR TIME_VARREF) TIME_VARREF))))))))
		(union-cps (cdr ls1) ls2
		  (lambda (val cost2)
		    (k (cons (car ls1) val)
		      (+ cost (+ cost2 (+ (+ TIME_COND (+ TIME_NULL TIME_VARREF)) (+ (+ TIME_COND (+ TIME_FUNCALL (+ TIME_CAR TIME_VARREF) TIME_VARREF)) (+ TIME_CONS (+ TIME_CAR TIME_VARREF)))))))))))))))

    (lambda (n m)
      (union-cps
	(make-list n 0)
	(make-list m 1)
	(lambda (val cost)
	  (list val cost))))))