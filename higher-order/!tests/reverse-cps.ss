;;;
;;;
;;;

(define reverse-cps
  (lambda (ls)
    (reverse-cps-hlp ls (lambda (x) x))))

(define reverse-cps-hlp
  (lambda (ls k)
    (if (null? ls)
      (k '())
      (reverse-cps-hlp (cdr ls)
	(lambda (v)
	  (append-cps v (cons (car ls) '()) k))))))

(define append-cps
  (lambda (ls1 ls2 k)
    (if (null? ls1)
      (k ls2)
      (append-cps (cdr ls1) ls2
	(lambda (v)
	  (k (cons (car ls1) v)))))))

'(reverse-cps (make-list size 'unknown))
