;;;
;;;
;;;

(define split
  (lambda (ls pred k)
    (if (null? ls)
      (k '() '())
      (split (cdr ls) pred
	(lambda (passed failed)
	  (if (pred (car ls))
	    (k (cons (car ls) passed) failed)
	    (k passed (cons (car ls) failed))))))))

(define interval-tester
  (lambda (x)
    (if (> 10 x)
      #f
      (if (> x 20)
	#f
	#t))))

(define initial-k
  (lambda (good bad)
    (cons good (cons bad '()))))

'(split (make-list size 'unknown) interval-tester initial-k)
