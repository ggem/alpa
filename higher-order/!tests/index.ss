;;
;;

(define index
  (lambda (item ls)
    (letrec ([index-cps
	       (lambda (ls k)
		 (if (null? ls)
		   -1
		   (if (eq? item (car ls))
		     (k 0)
		     (index-cps (cdr ls)
		       (lambda (v) (k (+ 1 v)))))))])
      (index-cps ls (lambda (x) x)))))



;;;(define index
;;;  (lambda (item ls)
;;;    (index-cps item ls (lambda (x) x))))

;;;(define index-cps
;;;  (lambda (item ls k)
;;;    (if (null? ls)
;;;      -1
;;;      (if (eq? item (car ls))
;;;	(k 0)
;;;	(index-cps item (cdr ls)
;;;	  (lambda (v) (k (+ 1 v))))))))

'(index 'unknown (make-list size 'unknown))
