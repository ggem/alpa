;;;
;;;
;;;

(define union
  (lambda (set1 set2)
    (if (null? set1)
      set2
      (let ([rr (union (cdr set1) set2)])
	(if (member? (car set1) set2)
	  rr
	  (cons (car set1) rr))))))

(define member?
  (lambda (item ls)
    (if (null? ls)
      #f
      (if (eq? item (car ls))
	#t
	(member? item (cdr ls))))))

'(union (make-list size 'unknown) (make-list size 'unknown))
