;

(define (selectsort ls)
  (if (null? ls)
      '()
      (let ([k (least (car ls) (cdr ls))])
	(cons k (selectsort (rest ls k))))))

(define (least s ls)
  (if (null? ls)
      s
      (if (< (car ls) s)
	  (least (car ls) (cdr ls))
	  (least s (cdr ls)))))

(define (rest ls k)
  (if (null? (cdr ls))
      '()
      (if (= k (car ls))
	  (cdr ls)
	  (cons (car ls) (rest (cdr ls) k)))))

'(selectsort (make-list size))
