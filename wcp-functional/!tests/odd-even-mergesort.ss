

(define (odd-even-mergesort x)
  (if (null? x)
      '()
      (if (null? (cdr x))
	  (cons (car x) '())
	  (merge (odd-even-mergesort (odd x)) (odd-even-mergesort (even x))))))

(define (odd x)
  (if (null? x)
      '()
      (cons (car x) (even (cdr x)))))

(define (even x)
  (if (null? x)
      '()
      (odd (cdr x))))

(define (merge x y)
  (if (null? x)
      y
      (if (null? y)
	  x
	  (if (< (car x) (car y))
	      (cons (car x) (merge (cdr x) y))
	      (cons (car y) (merge x (cdr y)))))))

'(odd-even-mergesort (make-list size))
