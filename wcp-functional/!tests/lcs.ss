(define (lcs n m ls1 ls2)
  (car (lcs-hat n m ls1 ls2)))

(define (lcs-hat n m ls1 ls2)
  (if (= n 0)
    (cons 0 '())
    (if (= m 0)
      (cons 0 '())
      (lcs-hat-prime n m ls1 ls2
	(lcs-hat (- n 1) m ls1 ls2)))))

(define (lcs-hat-prime n m ls1 ls2 r)
  (if (= n 0)
    (cons 0 '())
    (if (= m 0)
      (cons 0 '())
      (if (= n 1)
	(let ([v2 (lcs-hat-prime n (- m 1) ls1 ls2 (cons 0 '()))]
	      [cn (list-ref ls1 n)]
	      [cm (list-ref ls2 m)])
	  (if (= cn cm)
	    (cons 1 v2)
	    (cons (car v2) v2)))
	(let ([v2 (lcs-hat-prime n (- m 1) ls1 ls2 (cdr r))]
	      [cn (list-ref ls1 n)]
	      [cm (list-ref ls2 m)])
	  (if (= cn cm)
	    (cons (+ (car (cdr r)) 1) v2)
	    (cons (max (car v2) (car r)) v2)))))))

(define (list-ref ls index)
  (if (= index 1)
    (car ls)
    (list-ref (cdr ls) (- index 1))))

(define (max x y)
  (if (> x y) x y))

'(lcs size size (make-list size) (make-list size))