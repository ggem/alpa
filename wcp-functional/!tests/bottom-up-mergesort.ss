(define (bottom-up-mergesort ls)
  (if (null? ls)
    ls
    (if (null? (cdr ls))
      ls
      (new-merge (split ls)))))

(define (split ls)
  (if (null? ls)
    '()
    (cons (cons (car ls) '()) (split (cdr ls)))))

(define (new-merge ls-sublists)
  (if (null? (cdr ls-sublists))
    (car ls-sublists)
    (new-merge (merge-consec ls-sublists))))

(define (merge-consec ls-sublists)
  (if (null? ls-sublists)
    '()
    (if (null? (cdr ls-sublists))
      ls-sublists
      (cons (merge-lists (car ls-sublists)
	      (car (cdr ls-sublists)))
	(merge-consec (cdr (cdr ls-sublists)))))))

(define (merge-lists ls1 ls2)
  (if (null? ls1)
    ls2
    (if (null? ls2)
      ls1
      (if (< (car ls1) (car ls2))
	(cons (car ls1) (merge-lists (cdr ls1) ls2))
	(cons (car ls2) (merge-lists ls1 (cdr ls2)))))))

'(bottom-up-mergesort (make-list size))
