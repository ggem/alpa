(define quicksort
  (lambda (ls)
    (qsort ls '())))

(define qsort
  (lambda (ls rest)
    (if (null? ls) rest (let ((pivot (car ls)))
			  (let ((ls (cdr ls)))
			    (qsort
			      (less pivot ls)
			      (cons pivot
				(qsort (more pivot ls) rest))))))))

(define less
  (lambda (pivot ls)
    (if (null? ls) '() (let ((rr (less pivot (cdr ls))))
			 (let ((item (car ls)))
			   (if (< pivot item) rr (cons item rr)))))))

(define more
  (lambda (pivot ls)
    (if (null? ls) '() (let ((rr (more pivot (cdr ls))))
			 (let ((item (car ls)))
			   (if (< pivot item) (cons item rr) rr))))))
