;;;;
;;;; insertion sort
;;;;

(define (insertsort lst)
  (if (null? lst)
    '()
    (insert (car lst) (insertsort (cdr lst)))))
           
(define (insert a lst)
  (if (null? lst)
    (cons a '())
    (if (< a (car lst))
      (cons a lst)
      (cons (car lst) (insert a (cdr lst))))))

'(insertsort (make-list size))