;;;
;;;
;;;

(define map
  (lambda (f ls)
    (if (null? ls)
      '()
      (cons (f (car ls)) (map f (cdr ls))))))


(define c-add
  (lambda (n)
    (lambda (m)
      (+ n m))))

'(map (c-add 4) (make-list size))
