;;
;;

(define (index item ls)
  (index-hlp item ls 0))

(define (index-hlp item ls count)
  (if (null? ls)
    -1
    (if (eq? item (car ls))
      count
      (index-hlp item (cdr ls) (+ count 1)))))

'(index unknown (make-list size 'unknown))
