;;
;;

(define ordered?
  (lambda (ls)
    (if (null? ls)
      #t
      (if (null? (cdr ls))
	#t
	(ordered?-hlp (car (cdr ls)) (cdr (cdr ls))
	  (if (> (car ls) (car (cdr ls))) > <))))))

(define ordered?-hlp
  (lambda (item ls pred)
    (if (null? ls)
      #t
      (if (pred item (car ls))
	(ordered?-hlp (car ls) (cdr ls) pred)
	#f))))

'(ordered? (make-list size))
