;


(define (reverse! ls)
  (if (null? ls)
      ls
      (reverse!-loop '() ls)))

(define (reverse!-loop prev curr)
  (let ([next (cdr curr)])
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (reverse!-loop curr next))))

'(reverse! (make-list-unknowns 'something))
