;;

(define (pqueue item priority queue)
  (if (null? queue)
      (cons (cons priority item) '())
      (if (< priority (car (car queue)))
          (cons (cons priority item) queue)
          (cons (car queue) (pqueue item priority (cdr queue))))))

(define (list->queue ls q) ; for testing purposes
  (if (null? ls)
      q
      (list->queue (cdr ls) (pqueue 'item (car ls) q))))

'(list->queue (make-list size) '())
