;

(define ack
  (letrec ([ack (lambda (m)
		  (if (= m 0)
		    (lambda (n) (+ n 1))
		    (let ([ack_m-1 (ack (- m 1))])
		      (letrec ([ack_m
				 (lambda (n)
				   (if (= n 0)
				     (ack_m-1 1)
				     (ack_m-1 (ack_m (- n 1)))))])
			ack_m))))])
    (lambda (m n)
      ((ack m) n))))

'(ack size size)
