;;;
;;;  timing.ss
;;;
;;; procedures to get the time cost of scheme programs
;;;

(load "/u/ggomezes/alpa/functional/scheme/time.ss")

(define time-loop
  (let ([min-time 20000])
    (lambda (exp)
      (let ([timer
	      (lambda (old-timed loops)
		(car
		  (port->list
		    (run-scheme
		      `(begin
			 ,start-up-code
			 (collect)
			 (display (time-exp ,loops (lambda () ,exp))))))))])
	(do ([loops 1 (if (zero? (time->runtime timed))
			(* 10 loops)
			(max (+ 1 loops)
			  (ceiling (* (min 10
					(/ min-time (time->runtime timed)))
				     loops))))]
	     [timed (timer (make-time 0 0) 1) (timer timed loops)]
	     [old-loops 1 loops])
	  [(> (time->runtime timed) min-time)
	   (time-divide timed old-loops)])))))

(define do-tests
  (lambda (loops exps)
    (let ([size (length exps)])
      (let ([vec (make-vector size (make-time 0 0))])
	(do ([i 0 (fx+ i 1)])
	  ((fx= i loops))
	  (do ([exps exps (cdr exps)]
	       [index 0 (+ index 1)])
	    [(null? exps)]
	    (let ([time (time-loop (car exps))])
	      (printf ";  ~s: ~s~%" (car exps) time)
	      (vector-set! vec index
		(time-add time (vector-ref vec index))))))
	(printf "~%; AVERAGES:~%")
	(do ([exps exps (cdr exps)]
	     [index 0 (+ index 1)])
	  [(null? exps)]
	  (printf "~s:	~s milliseconds.~%" (car exps)
	    (time-divide (vector-ref vec index) loops)))))))
 
(define start-up-code
  '(begin
     (load "/u/ggomezes/alpa/functional/scheme/time.ss")
     (define iota
       (letrec ([iota (lambda (n answ)
			(if (zero? n)
			  answ
			  (iota (- n 1) (cons (- n 1) answ))))])
	 (lambda (n)
	   (iota n '()))))
     (define time-exp
       (lambda (loops thunk)
	 (let ([start (run-time)])
	   (do ([i loops (- i 1)])
	     [(= i 0) 0]
	     (thunk))
	   (let ([end (run-time)])
	     (time-minus end start)))))
     (define list-0010 (make-list   10  -1))
     (define list-0020 (make-list   20  -1))
     (define list-0050 (make-list   50  -1))
     (define list-0100 (make-list  100  -1))
     (define list-0200 (make-list  200  -1))
     (define list-0300 (make-list  300  -1))
     (define list-0500 (make-list  500  -1))
     (define list-1000 (make-list 1000  -1))
     (define list-2000 (make-list 2000  -1))
     (define iota-0010 (reverse (iota   10)))
     (define iota-0020 (reverse (iota   20)))
     (define iota-0050 (reverse (iota   50)))
     (define iota-0100 (reverse (iota  100)))
     (define iota-0200 (reverse (iota  200)))
     (define iota-0300 (reverse (iota  300)))
     (define iota-0500 (reverse (iota  500)))
     (define iota-1000 (reverse (iota 1000)))
     (define iota-2000 (reverse (iota 2000)))

;;;;
;;;; union
;;;;

     (define union
       (lambda (l1 l2)
	 (if (null? l1)
	   l2
	   (let ([rr (union (cdr l1) l2)])
	     (if (member? (car l1) l2)
	       rr
	       (cons (car l1) rr))))))

     (define member?
       (lambda (a l)
	 (if (null? l)
	   #f
	   (if (eq? (car l) a)
	     #t
	     (member? a (cdr l))))))


;;;;
;;;; insertion sort
;;;;

     (define insertsort
       (lambda (lst)
	 (if (null? lst)
	   '()
	   (insert (car lst) (insertsort (cdr lst))))))
           
     (define insert
       (lambda (a lst)
	 (if (null? lst)
	   (cons a '())
	   (if (< a (car lst))
	     (cons a lst)
	     (cons (car lst) (insert a (cdr lst)))))))

;;;;
;;;; reverse
;;;;

     (define reverse
       (lambda (ls)
	 (reverse2 ls '())))
           
     (define reverse2
       (lambda (ls answ)
	 (if (null? ls)
	   answ
	   (reverse2 (cdr ls) (cons (car ls) answ)))))


;;;;
;;;; nontail-reverse
;;;;

     (define nreverse
       (lambda (ls)
	 (if (null? ls)
	   '()
	   (append (nreverse (cdr ls)) (cons (car ls) '())))))

     (define append
       (lambda (l1 l2)
	 (if (null? l1)
	   l2
	   (cons (car l1) (append (cdr l1) l2)))))


;;;;
;;;; product
;;;;

     (define product
       (lambda (set1 set2)
	 (if (null? set1)
	   '()
	   (append (pair (car set1) set2)
	     (product (cdr set1) set2)))))

     (define pair
       (lambda (item set)
	 (if (null? set)
	   '()
	   (cons (cons item (cons (car set) '()))
	     (pair item (cdr set))))))

     (define append
       (lambda (l1 l2)
	 (if (null? l1)
	   l2
	   (cons (car l1) (append (cdr l1) l2)))))


;;;;
;;;; select sort
;;;;

     (define selectsort
       (lambda (x)
	 (if (null? x)
	   '()
	   (let ([k (least x)])
	     (cons k (selectsort (rest x k)))))))
           
     (define least
       (lambda (x)
	 (if (null? (cdr x))
	   (car x)
	   (let ([s (least (cdr x))])
	     (if (< (car x) s)
	       (car x)
	       s)))))

     (define rest
       (lambda (x k)
	 (if (null? (cdr x))
	   '()
	   (if (= k (car x))
	     (cdr x)
	     (cons (car x) (rest (cdr x) k))))))

;;;;
;;;; merge sort
;;;;

     (define mergesort
       (lambda (x)
	 (if (null? x)
	   '()
	   (if (null? (cdr x))
	     (cons (car x) '())
	     (merge (mergesort (odd x)) (mergesort (even x)))))))
           
     (define odd
       (lambda (x)
	 (if (null? x)
	   '()
	   (cons (car x) (even (cdr x))))))
               
     (define even
       (lambda (x)
	 (if (null? x)
	   '()
	   (odd (cdr x)))))
                   
     (define merge
       (lambda (x y)
	 (if (null? x) 
	   y 
	   (if (null? y) 
	     x 
	     (if (< (car x) (car y))
	       (cons (car x) (merge (cdr x) y))
	       (cons (car y) (merge x (cdr y))))))))

;;;;
;;;; quicksort
;;;;

     (define quicksort
       (lambda (ls)
	 (qsort ls '())))

     (define qsort
       (lambda (ls rest)
	 (if (null? ls)
	   rest
	   (let ([pivot (car ls)])
	     (let ([ls (cdr ls)])
	       (qsort (less pivot ls)
		 (cons pivot (qsort (more pivot ls) rest))))))))

     (define less
       (lambda (pivot ls)
	 (if (null? ls)
	   '()
	   (let ([rr (less pivot (cdr ls))])
	     (let ([item (car ls)])
	       (if (< pivot item)
		 rr
		 (cons item rr)))))))

     (define more
       (lambda (pivot ls)
	 (if (null? ls)
	   '()
	   (let ([rr (more pivot (cdr ls))])
	     (let ([item (car ls)])
	       (if (< pivot item)
		 (cons item rr)
		 rr))))))
     ))

;;;;
;;;; test cases
;;;;


(define timer-procs
  (lambda ()
    (do-tests 20
      '((insertsort iota-0010)
	(mergesort iota-0010)
	(nreverse list-0010)
	(product list-0010 iota-0010)
	(reverse list-0010)
	(selectsort iota-0010)
	(union list-0010 iota-0010)
	(insertsort iota-0020)
	(mergesort iota-0020)
	(nreverse list-0020)
	(product list-0020 iota-0020)
	(reverse list-0020)
	(selectsort iota-0020)
	(union list-0020 iota-0020)
	(insertsort iota-0050)
	(mergesort iota-0050)
	(nreverse list-0050)
	(product list-0050 iota-0050)
	(reverse list-0050)
	(selectsort iota-0050)
	(union list-0050 iota-0050)
	(insertsort iota-0100)
	(mergesort iota-0100)
	(nreverse list-0100)
	(reverse list-0100)
	(selectsort iota-0100)
	(union list-0100 iota-0100)
	(insertsort iota-0200)
	(mergesort iota-0200)
	(nreverse list-0200)
	(reverse list-0200)
	(selectsort iota-0200)
	(union list-0200 iota-0200)
	(insertsort iota-0300)
	(mergesort iota-0300)
	(nreverse list-0300)
	(reverse list-0300)
	(selectsort iota-0300)
	(union list-0300 iota-0300)
	(insertsort iota-0500)
	(mergesort iota-0500)
	(nreverse list-0500)
	(reverse list-0500)
	(selectsort iota-0500)
	(union list-0500 iota-0500)
	(insertsort iota-1000)
	(mergesort iota-1000)
	(nreverse list-1000)
	(reverse list-1000)
	(selectsort iota-1000)
	(union list-1000 iota-1000)
	(insertsort iota-2000)
	(mergesort iota-2000)
	(nreverse list-2000)
	(reverse list-2000)
	(selectsort iota-2000)
	(union list-2000 iota-2000)))))

(timer-procs)
(exit)
