;;;
;;;  timing.ss
;;;
;;; procedures to get the time cost of scheme operations
;;;

(load "time.ss")

(define-syntax time-loop
  (lambda (x)
    (syntax-case x ()
      [(_ iterations exp)
       (fixnum? (syntax-object->datum (syntax iterations)))
       (syntax
	 (begin
	   (collect)
	   (let ([start (run-time)])
	     (do ([i 0 (fx+ i 1)])
	       ((fx= i iterations)))
	     (let ([middle (run-time)])
	       (do ([i 0 (fx+ i 1)])
		 ((fx= i iterations)) exp)
	       (collect)
	       (let ([end (run-time)])
		 (let ([five (time-minus end middle)]
		       [four (time-minus middle start)])
		   (let ([time (time-divide (time-minus five four)
				 (* 1.0 iterations))])
		     (printf ";~s: ~s milliseconds~n" 'exp time)
		     time)))))))])))

(define-syntax do-tests
  (lambda (x)
    (syntax-case x ()
      [(_ loops (iterations1 exp1) ...)
       (fixnum? (syntax-object->datum (syntax loops)))
       (with-syntax ([size (length (syntax (exp1 ...)))])
	 (with-syntax ([(index1 ...) (iota (syntax size))])
	   (syntax
	     (let ([vec (make-vector size (make-time 0 0))])
	       (do ([i 0 (fx+ i 1)])
		 ((fx= i loops))
		 (vector-set! vec index1
		   (time-add (vector-ref vec index1)
		     (time-loop iterations1 exp1)))
		 ...)
	       (printf "~n;; AVERAGES:~n")
	       (printf "~s:	~s milliseconds.~n" 'exp1
		 (time-divide (vector-ref vec index1) loops))
	       ...))))])))
		  
(define iota
  (letrec ([iota (lambda (n answ)
		   (if (zero? n)
		     answ
		     (iota (- n 1) (cons (- n 1) answ))))])
    (lambda (n)
      (iota n '()))))

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


;;;;
;;;; test cases
;;;;

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

(do-tests 20
  (40000 (union list-0010 iota-0010))
  (80000 (reverse list-0010))
  (50000 (nreverse list-0010))
  (16000 (product list-0010 iota-0010))
  (64000 (insertsort iota-0010))
  (32000 (selectsort iota-0010))
  (40000 (mergesort iota-0010))
  ( 6400 (union list-0020 iota-0020))
  (80000 (reverse list-0020))
  (24000 (nreverse list-0020))
  (50000 (product list-0020 iota-0020))
  ( 8600 (insertsort iota-0020))
  ( 8600 (selectsort iota-0020))
  (15000 (mergesort iota-0020))
  ( 800 (union list-0050 iota-0050))
  (8000 (reverse list-0050))
  (1000 (nreverse list-0050))
  ( 800 (product list-0050 iota-0050))
  ( 800 (insertsort iota-0050))
  ( 400 (selectsort iota-0050))
  (1000 (mergesort iota-0050))
  ( 200 (union list-0100 iota-0100))
  (8000 (reverse list-0100))
  ( 250 (nreverse list-0100))
  (  50 (product list-0100 iota-0100))
  ( 200 (insertsort iota-0100))
  ( 100 (selectsort iota-0100))
  ( 800 (mergesort iota-0100))
  (  60 (union list-0200 iota-0200))
  (8000 (reverse list-0200))
  ( 120 (nreverse list-0200))
  (  15 (product list-0200 iota-0200))
  (  80 (insertsort iota-0200))
  (  40 (selectsort iota-0200))
  ( 500 (mergesort iota-0200))
  (  25 (union list-0300 iota-0300))
  (8000 (reverse list-0300))
  (  64 (nreverse list-0300))
  (   4 (product list-0300 iota-0300))
  (  35 (insertsort iota-0300))
  (  16 (selectsort iota-0300))
  ( 250 (mergesort iota-0300))
  ( 100 (union list-0500 iota-0500))
  (5000 (reverse list-0500))
  (  20 (nreverse list-0500))
  (   8 (insertsort iota-0500))
  (   6 (selectsort iota-0500))
  ( 150 (mergesort iota-0500))
  (  25 (union list-1000 iota-1000))
  (2500 (reverse list-1000))
  (   5 (nreverse list-1000))
  (   2 (insertsort iota-1000))
  (   2 (selectsort iota-1000))
  (  64 (mergesort iota-1000))
  (   8 (union list-2000 iota-2000))
  (1000 (reverse list-2000))
  (   2 (nreverse list-2000))
  (   2 (insertsort iota-2000))
  (   2 (selectsort iota-2000))
  (  20 (mergesort iota-2000)))
