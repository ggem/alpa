;;;
;;; counter.  interpreter that counts each of the primitive operations
;;;

(define count
  (lambda (definitions call)
    ((lambda (x) '(pretty-print x) (eval x))
      `(let ([counts (make-vector 10 0)])
	 (let ([inc (lambda (i)
		      (vector-set! counts i
			(+ 1 (vector-ref counts i))))])
	   (let ([eq? (lambda (x y) (inc 1) (eq? x y))]
		 [< (lambda (x y) (inc 1) (< x y))]
		 [car (lambda (x) (inc 2) (car x))]
		 [cdr (lambda (x) (inc 3) (cdr x))]
		 [cons (lambda (x y) (inc 5) (cons x y))]
		 [null? (lambda (x) (inc 8) (null? x))])
	     ,@(map make-counter definitions)
	     ,call
	     (#%cons '+
	       (reverse
		 (map (lambda (n name)
			(list '* n name))
		   (vector->list counts)
		   '(cost_binding cost_booleanop cost_car cost_cdr
		      cost_cond cost_cons cost_constant cost_funcall
		      cost_null cost_varref))))))))))

(define make-counter
  (lambda (exp)
    (cond
      [(symbol? exp) `(begin (inc 9) ,exp)]
      [(null? exp) `(begin (inc 6) ,exp)]
      [(number? exp) `(begin (inc 6) ,exp)]
      [(boolean? exp) `(begin (inc 6) ,exp)]
      [(atom? exp) (error 'count "unknown type: ~s" exp)]
      [(eq? (car exp) 'lambda)
       `(lambda ,(cadr exp) . ,(map make-counter (cddr exp)))]
      [(eq? (car exp) 'define)
       `(define ,(cadr exp) ,(make-counter (caddr exp)))]
      [(eq? (car exp) 'quote) `(begin (inc 6) ,exp)]
      [(eq? (car exp) 'if)
       `(begin (inc 4)
	  (if ,(make-counter (cadr exp))
	    ,(make-counter (caddr exp))
	    ,(make-counter (cadddr exp))))]
      [(eq? (car exp) 'let)
       `(let ((,(caaadr exp) ,(make-counter (car (cdaadr exp)))))
	  (inc 0) . ,(map make-counter (cddr exp)))]
      [(memq (car exp) '(eq? car cdr cons null? <))
       `(,(car exp) . ,(map make-counter (cdr exp)))]
      [else
	`(begin (inc 7)
	   (,(car exp) . ,(map make-counter (cdr exp))))])))


(define iota
  (letrec ([iota (lambda (n answ)
		   (if (zero? n)
		     answ
		     (iota (- n 1) (cons (- n 1) answ))))])
    (lambda (n)
      (iota n '()))))

;;;;
;;;; test cases
;;;;

(define lists
  (vector
    (make-list   10  -1)
    (make-list   20  -1)
    (make-list   50  -1)
    (make-list  100  -1)
    (make-list  200  -1)
    (make-list  300  -1)
    (make-list  500  -1)
    (make-list 1000  -1)
    (make-list 2000  -1)))

(define iotas
  (vector
    (reverse (iota   10))
    (reverse (iota   20))
    (reverse (iota   50))
    (reverse (iota  100))
    (reverse (iota  200))
    (reverse (iota  300))
    (reverse (iota  500))
    (reverse (iota 1000))
    (reverse (iota 2000))))

(define sizes '(10 20 50 100 200 300 500 1000 2000))


(define procedures
  (list

;;;;
;;;; insertion sort
;;;;

    (list 'insertsort '(iotas)
      '(define insertsort
	 (lambda (lst)
	   (if (null? lst)
	     '()
	     (insert (car lst) (insertsort (cdr lst))))))
           
      '(define insert
	 (lambda (a lst)
	   (if (null? lst)
	     (cons a '())
	     (if (< a (car lst))
	       (cons a lst)
	       (cons (car lst) (insert a (cdr lst))))))))

;;;;
;;;; union
;;;;

    (list 'union '(lists iotas)
      '(define union
	 (lambda (l1 l2)
	   (if (null? l1)
	     l2
	     (let ([rr (union (cdr l1) l2)])
	       (if (member? (car l1) l2)
		 rr
		 (cons (car l1) rr))))))

      '(define member?
	 (lambda (a l)
	   (if (null? l)
	     #f
	     (if (eq? (car l) a)
	       #t
	       (member? a (cdr l)))))))

;;;;
;;;; reverse
;;;;

    (list 'reverse '(lists)
      '(define reverse
	 (lambda (ls)
	   (reverse2 ls '())))
           
      '(define reverse2
	 (lambda (ls answ)
	   (if (null? ls)
	     answ
	     (reverse2 (cdr ls) (cons (car ls) answ))))))


;;;;
;;;; nontail-reverse
;;;;

    (list 'nreverse '(lists)
      '(define nreverse
	 (lambda (ls)
	   (if (null? ls)
	     '()
	     (append (nreverse (cdr ls)) (cons (car ls) '())))))

      '(define append
	 (lambda (l1 l2)
	   (if (null? l1)
	     l2
	     (cons (car l1) (append (cdr l1) l2))))))


;;;;
;;;; product
;;;;

    (list 'product '(lists iotas)
      '(define product
	 (lambda (set1 set2)
	   (if (null? set1)
	     '()
	     (append (pair (car set1) set2)
	       (product (cdr set1) set2)))))

      '(define pair
	 (lambda (item set)
	   (if (null? set)
	     '()
	     (cons (cons item (cons (car set) '()))
	       (pair item (cdr set))))))

      '(define append
	 (lambda (l1 l2)
	   (if (null? l1)
	     l2
	     (cons (car l1) (append (cdr l1) l2))))))


;;;;
;;;; select sort
;;;;

    (list 'selectsort '(iotas)
      '(define selectsort
	 (lambda (x)
	   (if (null? x)
	     '()
	     (let ([k (least x)])
	       (cons k (selectsort (rest x k)))))))
           
      '(define least
	 (lambda (x)
	   (if (null? (cdr x))
	     (car x)
	     (let ([s (least (cdr x))])
	       (if (< (car x) s)
		 (car x)
		 s)))))

      '(define rest
	 (lambda (x k)
	   (if (null? (cdr x))
	     '()
	     (if (= k (car x))
	       (cdr x)
	       (cons (car x) (rest (cdr x) k)))))))

;;;;
;;;; merge sort
;;;;

    (list 'mergesort '(iotas)
      '(define mergesort
	 (lambda (x)
	   (if (null? x)
	     '()
	     (if (null? (cdr x))
	       (cons (car x) '())
	       (merge (mergesort (odd x)) (mergesort (even x)))))))
           
      '(define odd
	 (lambda (x)
	   (if (null? x)
	     '()
	     (cons (car x) (even (cdr x))))))
               
      '(define even
	 (lambda (x)
	   (if (null? x)
	     '()
	     (odd (cdr x)))))
                   
      '(define merge
	 (lambda (x y)
	   (if (null? x) 
	     y 
	     (if (null? y) 
	       x 
	       (if (< (car x) (car y))
		 (cons (car x) (merge (cdr x) y))
		 (cons (car y) (merge x (cdr y)))))))))

;;;;
;;;; quicksort
;;;;

    (list 'quicksort '(iotas)
      '(define quicksort
	 (lambda (ls)
	   (qsort ls '())))

      '(define qsort
	 (lambda (ls rest)
	   (if (null? ls)
	     rest
	     (let ([pivot (car ls)])
	       (let ([ls (cdr ls)])
		 (qsort (less pivot ls)
		   (cons pivot (qsort (more pivot ls) rest))))))))

      '(define less
	 (lambda (pivot ls)
	   (if (null? ls)
	     '()
	     (let ([rr (less pivot (cdr ls))])
	       (let ([item (car ls)])
		 (if (< pivot item)
		   rr
		   (cons item rr)))))))

      '(define more
	 (lambda (pivot ls)
	   (if (null? ls)
	     '()
	     (let ([rr (more pivot (cdr ls))])
	       (let ([item (car ls)])
		 (if (< pivot item)
		   (cons item rr)
		   rr)))))))))

(do ([procs procedures (cdr procs)])
  [(null? procs)]
  (do ([sizes sizes (cdr sizes)]
       [i 0 (+ i 1)])
    [(null? sizes)]
    (printf "~n(~a ~a)~n" (caar procs) (car sizes))
    (pretty-print
      (count (cddar procs) `(,(caar procs) .
			      ,(map (lambda (x) `(vector-ref ,x ,i))
				 (cadar procs)))))))