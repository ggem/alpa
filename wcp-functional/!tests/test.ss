;; test.ss
;;   contains functions that call live-mem on input programs with several
;;   different sizes of arguments

;(define list-sizes1 '(10 20))
;(define list-sizes2 '(30 40))

;; merge-sort
;(define list-sizes1 
;  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
;(define list-sizes1 
;  '(11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
;(define list-sizes2 '())

(define list-sizes1 '(10 20 50 100 200 300 500 1000 1500 2000))
(define list-sizes2 '())

;(define list-sizes1 '(10 20 50 100 200))
;(define list-sizes1 '())
;(define list-sizes2 '(300 500 1000 2000))

;; insertion sort 
;(define list-sizes1 '())
;(define list-sizes2 '(500 1000 2000))

;(define list-sizes-2args-lcs-incr
;  '((10 . 10) (20 . 20) (50 . 50) (100 . 100) (200 . 200)))

;(define list-sizes-2args-lcs-incr
;  '((13 . 13) (14 . 14) (16 . 16) (17 . 17) (19 . 19) (30 . 30) (40 . 40)
;    (50 . 50)))

;(define list-sizes-2args-lcs-incr
;  '((300 . 300) (500 . 500) (1000 . 1000) (2000 . 2000)))

;(define list-sizes-2args-lcs-incr
;  '((1500 . 1500)))

;(define list-sizes-2args-lcs-incr
;  '((10 . 10) (20 . 20) (50 . 50) (100 . 100) (200 . 200) (300 . 300) 
;    (500 . 500) (1000 . 1000) (2000 . 2000)))

(define list-sizes-2args-lcs-incr
  '((10 . 10) (20 . 20) (50 . 50) (100 . 100) (200 . 200)))

(define num-repeat1 10.0)
(define num-repeat2 2.0)

(define compare-space-bound-fns
  (lambda (space-fn bound-fn list-sizes)
    (letrec ([loop (lambda (ls)
		     (if (null? ls)
			 (void)
			 (begin 
			   (display
			    (list (car ls)
				  ((space-fn (car ls)))
				  ((bound-fn (car ls)))))
			   (newline)
			   (loop (cdr ls)))))])
      (loop list-sizes))))

(define compare-space-bound-fns2
  (lambda (space-fn bound-fn list-sizes-2args) 
    (letrec ([loop (lambda (ls)
		     (if (null? ls)
			 (void)
			 (begin 
			   (display
			    (list (car ls)
				  ((space-fn (caar ls) (cadar ls)))
				  ((bound-fn (caar ls) (cadar ls)))))
			   (newline)
			   (loop (cdr ls)))))])
      (loop list-sizes-2args))))

(define test-with-gc
  (lambda (fn space-fn bound-fn list-sizes1 num-repeat1 
	   list-sizes2 num-repeat2)
    (letrec ([loop (lambda (ls num-repeat)
		     (if (null? ls)
			 (void)
			 (let ([mult-args (if (list? (car ls)) #t #f)])
			   (display (car ls))
			   (display "  ")
			   (display 
			    (cons 
			     "fn" 
			     (avg-runtime 
			      (if mult-args (apply fn (car ls)) (fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "space-fn"
			     (avg-runtime 
			      (if mult-args 
				  (apply space-fn (car ls))
				  (space-fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "bound-fn"
			     (avg-runtime 
			      (if mult-args 
				  (apply bound-fn (car ls))
				  (bound-fn (car ls)))
			      num-repeat)))
			   (newline)
			   (loop (cdr ls) num-repeat))))])
      (loop list-sizes1 num-repeat1)
      (loop list-sizes2 num-repeat2))))

(define test-no-gc-stats
  (lambda (fn space-fn bound-fn list-sizes1 num-repeat1 
	   list-sizes2 num-repeat2)
    (letrec ([loop (lambda (ls num-repeat)
		     (if (null? ls)
			 (void)
			 (let ([mult-args (if (list? (car ls)) #t #f)])
			   (display (car ls))
			   (display "  ")
			   (display 
			    (cons 
			     "fn" 
			     (avg-runtime-no-gc 
			      (if mult-args (apply fn (car ls)) (fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "space-fn"
			     (avg-runtime-no-gc 
			      (if mult-args 
				  (apply space-fn (car ls))
				  (space-fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "bound-fn"
			     (avg-runtime-no-gc 
			      (if mult-args 
				  (apply bound-fn (car ls))
				  (bound-fn (car ls)))
			      num-repeat)))
			   (newline)
			   (loop (cdr ls) num-repeat))))])
      (loop list-sizes1 num-repeat1)
      (loop list-sizes2 num-repeat2))))

(define test-no-gc-collecthandler
  (lambda (fn space-fn bound-fn list-sizes1 num-repeat1 
	   list-sizes2 num-repeat2)
    (letrec ([loop (lambda (ls num-repeat)
		     (if (null? ls)
			 (void)
			 (let ([mult-args (if (list? (car ls)) #t #f)])
			   (display (car ls))
			   (display "  ")
			   (display 
			    (cons 
			     "fn" 
			     (avg-runtime 
			      (if mult-args (apply fn (car ls)) (fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "space-fn"
			     (avg-runtime 
			      (if mult-args 
				  (apply space-fn (car ls))
				  (space-fn (car ls)))
			      num-repeat)))
			   (display "  ")
			   (display 
			    (cons 
			     "bound-fn"
			     (avg-runtime 
			      (if mult-args 
				  (apply bound-fn (car ls))
				  (bound-fn (car ls)))
			      num-repeat)))
			   (newline)
			   (loop (cdr ls) num-repeat))))]
	     [old-collect-request-handler (collect-request-handler)])
      (collect-request-handler void)
      (loop list-sizes1 num-repeat1)
      (loop list-sizes2 num-repeat2)
      (collect-request-handler old-collect-request-handler)
      (collect))))

(define avg-runtime
  (lambda (fn num-repeat)
    (let ([s1 #f]
	  [s2 #f])
      (do ((i 0 (add1 i)) 
	   (cpu 0 (+ cpu (- (sstats-cpu s2) (sstats-cpu s1))))
	   (real 0 (+ real (- (sstats-real s2) (sstats-real s1)))))
	  ((= i num-repeat) (list (/ cpu num-repeat) (/ real num-repeat)))
	(set! s1 (statistics))
	(fn)
	(set! s2 (statistics))
	(collect)))))

(define avg-runtime-no-gc
  (lambda (fn num-repeat)
    (let ([s1 #f]
	  [s2 #f])
      (do ((i 0 (add1 i)) 
	   (cpu 0 (+ cpu (- (- (sstats-cpu s2) (sstats-cpu s1))
			    (- (sstats-gc-cpu s2) (sstats-gc-cpu s1)))))
	   (real 0 (+ real (- (- (sstats-real s2) (sstats-real s1))
			      (- (sstats-gc-real s2) (sstats-gc-real s1))))))
	  ((= i num-repeat) (list (/ cpu num-repeat) (/ real num-repeat)))
	(set! s1 (statistics))
	(fn)
	(set! s2 (statistics))))))
  
;; insertion sort
(define insertion-sort-n
  (letrec ([make-worst-case-list
	    (lambda (n i)
	      (if (= n 0)
		  '()
		  `(cons ,i ,(make-worst-case-list (- n 1) (+ i 1)))))])
    (lambda (n)
      (lambda ()
	(eval `(let ()
		 (define insert
		   (lambda (x ls)
		     (if (null? ls)
			 (cons x ())
			 (if (<= x (car ls))
			     (cons x ls)
			     (cons (car ls) (insert x (cdr ls)))))))
		 (define insertion-sort-helper
		   (lambda (ls sorted-ls)
		     (if (null? ls)
			 sorted-ls
			 (insertion-sort-helper 
			  (cdr ls) 
			  (insert (car ls) sorted-ls)))))
		 (define insertion-sort
		   (lambda (ls)
		     (insertion-sort-helper ls ())))
		 (insertion-sort ,(make-worst-case-list n 1))))))))

(define space-insertion-sort-n
  (letrec ([make-worst-case-list 
	    (lambda (n i)
	      (if (= n 0)
		  '()
		  `(cons ,i ,(make-worst-case-list (- n 1) (+ i 1)))))])
    (lambda (n)
      (lambda ()
	(live-mem-countall `((define insert
			       (lambda (x ls)
				 (if (null? ls)
				     (cons x ())
				     (if (<= x (car ls))
					 (cons x ls)
					 (cons (car ls) 
					       (insert x (cdr ls)))))))
			     (define insertion-sort-helper
			       (lambda (ls sorted-ls)
				 (if (null? ls)
				     sorted-ls
				     (insertion-sort-helper 
				      (cdr ls) 
				      (insert (car ls) sorted-ls)))))
			     (define insertion-sort
			       (lambda (ls)
				 (insertion-sort-helper ls '())))
			     (insertion-sort ,(make-worst-case-list n 1))))))))

(define bound-insertion-sort-n
  (lambda (n)
    (lambda ()
      (live-mem-countall `((define insert
			     (lambda (x ls)
			       (if (null? ls)
				   (cons x ())
				   (if (<= x (car ls))
				       (cons x ls)
				       (cons (car ls) (insert x (cdr ls)))))))
			   (define insertion-sort-helper
			     (lambda (ls sorted-ls)
			       (if (null? ls)
				   sorted-ls
				   (insertion-sort-helper 
				    (cdr ls) 
				    (insert (car ls) sorted-ls)))))
			   (define insertion-sort
			     (lambda (ls)
			       (insertion-sort-helper ls '())))
			   (insertion-sort ,(make-cons-list n)))))))

(define time-ins-sort
  (lambda (sizes)
    (test-no-gc-stats insertion-sort-n 
		      space-insertion-sort-n
		      bound-insertion-sort-n
		      sizes
		      10.0
		      '()
		      #f)))

(define compare-ins-sort-fns
  (lambda (sizes)
    (compare-space-bound-fns space-insertion-sort-n bound-insertion-sort-n
			     sizes)))

;; selection-sort
;;   the definition of rem-elem is not the usual one; instead of 
;;     (if (null? ls) () ...)
;;   we use
;;     (if (null? (cdr ls)) () ...)
;;   this is reasonable because for our purposes rem-element is always used
;;   an element that is known to be in the list given to it; this new
;;   definition is required to avoid infinite calls of the form
;;   (selection-sort x 'unknown)
(define selection-sort-n
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons ,n ,(make-worst-case-list (- n 1)))))])
    (lambda (n)
      (let ([arg-ls (make-worst-case-list n)])
	(lambda ()
	  (eval `(let ()
		   (define least-element 
		     (lambda (ls)
		       (if (null? ls)
			   #f
			   (if (null? (cdr ls))
			       (car ls)
			       (let ([result (least-element (cdr ls))])
				 (if (< (car ls) result)
				     (car ls)
				     result))))))
		   (define rem-elem
		     (lambda (x ls)
		       (if (null? (cdr ls))
			   ()
			   (if (= (car ls) x)
			       (cdr ls)
			       (cons (car ls) (rem-elem x (cdr ls)))))))
		   (define selection-sort
		     (lambda (ls)
		       (if (null? ls)
			   ls
			   (let ([min (least-element ls)])
			     (let ([newls (rem-elem min ls)])
			       (cons min (selection-sort newls)))))))
		   (selection-sort ,arg-ls))))))))

(define space-selection-sort-n
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons ,n ,(make-worst-case-list (- n 1)))))])
    (lambda (n)
      (let ([arg-ls (make-worst-case-list n)])
	(lambda ()
	  (live-mem-countall `((define least-element 
				 (lambda (ls)
				   (if (null? ls)
				       #f
				       (if (null? (cdr ls))
					   (car ls)
					   (let ([result 
						  (least-element (cdr ls))])
					     (if (< (car ls) result)
						 (car ls)
						 result))))))
			       (define rem-elem
				 (lambda (x ls)
				   (if (null? (cdr ls))
				       ()
				       (if (= (car ls) x)
					   (cdr ls)
					   (cons (car ls) 
						 (rem-elem x (cdr ls)))))))
			       (define selection-sort
				 (lambda (ls)
				   (if (null? ls)
				       ls
				       (let ([min (least-element ls)])
					 (let ([newls (rem-elem min ls)])
					   (cons min 
						 (selection-sort newls)))))))
			       (selection-sort ,arg-ls))))))))

(define bound-selection-sort-n
  (lambda (n)
    (let ([arg-ls (make-cons-list n)])
      (lambda ()
	(live-mem-countall `((define least-element 
			       (lambda (ls)
				 (if (null? ls)
				     #f
				     (if (null? (cdr ls))
					 (car ls)
					 (let ([result 
						(least-element (cdr ls))])
					   (if (< (car ls) result)
					       (car ls)
					       result))))))
			     (define rem-elem
			       (lambda (x ls)
				 (if (null? (cdr ls))
				     ()
				     (if (= (car ls) x)
					 (cdr ls)
					 (cons (car ls) 
					       (rem-elem x (cdr ls)))))))
			     (define selection-sort
			       (lambda (ls)
				 (if (null? ls)
				     ls
				     (let ([min (least-element ls)])
				       (let ([newls (rem-elem min ls)])
					 (cons min (selection-sort newls)))))))
			     (selection-sort ,arg-ls)))))))

(define test-selection-sort
  (lambda ()
    (test selection-sort-n space-selection-sort-n bound-selection-sort-n)))

(define compare-sel-sort-fns
  (lambda (sizes)
    (compare-space-bound-fns space-selection-sort-n bound-selection-sort-n
			     sizes)))

;; merge sort(splitting the list into the first and second halves)
;`((define new-length
;    (lambda (ls)
;      (if (null? ls)
;	  0
;	  (+ 1 (new-length (cdr ls))))))
;  (define first-part
;    (lambda (ls index)
;      (if (= index 0)
;	  ()
;	  (cons (car ls) (first-part (cdr ls) (- index 1))))))
;  (define second-part
;    (lambda (ls index)
;      (if (= index 0)
;	  ls
;	  (second-part (cdr ls) (- index 1)))))
;  (define new-merge
;    (lambda (ls1 ls2)
;      (if (null? ls1)
;	  ls2
;	  (if (null? ls2)
;	      ls1
;	      (if (<= (car ls1) (car ls2))
;		  (cons (car ls1) (new-merge (cdr ls1) ls2)) 
;		  (cons (car ls2) (new-merge ls1 (cdr ls2))))))))
;  (define merge-sort
;    (lambda (ls)
;      (if (or (null? ls) (null? (cdr ls)))
;	  ls
;	  (let ([mid (quotient (new-length ls) 2)])
;	    (new-merge (merge-sort (first-part ls mid))
;		       (merge-sort (second-part ls mid))))))))

;; merge sort(using even and odd)
(define space-merge-sort-n
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  `'()
		  `(cons ,n ,(make-worst-case-list (- n 1)))))]) 
    (lambda (n)
      (let ([arg-ls (make-worst-case-list n)])
	(lambda ()
	  (live-mem-countall `((define even
				 (lambda (ls)
				   (if (null? ls)
				       ()
				       (cons (car ls) (odd (cdr ls))))))
			       (define odd
				 (lambda (ls)
				   (if (null? ls)
				       ()
				       (even (cdr ls)))))
			       (define new-merge
				 (lambda (ls1 ls2)
				   (if (null? ls1)
				       ls2
				       (if (null? ls2)
					   ls1
					   (if (<= (car ls1) (car ls2))
					       (cons (car ls1) 
						     (new-merge (cdr ls1) 
								ls2)) 
					       (cons (car ls2) 
						     (new-merge 
						      ls1 
						      (cdr ls2))))))))
			       (define merge-sort
				 (lambda (ls)
				   (if (null? ls)
				       ls
				       (if (null? (cdr ls))
					   ls
					   (new-merge 
					    (merge-sort (even ls))
					    (merge-sort (odd ls)))))))
			       (merge-sort ,arg-ls))))))))

(define bound-merge-sort-n
  (lambda (n)
    (let ([arg-ls (make-cons-list n)])
      (lambda ()
	(live-mem-countall `((define even
			       (lambda (ls)
				 (if (null? ls)
				     ()
				     (cons (car ls) (odd (cdr ls))))))
			     (define odd
			       (lambda (ls)
				 (if (null? ls)
				     ()
				     (even (cdr ls)))))
			     (define new-merge
			       (lambda (ls1 ls2)
				 (if (null? ls1)
				     ls2
				     (if (null? ls2)
					 ls1
					 (if (<= (car ls1) (car ls2))
					     (cons (car ls1) 
						   (new-merge (cdr ls1) ls2)) 
					     (cons (car ls2) 
						   (new-merge ls1 
							      (cdr ls2))))))))
			     (define merge-sort
			       (lambda (ls)
				 (if (or (null? ls) (null? (cdr ls)))
				     ls
				     (new-merge (merge-sort (even ls))
						(merge-sort (odd ls))))))
			     (merge-sort ,arg-ls)))))))

(define compare-merge-sort-fns
  (lambda (sizes)
    (compare-space-bound-fns space-merge-sort-n bound-merge-sort-n
			     sizes)))

(define null-bound-fn
  (lambda (n)
    (lambda ()
      (void))))

(define test-space-merge-sort
  (lambda (sizes)
    (compare-space-bound-fns space-merge-sort-n null-bound-fn sizes)))

;; reverse(tail-recursive)
(define space-reverse-n
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons 1 ,(make-worst-case-list (- n 1)))))]) 
    (lambda (n)
      (let ([arg-ls (make-worst-case-list n)])
	(lambda ()
	  (live-mem-countall `((define my-reverse
				 (lambda (ls newls)
				   (if (null? ls)
				       newls
				       (my-reverse (cdr ls) 
						   (cons (car ls) newls)))))
			       (my-reverse ,arg-ls ()))))))))

(define bound-reverse-n
  (lambda (n)
    (let ([arg-ls (make-cons-list n)])
      (lambda ()
	(live-mem-countall `((define my-reverse
			       (lambda (ls newls)
				 (if (null? ls)
				     newls
				     (my-reverse (cdr ls) 
						 (cons (car ls) newls)))))
			     (my-reverse ,arg-ls ())))))))

(define compare-reverse-fns
  (lambda (sizes)
    (compare-space-bound-fns space-reverse-n bound-reverse-n sizes)))

;; reverse(using append)
(define space-reverse-append-n
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons 1 ,(make-worst-case-list (- n 1)))))]) 
    (lambda (n)
      (let ([arg-ls (make-worst-case-list n)])
	(lambda ()
	  (live-mem-countall `((define my-append
				 (lambda (ls1 ls2)
				   (if (null? ls1)
				       ls2
				       (cons (car ls1) 
					     (my-append (cdr ls1) ls2)))))
			       (define my-reverse
				 (lambda (ls)
				   (if (null? ls)
				       ()
				       (my-append (my-reverse (cdr ls)) 
						  (cons (car ls) ())))))
			       (my-reverse ,arg-ls))))))))

(define bound-reverse-append-n
  (lambda (n)
    (let ([arg-ls (make-cons-list n)])
      (lambda ()
	(live-mem-countall `((define my-append
			       (lambda (ls1 ls2)
				 (if (null? ls1)
				     ls2
				     (cons (car ls1) 
					   (my-append (cdr ls1) ls2)))))
			     (define my-reverse
			       (lambda (ls)
				 (if (null? ls)
				     ()
				     (my-append (my-reverse (cdr ls)) 
						(cons (car ls) ())))))
			     (my-reverse ,arg-ls)))))))

(define compare-reverse-append-fns
  (lambda (sizes)
    (compare-space-bound-fns space-reverse-append-n bound-reverse-append-n
			     sizes)))

;; set union
(define space-set-union-n
  (letrec ([make-worst-case-list
	    (lambda (n x)
	      (if (= n x)
		  '()
		  `(cons ,n ,(make-worst-case-list (- n 1) x))))])
    (lambda (n)
      (let ([arg-ls1 (make-worst-case-list n 0)]
	    [arg-ls2 (make-worst-case-list (* n 2) n)])
	(lambda ()
	  (live-mem `((define member?
			(lambda (x ls)
			  (if (null? ls)
			      #f
			      (if (= x (car ls))
				  #t
				  (member? x (cdr ls))))))
		      (define set-union
			(lambda (ls1 ls2)
			  (if (null? ls1)
			      ls2
			      (let ([result (set-union (cdr ls1) ls2)])
				(if (member? (car ls1) result)
				    result
				    (cons (car ls1) result))))))
		      (set-union ,arg-ls1 ,arg-ls2))))))))

; set union using set intersection
(define bound-set-union-n
  (lambda (n)
    (let ([arg-ls1 (make-cons-list n)]
	  [arg-ls2 (make-cons-list n)])
      (lambda ()
	(live-mem `((define my-append
			(lambda (ls1 ls2)
			  (if (null? ls1)
			      ls2
			      (cons (car ls1) (my-append (cdr ls1) ls2)))))
		    (define member?
		      (lambda (x ls)
			(if (null? ls)
			    #f
			    (or (= x (car ls)) (member? x (cdr ls))))))
		    (define set-union-helper
		      (lambda (ls1 ls2)
			(if (null? ls1)
			    '()
			    (if (member? (car ls1) ls2)
				(set-union-helper (cdr ls1) ls2)
				(cons (car ls1) 
				      (set-union-helper (cdr ls1) ls2))))))
		    (define set-union
		      (lambda (ls1 ls2)
			(my-append (set-union-helper ls1 ls2) ls2)))
		    (set-union ,arg-ls1 ,arg-ls2)))))))
		  
(define compare-set-union-fns
  (lambda (sizes)
    (compare-space-bound-fns space-set-union-n bound-set-union-n sizes)))

;; set intersection
(define space-set-intersect-n
  (letrec ([make-worst-case-list
	    (lambda (n x)
	      (if (= n x)
		  '()
		  `(cons ,n ,(make-worst-case-list (- n 1) x))))])
    (lambda (n)
      (let ([arg-ls1 (make-worst-case-list n 0)]
	    [arg-ls2 (make-worst-case-list n 0)])
	(lambda ()
	  (live-mem `((define member?
			(lambda (x ls)
			  (if (null? ls)
			      #f
			      (if (= x (car ls))
				  #t
				  (member? x (cdr ls))))))
		      (define set-intersect
			(lambda (ls1 ls2)
			  (if (null? ls1)
			      '()
			      (if (member? (car ls1) ls2)
				  (cons (car ls1) 
					(set-intersect (cdr ls1) ls2))
				  (set-intersect (cdr ls1) ls2)))))
		      (set-intersect ,arg-ls1 ,arg-ls2))))))))

(define bound-set-intersect-n
  (lambda (n)
    (let ([arg-ls1 (make-cons-list n)]
	  [arg-ls2 (make-cons-list n)])
      (lambda ()
	(live-mem `((define member?
			(lambda (x ls)
			  (if (null? ls)
			      #f
			      (if (= x (car ls))
				  #t
				  (member? x (cdr ls))))))
		      (define set-intersect
			(lambda (ls1 ls2)
			  (if (null? ls1)
			      '()
			      (if (member? (car ls1) ls2)
				  (cons (car ls1) 
					(set-intersect (cdr ls1) ls2))
				  (set-intersect (cdr ls1) ls2)))))
		      (set-intersect ,arg-ls1 ,arg-ls2)))))))
		  
(define compare-set-intersect-fns
  (lambda (sizes)
    (compare-space-bound-fns space-set-intersect-n bound-set-intersect-n
			     sizes)))

;; pascal-tree
(define space-pascal-tree-n
  (lambda (n)
    (lambda () 
      (void))))

(define bound-pascal-tree-n
  (lambda (n)
    (lambda ()
      (live-mem `((define pascal-tree
		    (lambda (num-rows)
		      (pascal-tree-helper num-rows 0 ())))
		  (define pascal-tree-helper
		    (lambda (num-rows curr-row prev-rows)
		      (if (= curr-row num-rows)
			  prev-rows
			  (let ([new-tree (pascal-row curr-row 0 prev-rows)])
			    (pascal-tree-helper
			     num-rows 
			     (+ curr-row 1) 
			     (new-append prev-rows (cons new-tree ())))))))
		  (define pascal-row
		    (lambda (row col prev-rows)
		      (if (> col row)
			  ()
			  (if (or (= col 0) (= col row))
			      (cons 1 (pascal-row row (+ col 1) prev-rows))
			      (cons 
			       (+ (pascal-elem (- row 1) (- col 1) prev-rows)
				  (pascal-elem (- row 1) col prev-rows))
			       (pascal-row row (+ col 1) prev-rows))))))
		  (define pascal-elem
		    (lambda (row col pascal-tree)
		      (if (null? pascal-tree)
			  #f
			  (if (= row 0)
			      (new-list-ref (car pascal-tree) col)
			      (pascal-elem (- row 1) col (cdr pascal-tree))))))
		  (define new-append
		    (lambda (ls1 ls2)
		      (if (null? ls1)
			  ls2
			  (cons (car ls1) (new-append (cdr ls1) ls2)))))
		  (define new-list-ref
		    (lambda (ls n)
		      (if (null? ls)
			  #f
			  (if (= n 0)
			      (car ls)
			      (new-list-ref (cdr ls) (- n 1))))))
		  (pascal-tree ,n))))))
		  
(define compare-pascal-tree-fns
  (lambda (sizes)
    (compare-space-bound-fns space-pascal-tree-n bound-pascal-tree-n
			     sizes)))

(define list->cons-list
    (lambda (ls)
      (if (null? ls)
	  '()
	  (list 'cons (car ls) (list->cons-list (cdr ls))))))

;; space-lcs-incr-lists
;;   given two strings represented as lists, determines the live heap space
;;   usage of lcs-incr when applied to the two strings
(define space-lcs-incr-lists
  (lambda (ls1 ls2)
    (let ([n (length ls1)]
	  [m (length ls2)]
	  [arg-ls1 (list->cons-list ls1)]
	  [arg-ls2 (list->cons-list ls2)])
      (live-mem-countall `((define lcs
			     (lambda (n m ls1 ls2)
			       (car (lcs-hat n m ls1 ls2))))
			   (define lcs-hat
			     (lambda (n m ls1 ls2)
			       (if (or (= n 0) (= m 0))
				   (cons 0 '())
				   (let ([r (lcs-hat (- n 1) m ls1 ls2)])
				     (lcs-hat-prime n m ls1 ls2 r)))))
			   (define lcs-hat-prime
			     (lambda (n m ls1 ls2 r)
			       (if (or (= n 0) (= m 0))
				   (cons 0 '())
				   (if (= n 1)
				       (let ([v2 
					      (lcs-hat-prime n 
							     (- m 1) 
							     ls1 
							     ls2 
							     (cons 0 
								   '()))])
					 (if (= (new-list-ref ls1 n) 
						(new-list-ref ls2 m))
					     (cons 1 v2)
					     (cons (car v2) v2)))
				       (let ([v2 
					      (lcs-hat-prime n 
							     (- m 1) 
							     ls1 
							     ls2 
							     (cdr r))])
					 (if (= (new-list-ref ls1 n) 
						(new-list-ref ls2 m))
					     (cons (+ (car (cdr r)) 1) v2)
					     (cons 
					      (new-max (car v2) (car r)) 
					      v2)))))))
			   (define new-list-ref
			     (lambda (ls index)
			       (if (= index 1)
				   (car ls)
				   (new-list-ref (cdr ls) (- index 1)))))
			   (define new-max
			     (lambda (x y)
			       (if (> x y) x y)))
			   (lcs ,n ,m ,arg-ls1 ,arg-ls2))))))

;; longest common subsequence(incremental version)
;; given n, m, lcs-incr uses the same amount of live space on all lists of
;; sizes n and m
(define space-lcs-incr-n-m
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons 1 ,(make-worst-case-list (- n 1)))))])
    (lambda (n m)
      (let ([arg-ls1 (make-worst-case-list n)]
	    [arg-ls2 (make-worst-case-list m)])
	(lambda ()
	  (live-mem-countall `((define lcs
				 (lambda (n m ls1 ls2)
				   (car (lcs-hat n m ls1 ls2))))
			       (define lcs-hat
				 (lambda (n m ls1 ls2)
				   (if (or (= n 0) (= m 0))
				       (cons 0 '())
				       (let ([r (lcs-hat (- n 1) m ls1 ls2)])
					 (lcs-hat-prime n m ls1 ls2 r)))))
			       (define lcs-hat-prime
				 (lambda (n m ls1 ls2 r)
				   (if (or (= n 0) (= m 0))
				       (cons 0 '())
				       (if (= n 1)
					   (let ([v2 
						  (lcs-hat-prime n 
								 (- m 1) 
								 ls1 
								 ls2 
								 (cons 0 
								       '()))])
					     (if (= (new-list-ref ls1 n) 
						    (new-list-ref ls2 m))
						 (cons 1 v2)
						 (cons (car v2) v2)))
					   (let ([v2 
						  (lcs-hat-prime n 
								 (- m 1) 
								 ls1 
								 ls2 
								 (cdr r))])
					     (if (= (new-list-ref ls1 n) 
						    (new-list-ref ls2 m))
						 (cons (+ (car (cdr r)) 1) v2)
						 (cons 
						  (new-max (car v2) (car r)) 
						  v2)))))))
			       (define new-list-ref
				 (lambda (ls index)
				   (if (= index 1)
				       (car ls)
				       (new-list-ref (cdr ls) (- index 1)))))
			       (define new-max
				 (lambda (x y)
				   (if (> x y) x y)))
			       (lcs ,n ,m ,arg-ls1 ,arg-ls2))))))))

(define bound-lcs-incr-n-m
  (lambda (n m)
    (let ([arg-ls1 (make-cons-list n)]
	  [arg-ls2 (make-cons-list m)])
      (lambda ()
	(live-mem-countall `((define lcs
			       (lambda (n m ls1 ls2)
				 (car (lcs-hat n m ls1 ls2))))
			     (define lcs-hat
			       (lambda (n m ls1 ls2)
				 (if (or (= n 0) (= m 0))
				     (cons 0 '())
				     (let ([r (lcs-hat (- n 1) m ls1 ls2)])
				       (lcs-hat-prime n m ls1 ls2 r)))))
			     (define lcs-hat-prime
			       (lambda (n m ls1 ls2 r)
				 (if (or (= n 0) (= m 0))
				     (cons 0 '())
				     (if (= n 1)
					 (let ([v2 
						(lcs-hat-prime n 
							       (- m 1) 
							       ls1 
							       ls2 
							       (cons 0 '()))])
					   (if (= (new-list-ref ls1 n) 
						  (new-list-ref ls2 m))
					       (cons 1 v2)
					       (cons (car v2) v2)))
					 (let ([v2 
						(lcs-hat-prime n 
							       (- m 1) 
							       ls1 
							       ls2 
							       (cdr r))])
					   (if (= (new-list-ref ls1 n) 
						  (new-list-ref ls2 m))
					       (cons (+ (car (cdr r)) 1) v2)
					       (cons 
						(new-max (car v2) (car r)) 
						v2)))))))
			     (define new-list-ref
			       (lambda (ls index)
				 (if (= index 1)
				     (car ls)
				     (new-list-ref (cdr ls) (- index 1)))))
			     (define new-max
			       (lambda (x y)
				 (if (> x y) x y)))
			     (lcs ,n ,m ,arg-ls1 ,arg-ls2)))))))

(define compare-lcs-incr-fns
  (lambda (sizes)
    (compare-space-bound-fns2 space-lcs-incr-n-m bound-lcs-incr-n-m
			      sizes)))

;; binomial coefficients
(define space-bincoeff-incr-n-m
  (lambda (n m)
    (lambda ()
      (live-mem-countall `((define b
			     (lambda (n m)
			       (car (bhat n m))))
			   (define bhat
			     (lambda (n m)
			       (if (= m 0)
				   (cons 1 '())
				   (if (= m n)
				       (cons 1 '())
				       (bhatprime n m (bhat (- n 1) m))))))
			   (define bhatprime
			     (lambda (n m r)
			       (if (= m 0)
				   (cons 1 '())
				   (if (= m n)
				       (cons 1 '())
				       (if (= m (- n 1))
					   (let ([v1 (bhatprime (- n 1) 
								(- m 1) 
								(cons 1 '()))])
					     (cons (+ 1 (car v1)) 
						   (cons v1 '())))
					   (let ([v1 (bhatprime 
						      (- n 1) 
						      (- m 1) 
						      (car (cdr r)))])
					     (cons (+ (car v1) (car r)) 
						   (cons v1 '()))))))))
			   (b ,n ,m))))))

(define null-bound-fn2
  (lambda (n m)
    (lambda ()
      (void))))

;; using compare-bincoeff-incr-fns-n, it was determined that 
;;   live-space(b(n, n - 2)) > live-space(b(n, i)), 0 <= i <= n, i != (n - 2)
;(define bincoeff-incr-args
;  '((10 . 8) (20 . 18) (50 . 48) (100 . 98) (200 . 198) (300 . 298) 
;    (500 . 498) (1000 . 998) (2000 . 1998)))

(define bincoeff-incr-args
  '((10 . 8) (20 . 18) (50 . 48) (100 . 98) (200 . 198)))

(define compare-bincoeff-incr-fns
  (lambda (sizes)
    (compare-space-bound-fns2 space-bincoeff-incr-n-m null-bound-fn2
			      sizes)))

(define make-list-n-rangem
  (lambda (n)
    (letrec ([loop (lambda (m)
		     (if (= n m)
			 (list (cons n m))
			 (cons (cons n m) (loop (+ m 1)))))])
      (loop 0))))

(define compare-bincoeff-incr-fns-n
  (lambda (n)
    (compare-space-bound-fns2 space-bincoeff-incr-n-m null-bound-fn2
			      (make-list-n-rangem n))))

;; string editing
(define string-edit-incr-n-m
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons 1 ,(make-worst-case-list (- n 1)))))])
    (lambda (n m)
      (let ([str1 (make-worst-case-list n)]
	    [str2 (make-worst-case-list m)])
	(lambda ()
	  (eval `(let () 
		   (define string-edit-incr
		     (lambda (i j str1 str2)
		       (car (string-edit-bar i j str1 str2))))
		   (define string-edit-bar
		     (lambda (i j str1 str2)
		       (if (and (= i 0) (= j 0))
			   (cons 0 ())
			   (if (= j 0)
			       (let ([v (string-edit-bar (- i 1) 
							 0 
							 str1 
							 str2)])
				 (cons (+ (car v) 1) (cons v ())))
			       (if (= i 0)
				   (let ([v (string-edit-bar 
					     0 
					     (- j 1) 
					     str1 
					     str2)])
				     (cons (+ (car v) 2) 
					   (cons v ())))
				   (let ([r (string-edit-bar 
					     i 
					     (- j 1) 
					     str1 
					     str2)])
				     (string-edit-prime i 
							j 
							str1 
							str2 
							r)))))))
		   (define string-edit-prime
		     (lambda (i j str1 str2 r)
		       (if (= i 0)
			   (if (= (- j 1) 0)
			       (cons 2 (cons (cons 0 ()) ()))
			       (let ([v (string-edit-prime 
					 0 
					 (- j 1) 
					 str1 
					 str2 
					 (car (cdr r)))])
				 (cons (+ (car v) 2) (cons v ()))))
			   (if (= (- j 1) 0)
			       (let ([v1 (car (cdr r))]
				     [v2 (string-edit-prime 
					  (- i 1) 
					  1 
					  str1 
					  str2 
					  (car (cdr r)))]
				     [v3 r])
				 (cons (min-cost 
					(+ (car v1) 
					   (cost-subst 
					    (str-ref str1 i) 
					    (str-ref str2 j)))
					(+ (car v2) 1)
					(+ (car v3) 2))
				       (cons v2 ())))
			       (let ([v1 (car (cdr r))]
				     [v2 (string-edit-prime 
					  (- i 1) 
					  j 
					  str1 
					  str2 
					  (car (cdr r)))]
				     [v3 r])
				 (cons (min-cost 
					(+ (car v1)
					   (cost-subst 
					    (str-ref str1 i) 
					    (str-ref str2 j)))
					(+ (car v2) 1)
					(+ (car v3) 2))
				       (cons v2 ())))))))
		   (define str-ref
		     (lambda (str i)
		       (if (= (- i 1) 0)
			   (car str)
			   (str-ref (cdr str) (- i 1)))))
		   (define min-cost
		     (lambda (c1 c2 c3)
		       (if (> c1 c2)
			   (if (> c2 c3) c3 c2)
			   (if (> c1 c3) c3 c1))))
		   (define cost-subst
		     (lambda (c1 c2)
		       (if (= c1 c2) 0 3)))
		   (string-edit-incr ,n ,m ,str1 ,str2))))))))

(define space-string-edit-incr-n-m
  (letrec ([make-worst-case-list
	    (lambda (n)
	      (if (= n 0)
		  '()
		  `(cons 1 ,(make-worst-case-list (- n 1)))))])
    (lambda (n m)
      (let ([str1 (make-worst-case-list n)]
	    [str2 (make-worst-case-list m)])
	(lambda ()
	  (live-mem-countall `((define string-edit-incr
				 (lambda (i j str1 str2)
				   (car (string-edit-bar i j str1 str2))))
			       (define string-edit-bar
				 (lambda (i j str1 str2)
				   (if (and (= i 0) (= j 0))
				       (cons 0 ())
				       (if (= j 0)
					   (let ([v (string-edit-bar (- i 1) 
								     0 
								     str1 
								     str2)])
					     (cons (+ (car v) 1) (cons v ())))
					   (if (= i 0)
					       (let ([v (string-edit-bar 
							 0 
							 (- j 1) 
							 str1 
							 str2)])
						 (cons (+ (car v) 2) 
						       (cons v ())))
					       (let ([r (string-edit-bar 
							 i 
							 (- j 1) 
							 str1 
							 str2)])
						 (string-edit-prime i 
								    j 
								    str1 
								    str2 
								    r)))))))
			       (define string-edit-prime
				 (lambda (i j str1 str2 r)
				   (if (= i 0)
				       (if (= (- j 1) 0)
					   (cons 2 (cons (cons 0 ()) ()))
					   (let ([v (string-edit-prime 
						     0 
						     (- j 1) 
						     str1 
						     str2 
						     (car (cdr r)))])
					     (cons (+ (car v) 2) (cons v ()))))
				       (if (= (- j 1) 0)
					   (let ([v1 (car (cdr r))]
						 [v2 (string-edit-prime 
						      (- i 1) 
						      1 
						      str1 
						      str2 
						      (car (cdr r)))]
						 [v3 r])
					     (cons (min-cost 
						    (+ (car v1) 
						       (cost-subst 
							(str-ref str1 i) 
							(str-ref str2 j)))
						    (+ (car v2) 1)
						    (+ (car v3) 2))
						   (cons v2 ())))
					   (let ([v1 (car (cdr r))]
						 [v2 (string-edit-prime 
						      (- i 1) 
						      j 
						      str1 
						      str2 
						      (car (cdr r)))]
						 [v3 r])
					     (cons (min-cost 
						    (+ (car v1)
						       (cost-subst 
							(str-ref str1 i) 
							(str-ref str2 j)))
						    (+ (car v2) 1)
						    (+ (car v3) 2))
						   (cons v2 ())))))))
			       (define str-ref
				 (lambda (str i)
				   (if (= (- i 1) 0)
				       (car str)
				       (str-ref (cdr str) (- i 1)))))
			       (define min-cost
				 (lambda (c1 c2 c3)
				   (if (> c1 c2)
				       (if (> c2 c3) c3 c2)
				       (if (> c1 c3) c3 c1))))
			       (define cost-subst
				 (lambda (c1 c2)
				   (if (= c1 c2) 0 3)))
			       (string-edit-incr ,n ,m ,str1 ,str2))))))))

(define bound-string-edit-incr-n-m
  (lambda (n m)
    (let ([str1 (make-cons-list n)]
	  [str2 (make-cons-list m)])
      (lambda ()
	(live-mem-countall `((define string-edit-incr
			       (lambda (i j str1 str2)
				 (car (string-edit-bar i j str1 str2))))
			     (define string-edit-bar
			       (lambda (i j str1 str2)
				 (if (and (= i 0) (= j 0))
				     (cons 0 ())
				     (if (= j 0)
					 (let ([v (string-edit-bar (- i 1) 
								   0 
								   str1 
								   str2)])
					   (cons (+ (car v) 1) (cons v ())))
					 (if (= i 0)
					     (let ([v (string-edit-bar 
						       0 
						       (- j 1) 
						       str1 
						       str2)])
					       (cons (+ (car v) 2) 
						     (cons v ())))
					     (let ([r (string-edit-bar 
						       i 
							 (- j 1) 
							 str1 
							 str2)])
					       (string-edit-prime i 
								  j 
								  str1 
								  str2 
								  r)))))))
			     (define string-edit-prime
			       (lambda (i j str1 str2 r)
				 (if (= i 0)
				     (if (= (- j 1) 0)
					 (cons 2 (cons (cons 0 ()) ()))
					 (let ([v (string-edit-prime 
						   0 
						   (- j 1) 
						   str1 
						   str2 
						   (car (cdr r)))])
					   (cons (+ (car v) 2) (cons v ()))))
				     (if (= (- j 1) 0)
					 (let ([v1 (car (cdr r))]
					       [v2 (string-edit-prime 
						    (- i 1) 
						    1 
						    str1 
						    str2 
						    (car (cdr r)))]
					       [v3 r])
					   (cons (min-cost 
						  (+ (car v1) 
						     (cost-subst 
						      (str-ref str1 i) 
						      (str-ref str2 j)))
						  (+ (car v2) 1)
						  (+ (car v3) 2))
						 (cons v2 ())))
					 (let ([v1 (car (cdr r))]
					       [v2 (string-edit-prime 
						    (- i 1) 
						    j 
						    str1 
						    str2 
						    (car (cdr r)))]
					       [v3 r])
					   (cons (min-cost 
						  (+ (car v1)
						     (cost-subst 
						      (str-ref str1 i) 
						      (str-ref str2 j)))
						  (+ (car v2) 1)
						  (+ (car v3) 2))
						 (cons v2 ())))))))
			     (define str-ref
			       (lambda (str i)
				 (if (= (- i 1) 0)
				     (car str)
				     (str-ref (cdr str) (- i 1)))))
			     (define min-cost
			       (lambda (c1 c2 c3)
				 (if (> c1 c2)
				     (if (> c2 c3) c3 c2)
				     (if (> c1 c3) c3 c1))))
			     (define cost-subst
			       (lambda (c1 c2)
				 (if (= c1 c2) 0 3)))
			     (string-edit-incr ,n ,m ,str1 ,str2)))))))

(define compare-string-edit-incr-fns
  (lambda (sizes)
    (compare-space-bound-fns2 space-string-edit-incr-n-m 
			      bound-string-edit-incr-n-m
			      sizes)))

(define time-string-edit
  (lambda ()
    (test-no-gc-stats string-edit-incr-n-m
		      space-string-edit-incr-n-m
		      bound-string-edit-incr-n-m
		      '((10 10) (20 20) (50 50) (100 100) (200 200)
			(300 300) (400 400) (500 500) (700 700) (1000 1000))
		      20.0
		      '()
		      #f)))

;; interpreter for call-by-value lambda calculus
(define space-interpret
  (lambda (exp)
    (live-mem-countall `((define my-interpret
			   (lambda (exp)
			     (my-eval exp (tuple '() '()))))
			 (define my-eval
			   (lambda (exp env)
			     (if 
			      (integer? exp) 
			      exp
			      (if 
			       (symbol? exp) 
			       (lookup exp (car env) (car (cdr env)))
			       (let ([len (my-length exp)])
				 (if 
				  (and (= len 4) (eq? (car exp) 'if))
				  (if (= (my-eval (car (cdr exp)) env) 1)
				      (my-eval (car (cdr (cdr exp))) env)
				      (my-eval (car (cdr (cdr (cdr exp)))) 
					       env))
				  (if 
				   (= len 2)
				   (let ([e (car exp)]
					 [f (car (cdr exp))])
				     (let ([f1 (my-eval f env)]
					   [closure-e (my-eval e env)])
				       (let ([lambda-e (car closure-e)]
					     [env-e (car 
						     (cdr closure-e))])
					 (my-eval 
					  (car (cdr (cdr lambda-e)))
					  (tuple 
					   (cons (car (cdr lambda-e))
						 (car env-e))
					   (cons f1 
						 (car (cdr env-e))))))))
				   (if 
				    (= len 3)
				    (let ([first (car exp)])
				      (if 
				       (eq? first 'lambda)
				       (tuple exp env)
				       (let ([v1 (my-eval (car (cdr exp)) env)]
					     [v2 (my-eval (car (cdr (cdr exp)))
							  env)])
					 (if 
					  (eq? first '+)
					  (+ v1 v2)
					  (if
					   (eq? first '-) 
					   (- v1 v2)
					   (if
					    (eq? first '*) 
					    (* v1 v2)
					    (if
					     (eq? first '/) 
					     (/ v1 v2)
					     (if
					      (eq? first '=) 
					      (if (= v1 v2) 1 0)
					      (if 
					       (eq? first '<) 
					       (if (< v1 v2) 1 0)
					       #f)))))))))
				    #f))))))))
			 (define tuple
			   (lambda (x y)
			     (cons x (cons y ()))))
			 (define lookup
			   (lambda (x ns vs)
			     (if (eq? x (car ns))
				 (car vs)
				 (lookup x (cdr ns) (cdr vs)))))
			 (define my-length
			   (lambda (ls)
			     (if (null? ls)
				 0
				 (+ 1 (my-length (cdr ls))))))
			 (my-interpret ,exp)))))
  
(define exp->cons-list
  (lambda (exp)
    (if (atom? exp)
	(if (symbol? exp) (quote exp) exp)
	(if (atom? (car exp))
	    `(cons ,(if (symbol? (car exp)) `(quote ,(car exp)) (car exp))
		   ,(exp->cons-list (cdr exp)))
	    `(cons ,(exp->cons-list (car exp)) ,(exp->cons-list (cdr exp)))))))

(define interpreter-program
  `((define my-interpret
      (lambda (exp)
	(my-eval exp (tuple '() '()))))
    (define my-eval
      (lambda (exp env)
	(if 
	 (integer? exp) 
	 exp
	 (if 
	  (symbol? exp) 
	  (lookup exp (car env) (car (cdr env)))
	  (let ([len (my-length exp)])
	    (if 
	     (and (= len 4) (eq? (car exp) 'if))
	     (if (= (my-eval (car (cdr exp)) env) 1)
		 (my-eval (car (cdr (cdr exp))) env)
		 (my-eval (car (cdr (cdr (cdr exp)))) env))
	     (if 
	      (= len 2)
	      (let ([e (car exp)]
		    [f (car (cdr exp))])
		(let ([f1 (my-eval f env)]
		      [closure-e (my-eval e env)])
		  (let ([lambda-e (car closure-e)]
			[env-e (car (cdr closure-e))])
		    (my-eval (car (cdr (cdr lambda-e)))
			     (tuple (cons (car (cdr lambda-e)) (car env-e))
				    (cons f1 (car (cdr env-e))))))))
	      (if 
	       (= len 3)
	       (let ([first (car exp)])
		 (if 
		  (eq? first 'lambda)
		  (tuple exp env)
		  (let ([v1 (my-eval (car (cdr exp)) env)]
			[v2 (my-eval (car (cdr (cdr exp))) env)])
		    (if 
		     (eq? first '+)
		     (+ v1 v2)
		     (if
		      (eq? first '-) 
		      (- v1 v2)
		      (if
		       (eq? first '*) 
		       (* v1 v2)
;		       (if
;			(eq? first '/) 
;			(/ v1 v2)
;			(if
;			 (eq? first '=) 
;			 (if (= v1 v2) 1 0)
;			 (if 
;			  (eq? first '<) 
;			  (if (< v1 v2) 1 0)
;			  #f)))))))))
		       #f))))))
	       #f))))))))
    (define tuple
      (lambda (x y)
	(cons x (cons y ()))))
    (define lookup
      (lambda (x ns vs)
	(if (eq? x (car ns))
	    (car vs)
	    (lookup x (cdr ns) (cdr vs)))))))

(define test-interpret
    (lambda (exp)
      (let ([in (open-input-file "tests/T-interpret")])
	(let ([T-interpret (read in)])
	  (close-port in)
	  (eval (add-auxfn-var-defs 
		 (append! T-interpret 
			  (transform-program (list exp) T T))))))))

;; insertion into binary trees
(define space-insert-complete-bintree
  (lambda (h)
    (let ([tree (exp->cons-list (make-complete-bintree h))])
      (lambda ()
	(live-mem-countall `((define insert-bintree
			       (lambda (x tree)
				 (if (not tree)
				     (cons x (cons #f (cons #f '())))
				     (if (< x (car tree))
					 (cons 
					  (car tree) 
					  (cons 
					   (insert-bintree x (car (cdr tree))) 
					   (cons (car (cdr (cdr tree))) '())))
					 (cons 
					  (car tree) 
					  (cons 
					   (car (cdr tree)) 
					   (cons 
					    (insert-bintree 
					     x
					     (car (cdr (cdr tree))))
					    '())))))))
			     (insert-bintree 5 ,tree)))))))

(define bound-insert-complete-bintree
  (lambda (h)
    (let ([tree (exp->cons-list (make-complete-bintree h))])
      (lambda ()
	(live-mem-countall `((define insert-bintree
			       (lambda (x tree)
				 (if (not tree)
				     (cons x (cons #f (cons #f '())))
				     (if (< x (car tree))
					 (cons 
					  (car tree) 
					  (cons 
					   (insert-bintree x (car (cdr tree))) 
					   (cons (car (cdr (cdr tree))) '())))
					 (cons 
					  (car tree) 
					  (cons 
					   (car (cdr tree)) 
					   (cons 
					    (insert-bintree 
					     x
					     (car (cdr (cdr tree))))
					    '())))))))
			     (insert-bintree 'unknown ,tree)))))))

(define make-complete-bintree
    (lambda (h)
      (make-tree-helper 1 (- (expt 2 (+ h 1)) 1))))

(define make-tree-helper
    (lambda (left right)
      (if (= left right)
	  (cons left (cons #f (cons #f '())))
	  (let ([mid (/ (+ left right) 2)])
	    (let ([left-tree (make-tree-helper left (- mid 1))]
		  [right-tree (make-tree-helper (+ mid 1) right)])
	      (cons mid (cons left-tree (cons right-tree '()))))))))

(define bintree-heights '(1 2 3 4 5 6 7 8 9 10))

(define compare-insert-bintree-fns
  (lambda (sizes)
    (compare-space-bound-fns space-insert-complete-bintree
			     bound-insert-complete-bintree
			     sizes)))

(define complete-ls-fns 
  '(rev rev-app ins-sort sel-sort mrg-sort bintree-ins lcs se bincoeff))

(define test-fns
  (lambda (ls-fns)
    (let ([rev-sizes '(10 20 50 100 200)]
	  [ins-sel-sizes '(1 2 3 4 5 6 7 8 9)]
	  [mrg-btins-sizes '(1 2 3 4 5 6 7 8 9 10)]
	  [lcs-sizes '((2 2) (4 4) (6 6) (8 8) (10 10) (12 12) (15 15))]
	  [se-sizes 
	   '((10 10) (20 20) (50 50) (100 100) (200 200))]
	  [bincoeff-sizes 
	   '((10 8) (20 18) (50 48) (100 98) (200 198))])
      (if (eq? 'help ls-fns)
	  (begin
	    (printf "usage: (test-fns 'help)~%")
	    (printf "       (test-fns '(ls-fn-names))~")
	    (printf "       ls-fn-names may contain the following symbols~%")
	    (printf "         rev, rev-app, ins-sort, sel-sort, mrg-sort~%")
	    (printf "         bintree-ins, lcs, se, bincoeff~%"))
	  (if (not (null? ls-fns))
	      (begin
		(case (car ls-fns)
		  [(rev)
		   (printf "reverse~%")
		   (compare-reverse-fns rev-sizes)]
		  [(rev-app)
		   (printf "~%reverse-append~%")
		   (compare-reverse-append-fns rev-sizes)]
		  [(ins-sort)
		   (printf "~%insertion-sort~%")
		   (compare-ins-sort-fns ins-sel-sizes)]
		  [(sel-sort)
		   (printf "~%selection-sort~%")
		   (compare-sel-sort-fns ins-sel-sizes)]
		  [(mrg-sort)
		   (printf "~%merge-sort~%")
		   (compare-merge-sort-fns mrg-btins-sizes)]
		  [(bintree-ins)
		   (printf "~%insert-bintree~%")
		   (compare-insert-bintree-fns mrg-btins-sizes)]
		  [(lcs)
		   (printf "~%lcs-incr~%")
		   (compare-lcs-incr-fns lcs-sizes)]
		  [(se)
		   (printf "~%string-edit-incr~%")
		   (compare-string-edit-incr-fns se-sizes)]
		  [(bincoeff)
		   (printf "~%bincoeff-incr~%")
		   (compare-bincoeff-incr-fns bincoeff-sizes)])
		(test-fns (cdr ls-fns))))))))
