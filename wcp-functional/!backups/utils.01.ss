;;;
;;;
;;;

(load "synforms.ss")

;;;
;;; syntax makers
;;;


(define tree-bind
  (lambda args
    (if (or (null? args) (null? (cdr args)))
      (error 'tree-bind "incorrect number of arguments ~a" (length args))
      (let ([args (reverse args)])
	(let ([body (car args)])
	  (let loop ([args (cdr args)] [tmps '()] [vtps '()])
	    (syncase args
	      [`() `(let ,tmps (let ,vtps ,(if (procedure? body) (body) body)))]
	      [`(,exp ,tmp ,p ,t ,v . ,rest)
	       (loop rest `((,tmp ,exp) . ,tmps)
		 `((,v (car ,tmp)) (,t (cadr ,tmp)) (,p (caddr ,tmp))
		   . ,vtps))])))))))


(define make-tree-bindings
  (lambda (vals Texps k)
    (let ([c (count!)]
	  [n (length Texps)])
      (make-variables `((v- t- p- tmp-) (,c) (-) ,(reverse (iota n)))
	(lambda vars
	  (vars->fields vars
	    (lambda (vs ts ps tmps)
	      (let ([vs (if vals vals vs)])
		(apply tree-bind
		  (append
		    (apply append (map list vs ts ps tmps Texps))
		    (list
		      (lambda ()
			(k vs ts ps)))))))))))))


;;;
;;;
;;;

(define merge-paths
  (letrec ([merge-2-paths
	     (lambda (path1 path2)
	       (cond
		 [(null? path1) path2]
		 [(null? path2) path1]
		 [else (cons (car path1)
			 (merge-2-paths (cdr path1) path2))]))])
    (letrec ([merge-paths
	       (lambda (paths)
		 (if (null? (cdr paths))
		   (car paths)
		   (merge-2-paths (car paths)
		     (merge-paths (cdr paths)))))])
      (lambda paths
	(merge-paths paths)))))

(define merge-paths list)

(define flatten-path
  (letrec ([flatten (lambda (path acc)
		      (cond
			[(null? path) acc]
			[(null? (car path))
			 (flatten (cdr path) acc)]
			[(symbol? (caar path))
			 (cons (car path) (flatten (cdr path) acc))]
			[else
			 (flatten (car path) (flatten (cdr path) acc))]))])
    (lambda (path)
      (flatten path '()))))

(define flatten-path
  (letrec ([flatten (lambda (path acc)
		      (syncase path
			[`() acc]
			[`(() . ,rest) (flatten rest acc)]
			[`(,prim . ,args)
			 (guard (symbol? prim))
			 (cons path acc)]
			[`((,prim . ,args) . ,rest)
			 (guard (symbol? prim))
			 (cons (car path) (flatten rest acc))]
			[`(,path1 . ,rest)
			 (flatten path1 (flatten rest acc))]))])
    (lambda (path)
      (flatten path '()))))

			

(define make-unknown
  (let ([count -1])
    (lambda ()
      (set! count (+ count 1))
      (cons 'unknown count))))
		   

(define unknown?
  (lambda (obj)
    (and (pair? obj)
	 (eq? (car obj) 'unknown)
	 (number? (cdr obj)))))

(define make-unknown-list
  (letrec ([make-list
	     (lambda (n)
	       (if (zero? n)
		 '()
		 (cons (make-unknown) (make-list (- n 1)))))])
    make-list))

(define make-variable
  (let ([obj->str (lambda (obj)
		    (cond
		      [(null? obj) ""]
		      [(string? obj) obj]
		      [(number? obj) (number->string obj)]
		      [(symbol? obj) (symbol->string obj)]
		      [else (error 'make-variable "unknown type: ~a" obj)]))])
    (letrec ([make-string (lambda (objs str)
			    (if (null? objs)
			      str
			      (make-string (cdr objs)
				(string-append str (obj->str (car objs))))))])
      (lambda args
	(string->symbol (make-string args ""))))))

(define make-variables
  (lambda (lists k)
    (apply k
      (map (lambda (items) (apply make-variable items))
	(cross-product lists)))))
      
(define cross-product
  (letrec ([cross-product-2sets
	     (lambda (set1 set2)
	       (let loop1 ([set1 set1] [answ '()])
		 (if (null? set1)
		   (reverse answ)
		   (let loop2 ([set2 set2] [answ answ])
		     (if (null? set2)
		       (loop1 (cdr set1) answ)
		       (loop2 (cdr set2)
			 `((,(car set1) . ,(car set2)) . ,answ)))))))])
    (letrec ([cross-product
	       (lambda (set0 sets)
		 (if (null? sets)
		   (map list set0)
		   (cross-product-2sets set0
		     (cross-product (car sets) (cdr sets)))))])
      (lambda (sets)
	(if (null? sets)
	  '()
	  (cross-product (car sets) (cdr sets)))))))

(define vars->fields
  (letrec ([split (lambda (ls n0 acc1)
		    (if (null? ls)
		      acc1
		      (let loop ([ls ls] [n n0] [acc2 '()])
			(if (zero? n)
			  (split ls n0 (cons acc2 acc1))
			  (loop (cdr ls) (- n 1) (cons (car ls) acc2))))))])
    (lambda (vars k)
      (let ([len (/ (length vars) 4)])
	(let ([lsts (split vars len '())])
	  (k (cadddr lsts) (caddr lsts) (cadr lsts) (car lsts)))))))


;;;
;;;
;;;

(define count!
  (let ([counter -1])
    (lambda ()
      (set! counter (+ 1 counter))
      counter)))

(define make-path
  (let ([make-unquote
	  (lambda (x)
	    `(,'unquote ,x))])
    (lambda (true? exp)
      (syncase exp
	[`(,prim . ,args)
	 (guard (primitive? prim))
	 (if true?
	   `(,'quasiquote (,prim . ,(map make-unquote args)))
	   `(,'quasiquote (not (,prim . ,(map make-unquote args)))))]
	[else '()]))))

;;;
;;;
;;;

(define replace!
  (lambda (old new obj)
    (if (atom? obj)
      (if (eq? obj old) new obj)
      (letrec ([rep! (lambda (obj)
		       (if (pair? (cdr obj))
			 (rep! (cdr obj)))
		       (if (pair? (car obj))
			 (rep! (car obj))
			 (if (eq? (car obj) old)
			   (set-car! obj new))))])
	(rep! obj)
	obj))))

;;;
;;; small tests
;;;



(define test01
  '((define (min a b c)
      (if (< a b)
	(if (< a c)
	  a
	  (if (< b c)
	    b
	    c))
	(if (< b c)
	  b
	  c)))
    '(min unknown unknown unknown)))

(define test02
  '((define (union set1 set2)
      (if (null? set1)
	set2
	(let ([rr (union (cdr set1) set2)])
	  (if (member? (car set1) set2)
	    rr
	    (cons (car set1) rr)))))

    (define (member? item ls)
      (if (null? ls)
	#f
	(if (eq? item (car ls))
	  #t
	  (member? item (cdr ls)))))

    '(union (make-list size) (make-list size))))

(define test
  (let ([null-condition?
	  (lambda (x)
	    (syncase x
	      [`(null? . ,x) #t]
	      [`(not (null? . ,x)) #t]
	      [else #f]))])
    (lambda ()
      (eval (scheme-wrap (wcp-file "!tests/insertsort.ss")))
      (pretty-print (time-insertsort 8))
      (time (time-insertsort 500))
      (void))))

;;;
;;; Times (taken from ~alpa/closure/!timing/measured-prim.txt)
;;;

(define Tcar 87.45)
(define Tcdr 60.95)
(define Tcons 95.8)
(define Tnull? 73.3)
(define Teq? 66.65)
(define T+ 66.55)
(define T- 65.15)
(define T* 509.45)
(define Tquotient 300.00)
(define T> 80.4)
(define T< 75.25)
(define T= 75.55)
(define Tc 0.25)
(define Tvar 2.1)
(define Tif 3.7)
(define Tlet 0.45)
(define Tcall 75.175)

;;  (define Tcar	1)
;;  (define Tcdr	1)
;;  (define Tnull?	1)
;;  (define Tcons	10)
;;  (define Teq?	100)
;;  (define T>	100)
;;  (define T<	100)
;;  (define T=	100)
;;  (define T+	1000)
;;  (define T-	1000)
;;  (define T*	1000)
;;  (define Tc	10000)
;;  (define Tvar	100000)
;;  (define Tif	1000000)
;;  (define Tlet	10000000)
;;  (define Tcall	100000000)
