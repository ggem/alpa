; Here is my version of the lattice benchmark. Modified slightly from a version
; that I got from Suresh Jagannathan and Andrew Wright. It runs under Scheme->C,
; Gambit-C, Bigloo, Chez, and Stalin. To run it under a specific
; <implementation>, select the stuff between ;;; begin <implementation> and
; ;;; end <implementation> and delete all of the other stuff between other
; ;;; begin and ;;; end comments.

;    Jeff (http://www.neci.nj.nec.com/homepages/qobi)
;
;;;; begin Scheme->C
;(define (panic s) (error 'panic s))
;;;; end Scheme->C
;;;; begin Gambit-C
;(define (panic s) (error s))
;;;; end Gambit-C
;;;; begin Bigloo
;(define (panic s) (error s 'panic 'panic))
;;;; end Bigloo
;;; begin Chez
;(define (panic s) (error 'panic s))
;;; end Chez


(define lattice
  (lambda ()
    (let ((l2
	    (make-lattice '(low high)
	      (lambda (lhs rhs)
		(if (eq? lhs 'low)
		  (if (eq? rhs 'low)
		    'equal
		    (if (eq? rhs 'high)
		      'less
		      'error))
		  (if (eq? lhs 'high)
		    (if (eq? rhs 'low)
		      'more
		      (if (eq? rhs 'high)
			'equal
			'error))
		    'error))))))
      (let ((l3 (maps l2 l2)))
	(let ((l4 (maps l3 l3)))
	  (cons (count-maps l2 l2)
	    (cons (count-maps l3 l3)
	      (cons (count-maps l2 l3)
		(cons (count-maps l3 l2)
		  (cons (count-maps l4 l4)
		    '()))))))))))



; Given a comparison routine that returns one of
;	less
;	more
;	equal
;	uncomparable
; return a new comparison routine that applies to sequences.
(define lexico
  (lambda (base)
    (letrec ([lex-first
	       (lambda (lhs rhs)
		 (if (null? lhs)
		   'equal
		   (let ([probe (base (car lhs) (car rhs))])
		     (if (if (eq? probe 'less) #t (eq? probe 'more))
		       (lex-fixed probe (cdr lhs) (cdr rhs))
		       (if (eq? probe 'equal)
			 (lex-first (cdr lhs) (cdr rhs))
			 (if (eq? probe 'uncomparable)
			   'uncomparable
			   'undefined))))))]
	     [lex-fixed
	       (lambda (fixed lhs rhs)
		 (letrec ([check
			    (lambda (lhs rhs)
			      (if (null? lhs)
				fixed
				(let ([probe (base (car lhs) (car rhs))])
				  (if (if (eq? probe 'equal) #t
					(eq? probe fixed))
				    (check (cdr lhs) (cdr rhs))
				    'uncomparable))))])
		   (check lhs rhs)))])
      lex-first)))

(define make-lattice
  (lambda (elem-list cmp-func)
    (cons elem-list cmp-func)))

(define lattice->elements car)

(define lattice->cmp cdr)

; Select elements of a list which pass some test.
(define zulu-select
  (lambda (test lst)
    (letrec ([select-a
	       (lambda (ac lst)
		 (if (null? lst)
		   (reverse ac)
		   (select-a
		     (let ((head (car lst)))
		       (if (test head)
			 (cons head ac)
			 ac))
		     (cdr lst))))])
      (select-a '() lst))))

(define reverse
  (letrec ((rev
	     (lambda (ls answ)
	       (if (null? ls)
		 answ
		 (rev (cdr ls) (cons (car ls) answ))))))
    (lambda (ls)
      (rev ls '()))))

(define append
  (lambda (ls1 ls2)
    (letrec ([append (lambda (ls1)
		       (if (null? ls1)
			 ls2
			 (cons (car ls1) (append (cdr ls1)))))])
      (append ls1))))

(define map
  (lambda (f ls)
    (letrec ([map (lambda (ls)
		    (if (null? ls)
		      '()
		      (cons (f (car ls)) (map (cdr ls)))))])
      (map ls))))

; Select elements of a list which pass some test and map a function
; over the result.  Note, only efficiency prevents this from being the
; composition of select and map.
(define select-map
  (lambda (test func lst)
    (letrec ([select-a
	       (lambda (ac lst)
		 (if (null? lst)
		   (reverse ac)
		   (select-a
		     (let ((head (car lst)))
		       (if (test head)
			 (cons (func head)
			   ac)
			 ac))
		     (cdr lst))))])
      (select-a '() lst))))

; This version of map-and tail-recurses on the last test.
(define map-and
  (lambda (proc lst)
    (if (null? lst)
      #t
      (letrec ((drudge
		 (lambda (lst)
		   (let ((rest (cdr lst)))
		     (if (null? rest)
		       (proc (car lst))
		       (if (proc (car lst))
			 (drudge rest)
			 #f))))))
	(drudge lst)))))

(define maps-1
  (lambda (source target pas new)
    (let ((scmp (lattice->cmp source))
	  (tcmp (lattice->cmp target)))
      (let ((less
	      (select-map
		(lambda (p)
		  (eq? 'less
		    (scmp (car p) new)))
		cdr
		pas))
	    (more
	      (select-map
		(lambda (p)
		  (eq? 'more
		    (scmp (car p) new)))
		cdr
		pas)))
	(zulu-select
	  (lambda (t)
	    (if (map-and
		  (lambda (t2)
		    (let ([tcmpt2 (tcmp t2 t)])
		      (if (eq? tcmpt2 'less) #t (eq? tcmpt2 'equal))))
		  less)
	      (map-and
		(lambda (t2)
		  (let ([tcmpt2 (tcmp t2 t)])
		    (if (eq? tcmpt2 'more) #t (eq? tcmpt2 'equal))))
		more)
	      #f))
	  (lattice->elements target))))))

(define maps-rest
  (lambda (source target pas rest to-1 to-collect)
    (if (null? rest)
      (to-1 pas)
      (let ((next (car rest))
	    (rest (cdr rest)))
	(to-collect
	  (map
	    (lambda (x)
	      (maps-rest source target
		(cons
		  (cons next x)
		  pas)
		rest
		to-1
		to-collect))
	    (maps-1 source target pas next)))))))

(define maps
  (lambda (source target)
    (make-lattice
      (maps-rest source
	target
	'()
	(lattice->elements source)
	(lambda (x) (cons (map cdr x) '()))
	(lambda (x)
	  (letrec ([loop (lambda (x)
			   (if (null? x)
			     '()
			     (append (car x) (loop (cdr x)))))])
	    (loop x))))
      (lexico (lattice->cmp target)))))

(define print-frequency 10000)

(define count-maps
  (lambda (source target)
    (maps-rest source
      target
      '()
      (lattice->elements source)
      (lambda (x) 1)
      (lambda (x)
	(letrec ([loop (lambda (x c)
			 (if (null? x)
			   c
			   (loop (cdr x) (+ (car x) c))))])
	  (loop x 0))))))


'(lattice)

;;; Chez	  50.14 seconds ~  0.84 mins
;;; Petite	 394.69 seconds ~  6.58 mins
;;; Gambit-C	1575.33 seconds ~ 26.26 mins
