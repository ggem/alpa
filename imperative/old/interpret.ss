;
; this is not really an interpreter, but a macro so that
; Chez Scheme will be in charge of doing the work.

(define-syntax alpa-imp
  (lambda (x)
    (syntax-case x (vars definitions define)
      ((_(vars global-var ...)
	 (definitions
	   (define (func arg0 ...) (vars local-var ...)
	     local-body0
	     local-body1 ...) ...)
	 global-body0
	 global-body1 ...)
       (syntax
	 (let ([global-var 0] ...)
	   (define (func arg0 ...)
	     (let ([local-var 0] ...)
	       local-body0 local-body1 ...))
	   ...
	   global-body0 global-body1 ...))))))

(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((_ test body0 body1 ...)
       (syntax
	 (let loop ()
	   (when test body0 body1 ... (loop))))))))
