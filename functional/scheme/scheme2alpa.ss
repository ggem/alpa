;;;
;;;
;;;


(define (scheme->alpa filename)
  (let ([ip (open-input-file filename)])
    (do ([exp (read ip) (read ip)]
	 [answ "" (string-append answ (exp->alpa exp 0))])
      [(eof-object? exp)
       (close-input-port ip)
       (display answ)])))

(define (exp->alpa exp level)
  (cond
    [(atom? exp) (atom->alpa exp)]
    [(pair? (app->operator exp))
     (error 'scheme->alpa "sorry, I'm not that good")]
    [else
      (let ([new-level (+ level 1)])
	(case (app->operator exp)
	  [(define)
	   (string-append
	     (symbol->string (definition->name exp))
	     (exps->alpa (definition->args exp) new-level)
	     " ="
	     (my-newline new-level)
	     (exp->alpa (definition->body exp) new-level)
	     (string #\; #\newline #\newline))]
	  [(if)
	   (string-append
	     "if "
	     (exp->alpa (if->test exp) new-level)
	     " then"
	     (my-newline new-level)
	     (exp->alpa (if->then exp) new-level)
	     (my-newline level)
	     "else"
	     (my-newline new-level)
	     (exp->alpa (if->else exp) new-level))]
	  [(let)
	   (string-append
	     "let "
	     (symbol->string (binding->var exp))
	     " = "
	     (exp->alpa (binding->val exp) new-level)
	     (my-newline level)
	     "in"
	     (my-newline new-level)
	     (exp->alpa (binding->body exp) new-level)
	     (my-newline level)
	     "end")]
	  [(not)
	   (string-append
	     "(!"
	     (exp->alpa (car (app->oparands exp)) new-level)
	     ")")]
	  [(+ - * / >= = and or)
	   (string-append
	     "("
	     (exp->alpa (car (app->operands exp)) new-level)
	     (cdr (assq (app->operator exp)
		    '((+ . " + ")
		      (- . " - ")
		      (* . " * ")
		      (/ . " / ")
		      (>= . " >= ")
		      (= . " == ")
		      ( and . " & ")
		      ( or  . " | "))))
	     (exp->alpa (cadr (app->operands exp)) new-level)
	     ")")]
	  [(quote)
	   (let ([datum (cadr exp)])
	     (cond
	       [(null? datum) "nil"]
	       [(symbol? datum) (format "'~a" datum)]
	       [(number? datum) (number->string datum)]
	       [(pair? datum) (format "\"~a\"" datum)]
	       [else (error 'scheme->alpa "unknown quoted datum ~s" datum)]))]
	  [else
	    (string-append
	      (exp->alpa (app->operator exp) level)
	      (exps->alpa (app->operands exp) new-level))]))]))


(define (exps->alpa exps level)
  (if (null? exps)
    "()"
    (do ([exps (cdr exps) (cdr exps)]
	 [answ (string-append "(" (exp->alpa (car exps) level))
	   (string-append answ ", " (exp->alpa (car exps) level))])
      [(null? exps) (string-append answ ")")])))

(define (atom->alpa atom)
  (cond
    [(boolean? atom) (if atom "true" "false")]
    [(number? atom) (number->string atom)]
    [(null? atom) "nil"]
    [(symbol? atom) (symbol->string atom)]
    [else (error 'scheme->alpa "unknown atom ~s" atom)]))

(define (my-newline level)
  (list->string
    (cons #\newline
      (make-list (* level 2) #\space))))

(define (cadaadr x) (cadar (cadr x)))

(define definition->name caadr)
(define definition->args cdadr)
(define definition->body caddr)
(define if->test	cadr)
(define if->then	caddr)
(define if->else	cadddr)
(define app->operator	car)
(define app->operands	cdr)
(define binding->var	caaadr)
(define binding->val	cadaadr)
(define binding->body	caddr)

