;;
;;
;; ack.ss

(optimize-level 3)

(define cost-ack
  (let ()
    (define ack
      (letrec
	([ack
	   (cons
	     (lambda (m)
	       (if (= m 0)
		 (cons
		   (lambda (n) (+ n 1))
		   (lambda (n) #18(0 0 0 0 0 1 0 0 0 0 0 1 1 0)))
		 (let ([ack_m-1 ((car ack) (- m 1))])
		   (letrec
		     ([ack_m
			(cons
			  (lambda (n)
			    (if (= n 0)
			      ((car ack_m-1) 1)
			      ((car ack_m-1)
			       ((car ack_m) (- n 1)))))
			  (lambda (n)
			    (if (= n 0)
			      (c+ #18(0 0 0 0 0 0 0 0 0 0 1 2 2 1 0 0 1 0)
				((cdr ack_m-1) 1))
			      (c+ ((cdr ack_m-1)
				   ((car ack_m) (- n 1)))
				((cdr ack_m) (- n 1))
				#18(0 0 0 0 0 0 1 0 0 0 1 2 4 1 0 0 2 0)))))])
		     ack_m))))
	     (lambda (m)
	       (if (= m 0)
		 #18(0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 1)
		 (let ([ack_m-1 ((car ack) (- m 1))])
		   (c+ (letrec
			 ([ack_m
			    (cons
			      (lambda (n)
				(if (= n 0)
				  ((car ack_m-1) 1)
				  ((car ack_m-1)
				   ((car ack_m) (- n 1)))))
			      (lambda (n)
				(if (= n 0)
				  (c+ #18(0 0 0 0 0 0 0 0 0 0 1 2 2 1 0 0 1 0)
				    ((cdr ack_m-1) 1))
				  (c+ ((cdr ack_m-1)
				       ((car ack_m)
					(- n 1)))
				    ((cdr ack_m) (- n 1))
				    #18(0 0 0 0 0 0 1 0 0 0 1 2 4 1 0 0 2 0)))))])
			 #18(0 0 0 0 0 0 1 0 0 0 1 2 4 1))
		     ((cdr ack) (- m 1)))))))])
        (cons
          (lambda (m n) ((car ((car ack) m)) n))
          (lambda (m n)
            (c+ ((cdr ack) m)
	      ((cdr ((car ack) m)) n)
	      #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 2 0))))))
    (lambda (size0 size1)
      (let ()
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 1 0)
	    ((cdr ack) size0 size1)))))))

(define cost-ackermann
  (let ()
    (define ackermann
      (cons
        (lambda (m n)
          (if (= m 0)
	    (+ n 1)
	    (if (= n 0)
	      ((car ackermann) (- m 1) 1)
	      ((car ackermann)
	       (- m 1)
	       ((car ackermann) m (- n 1))))))
        (lambda (m n)
          (if (= m 0)
	    #18(0 0 0 0 0 1 0 0 0 0 1 2 2 1 0)
	    (if (= n 0)
	      (c+ #18(0 0 0 0 0 0 1 0 0 0 2 4 4 2 0 0 1 0)
		((cdr ackermann) (- m 1) 1))
	      (c+ ((cdr ackermann) m (- n 1))
		((cdr ackermann)
		 (- m 1)
		 ((car ackermann) m (- n 1)))
		#18(0 0 0 0 0 0 2 0 0 0 2 4 7 2 0 0 2 0)))))))
    (lambda (size0 size1)
      (let ()
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 1 0)
	    ((cdr ackermann) size0 size1)))))))

(define cost-cpstak
  (let ()
    (define cpstak
      (cons
        (lambda (x y z)
          (letrec ([tak
		     (cons
		       (lambda (x y z k)
			 (if (< y x)
			   ((car tak)
			    (- x 1)
			    y
			    z
			    (cons
			      (lambda (v1)
				((car tak)
				 (- y 1)
				 z
				 x
				 (cons
				   (lambda (v2)
				     ((car tak)
				      (- z 1)
				      x
				      y
				      (cons
					(lambda (v3)
					  ((car tak) v1 v2 v3 k))
					(lambda (v3)
					  (c+ #18(0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   5
                                                   0
                                                   0
                                                   0
                                                   1
                                                   0)
					    ((cdr tak) v1 v2 v3 k))))))
				   (lambda (v2)
				     (c+ #18(0
                                              0
                                              0
                                              0
                                              0
                                              0
                                              1
                                              0
                                              0
                                              0
                                              0
                                              1
                                              4
                                              0
                                              0
                                              0
                                              1)
				       ((cdr tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))))))
			      (lambda (v1)
				(c+ #18(0 0 0 0 0 0 1 0 0 0 0 1 4 0 0 0 1)
				  ((cdr tak)
				   (- y 1)
				   z
				   x
				   (cons
				     (lambda (v2)
				       ((car tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))
				     (lambda (v2)
				       (c+ #18(0
						0
						0
						0
						0
						0
						1
						0
						0
						0
						0
						1
						4
						0
						0
						0
						1)
					 ((cdr tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k)))))))))))))
			   ((car k) z)))
		       (lambda (x y z k)
			 (if (< y x)
			   (c+ #18(0 0 0 0 0 0 1 0 0 1 0 1 6 1 0 0 1)
			     ((cdr tak)
			      (- x 1)
			      y
			      z
			      (cons
				(lambda (v1)
				  ((car tak)
				   (- y 1)
				   z
				   x
				   (cons
				     (lambda (v2)
				       ((car tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))
				     (lambda (v2)
				       (c+ #18(0
						0
						0
						0
						0
						0
						1
						0
						0
						0
						0
						1
						4
						0
						0
						0
						1)
					 ((cdr tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k))))))))))
				(lambda (v1)
				  (c+ #18(0
					   0
					   0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   1
					   4
					   0
					   0
					   0
					   1)
				    ((cdr tak)
				     (- y 1)
				     z
				     x
				     (cons
				       (lambda (v2)
					 ((car tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k))))))
				       (lambda (v2)
					 (c+ #18(0
						  0
						  0
						  0
						  0
						  0
						  1
						  0
						  0
						  0
						  0
						  1
						  4
						  0
						  0
						  0
						  1)
					   ((cdr tak)
					    (- z 1)
					    x
					    y
					    (cons
					      (lambda (v3)
						((car tak)
						 v1
						 v2
						 v3
						 k))
					      (lambda (v3)
						(c+ #18(0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 5
							 0
							 0
							 0
							 1
							 0)
						  ((cdr tak)
						   v1
						   v2
						   v3
						   k))))))))))))))
			   (c+ #18(0 0 0 0 0 0 0 0 0 1 0 0 4 1 0 0 1 0)
			     ((cdr k) z)))))])
            ((car tak)
             x
             y
             z
             (cons
               (lambda (a) a)
               (lambda (a) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0))))))
        (lambda (x y z)
          (letrec ([tak
		     (cons
		       (lambda (x y z k)
			 (if (< y x)
			   ((car tak)
			    (- x 1)
			    y
			    z
			    (cons
			      (lambda (v1)
				((car tak)
				 (- y 1)
				 z
				 x
				 (cons
				   (lambda (v2)
				     ((car tak)
				      (- z 1)
				      x
				      y
				      (cons
					(lambda (v3)
					  ((car tak) v1 v2 v3 k))
					(lambda (v3)
					  (c+ #18(0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   0
                                                   5
                                                   0
                                                   0
                                                   0
                                                   1
                                                   0)
					    ((cdr tak) v1 v2 v3 k))))))
				   (lambda (v2)
				     (c+ #18(0
                                              0
                                              0
                                              0
                                              0
                                              0
                                              1
                                              0
                                              0
                                              0
                                              0
                                              1
                                              4
                                              0
                                              0
                                              0
                                              1)
				       ((cdr tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))))))
			      (lambda (v1)
				(c+ #18(0 0 0 0 0 0 1 0 0 0 0 1 4 0 0 0 1)
				  ((cdr tak)
				   (- y 1)
				   z
				   x
				   (cons
				     (lambda (v2)
				       ((car tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))
				     (lambda (v2)
				       (c+ #18(0
						0
						0
						0
						0
						0
						1
						0
						0
						0
						0
						1
						4
						0
						0
						0
						1)
					 ((cdr tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k)))))))))))))
			   ((car k) z)))
		       (lambda (x y z k)
			 (if (< y x)
			   (c+ #18(0 0 0 0 0 0 1 0 0 1 0 1 6 1 0 0 1)
			     ((cdr tak)
			      (- x 1)
			      y
			      z
			      (cons
				(lambda (v1)
				  ((car tak)
				   (- y 1)
				   z
				   x
				   (cons
				     (lambda (v2)
				       ((car tak)
					(- z 1)
					x
					y
					(cons
					  (lambda (v3)
					    ((car tak) v1 v2 v3 k))
					  (lambda (v3)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     0
						     5
						     0
						     0
						     0
						     1
						     0)
					      ((cdr tak)
					       v1
					       v2
					       v3
					       k))))))
				     (lambda (v2)
				       (c+ #18(0
						0
						0
						0
						0
						0
						1
						0
						0
						0
						0
						1
						4
						0
						0
						0
						1)
					 ((cdr tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k))))))))))
				(lambda (v1)
				  (c+ #18(0
					   0
					   0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   1
					   4
					   0
					   0
					   0
					   1)
				    ((cdr tak)
				     (- y 1)
				     z
				     x
				     (cons
				       (lambda (v2)
					 ((car tak)
					  (- z 1)
					  x
					  y
					  (cons
					    (lambda (v3)
					      ((car tak)
					       v1
					       v2
					       v3
					       k))
					    (lambda (v3)
					      (c+ #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       5
						       0
						       0
						       0
						       1
						       0)
						((cdr tak)
						 v1
						 v2
						 v3
						 k))))))
				       (lambda (v2)
					 (c+ #18(0
						  0
						  0
						  0
						  0
						  0
						  1
						  0
						  0
						  0
						  0
						  1
						  4
						  0
						  0
						  0
						  1)
					   ((cdr tak)
					    (- z 1)
					    x
					    y
					    (cons
					      (lambda (v3)
						((car tak)
						 v1
						 v2
						 v3
						 k))
					      (lambda (v3)
						(c+ #18(0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 0
							 5
							 0
							 0
							 0
							 1
							 0)
						  ((cdr tak)
						   v1
						   v2
						   v3
						   k))))))))))))))
			   (c+ #18(0 0 0 0 0 0 0 0 0 1 0 0 4 1 0 0 1 0)
			     ((cdr k) z)))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 1 1 2)
	      ((cdr tak)
	       x
	       y
	       z
	       (cons
		 (lambda (a) a)
		 (lambda (a) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0)))))))))
    (lambda (size0 size1 size2)
      (let ()
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 1 0)
	    ((cdr cpstak) size0 size1 size2)))))))

(define cost-fix
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define fix
      (let ([z
	      (closure
		(lambda (p)
		  (closure
		    (lambda (u)
		      (closure
			(lambda (t)
			  (closure
			    (lambda (t)
			      (closure
				(lambda (i)
				  (closure
				    (lambda (n)
				      (closure
					(lambda (g)
					  (closure
					    (lambda (s)
					      (closure
						(lambda (c)
						  (closure
						    (lambda (h)
						      (closure
							(lambda (e)
							  (closure
							    (lambda (m)
							      (closure
								(lambda (e)
								  (closure
								    (lambda (t)
								      (closure
									(lambda (o)
									  (closure
									    (lambda (w)
									      (closure
										(lambda (o)
										  (closure
										    (lambda (r)
										      (closure
											(lambda (k)
											  (closure
											    (lambda (!)
											      ((value
												 !)
											       (closure
												 (lambda (break)
												   ((value
												      ((value
													 ((value
													    ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 ((value
																			    ((value
																			       ((value
																				  w)
																				o))
																			     r))
																			  k))
																		       w))
																		    o))
																		 r))
																	      k))
																	   w))
																	o))
																     r))
																  k))
															       w))
															    o))
															 r))
														      k))
														   w))
														o))
													     r))
													  k))
												       !))
												    break))
												 (lambda (break)
												   (c+ ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      ((value
																				 ((value
																				    ((value
																				       w)
																				     o))
																				  r))
																			       k))
																			    w))
																			 o))
																		      r))
																		   k))
																		w))
																	     o))
																	  r))
																       k))
																    w))
																 o))
															      r))
															   k))
															w))
														     o))
														  r))
													       k))
													!)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 ((value
																			    ((value
																			       w)
																			     o))
																			  r))
																		       k))
																		    w))
																		 o))
																	      r))
																	   k))
																	w))
																     o))
																  r))
															       k))
															    w))
															 o))
														      r))
														   k))
														w))
													     o))
												      r)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 w)
																		       o))
																		    r))
																		 k))
																	      w))
																	   o))
																	r))
																     k))
																  w))
															       o))
															    r))
															 k))
														      w))
														   o))
														r))
													     k))
												      w)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   w)
																		 o))
																	      r))
																	   k))
																	w))
																     o))
																  r))
															       k))
															    w))
															 o))
														      r))
														   k))
														w))
													     o))
												      r)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     w)
																	   o))
																	r))
																     k))
																  w))
															       o))
															    r))
															 k))
														      w))
														   o))
														r))
													     k))
												      w)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       w)
																     o))
																  r))
															       k))
															    w))
															 o))
														      r))
														   k))
														w))
													     o))
												      r)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 w)
															       o))
															    r))
															 k))
														      w))
														   o))
														r))
													     k))
												      w)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   w)
															 o))
														      r))
														   k))
														w))
													     o))
												      r)
												     ((cost ((value
													       ((value
														  ((value
														     w)
														   o))
														r))
													     k))
												      w)
												     ((cost ((value
													       w)
													     o))
												      r)
												     ((cost w)
												      o)
												     ((cost ((value
													       ((value
														  w)
														o))
													     r))
												      k)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															w)
														      o))
														   r))
														k))
													     w))
												      o)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      w)
															    o))
															 r))
														      k))
														   w))
														o))
													     r))
												      k)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    w)
																  o))
															       r))
															    k))
															 w))
														      o))
														   r))
														k))
													     w))
												      o)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  w)
																	o))
																     r))
																  k))
															       w))
															    o))
															 r))
														      k))
														   w))
														o))
													     r))
												      k)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		w)
																	      o))
																	   r))
																	k))
																     w))
																  o))
															       r))
															    k))
															 w))
														      o))
														   r))
														k))
													     w))
												      o)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      w)
																		    o))
																		 r))
																	      k))
																	   w))
																	o))
																     r))
																  k))
															       w))
															    o))
															 r))
														      k))
														   w))
														o))
													     r))
												      k)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 ((value
																			    w)
																			  o))
																		       r))
																		    k))
																		 w))
																	      o))
																	   r))
																	k))
																     w))
																  o))
															       r))
															    k))
															 w))
														      o))
														   r))
														k))
													     w))
												      o)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 ((value
																			    ((value
																			       ((value
																				  w)
																				o))
																			     r))
																			  k))
																		       w))
																		    o))
																		 r))
																	      k))
																	   w))
																	o))
																     r))
																  k))
															       w))
															    o))
															 r))
														      k))
														   w))
														o))
													     r))
												      k)
												     ((cost ((value
													       ((value
														  ((value
														     ((value
															((value
															   ((value
															      ((value
																 ((value
																    ((value
																       ((value
																	  ((value
																	     ((value
																		((value
																		   ((value
																		      ((value
																			 ((value
																			    ((value
																			       ((value
																				  ((value
																				     ((value
																					w)
																				      o))
																				   r))
																				k))
																			     w))
																			  o))
																		       r))
																		    k))
																		 w))
																	      o))
																	   r))
																	k))
																     w))
																  o))
															       r))
															    k))
															 w))
														      o))
														   r))
														k))
													     !))
												      break)
												     #18(0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          22
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          21
                                                                                                          0))))))
											    (lambda (!)
											      (c+ #18(0
												       0
												       0
												       0
												       0
												       0
												       0
												       0
												       0
												       0
												       0
												       0
												       1
												       0
												       0
												       0
												       1)
												((cost !)
												 (closure
												   (lambda (break)
												     ((value
													((value
													   ((value
													      ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      ((value
																				 ((value
																				    w)
																				  o))
																			       r))
																			    k))
																			 w))
																		      o))
																		   r))
																		k))
																	     w))
																	  o))
																       r))
																    k))
																 w))
															      o))
															   r))
															k))
														     w))
														  o))
													       r))
													    k))
													 !))
												      break))
												   (lambda (break)
												     (c+ ((cost ((value
														   ((value
														      ((value
															 ((value
															    ((value
															       ((value
																  ((value
																     ((value
																	((value
																	   ((value
																	      ((value
																		 ((value
																		    ((value
																		       ((value
																			  ((value
																			     ((value
																				((value
																				   ((value
																				      ((value
																					 w)
																				       o))
																				    r))
																				 k))
																			      w))
																			   o))
																			r))
																		     k))
																		  w))
																	       o))
																	    r))
																	 k))
																      w))
																   o))
																r))
															     k))
															  w))
														       o))
														    r))
														 k))
													  !)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      ((value
																				 w)
																			       o))
																			    r))
																			 k))
																		      w))
																		   o))
																		r))
																	     k))
																	  w))
																       o))
																    r))
																 k))
															      w))
															   o))
															r))
														     k))
														  w))
													       o))
													r)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   w)
																			 o))
																		      r))
																		   k))
																		w))
																	     o))
																	  r))
																       k))
																    w))
																 o))
															      r))
															   k))
															w))
														     o))
														  r))
													       k))
													w)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     w)
																		   o))
																		r))
																	     k))
																	  w))
																       o))
																    r))
																 k))
															      w))
															   o))
															r))
														     k))
														  w))
													       o))
													r)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       w)
																	     o))
																	  r))
																       k))
																    w))
																 o))
															      r))
															   k))
															w))
														     o))
														  r))
													       k))
													w)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 w)
																       o))
																    r))
																 k))
															      w))
															   o))
															r))
														     k))
														  w))
													       o))
													r)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   w)
																 o))
															      r))
															   k))
															w))
														     o))
														  r))
													       k))
													w)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     w)
															   o))
															r))
														     k))
														  w))
													       o))
													r)
												       ((cost ((value
														 ((value
														    ((value
														       w)
														     o))
														  r))
													       k))
													w)
												       ((cost ((value
														 w)
													       o))
													r)
												       ((cost w)
													o)
												       ((cost ((value
														 ((value
														    w)
														  o))
													       r))
													k)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  w)
															o))
														     r))
														  k))
													       w))
													o)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																w)
															      o))
															   r))
															k))
														     w))
														  o))
													       r))
													k)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      w)
																    o))
																 r))
															      k))
															   w))
															o))
														     r))
														  k))
													       w))
													o)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    w)
																	  o))
																       r))
																    k))
																 w))
															      o))
															   r))
															k))
														     w))
														  o))
													       r))
													k)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  w)
																		o))
																	     r))
																	  k))
																       w))
																    o))
																 r))
															      k))
															   w))
															o))
														     r))
														  k))
													       w))
													o)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			w)
																		      o))
																		   r))
																		k))
																	     w))
																	  o))
																       r))
																    k))
																 w))
															      o))
															   r))
															k))
														     w))
														  o))
													       r))
													k)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      w)
																			    o))
																			 r))
																		      k))
																		   w))
																		o))
																	     r))
																	  k))
																       w))
																    o))
																 r))
															      k))
															   w))
															o))
														     r))
														  k))
													       w))
													o)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      ((value
																				 ((value
																				    w)
																				  o))
																			       r))
																			    k))
																			 w))
																		      o))
																		   r))
																		k))
																	     w))
																	  o))
																       r))
																    k))
																 w))
															      o))
															   r))
															k))
														     w))
														  o))
													       r))
													k)
												       ((cost ((value
														 ((value
														    ((value
														       ((value
															  ((value
															     ((value
																((value
																   ((value
																      ((value
																	 ((value
																	    ((value
																	       ((value
																		  ((value
																		     ((value
																			((value
																			   ((value
																			      ((value
																				 ((value
																				    ((value
																				       ((value
																					  w)
																					o))
																				     r))
																				  k))
																			       w))
																			    o))
																			 r))
																		      k))
																		   w))
																		o))
																	     r))
																	  k))
																       w))
																    o))
																 r))
															      k))
															   w))
															o))
														     r))
														  k))
													       !))
													break)
												       #18(0
													    0
													    0
													    0
													    0
													    0
													    0
													    0
													    0
													    0
													    0
													    0
													    22
													    0
													    0
													    0
													    21
													    0)))))))))
											(lambda (k)
											  #18(0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       0
											       1))))
										    (lambda (r)
										      #18(0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   0
											   1))))
										(lambda (o)
										  #18(0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       0
										       1))))
									    (lambda (w)
									      #18(0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   0
										   1))))
									(lambda (o)
									  #18(0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       0
									       1))))
								    (lambda (t)
								      #18(0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   0
									   1))))
								(lambda (e)
								  #18(0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       0
								       1))))
							    (lambda (m)
							      #18(0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   0
								   1))))
							(lambda (e)
							  #18(0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       0
							       1))))
						    (lambda (h)
						      #18(0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   0
							   1))))
						(lambda (c)
						  #18(0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       0
						       1))))
					    (lambda (s)
					      #18(0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   0
						   1))))
					(lambda (g)
					  #18(0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       1))))
				    (lambda (n)
				      #18(0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   0
					   1))))
				(lambda (i)
				  #18(0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       0
				       1))))
			    (lambda (t)
			      #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
			(lambda (t)
			  #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
		    (lambda (u) #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
		(lambda (p) #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))])
        (let ([z z])
          ((value
             ((value
                ((value
                   ((value
                      ((value
                         ((value
                            ((value
                               ((value
                                  ((value
                                     ((value
                                        ((value
                                           ((value
                                              ((value
                                                 ((value
                                                    ((value
                                                       ((value
                                                          ((value
                                                             ((value
                                                                ((value z)
                                                                 z))
                                                              z))
                                                           z))
                                                        z))
                                                     z))
                                                  z))
                                               z))
                                            z))
                                         z))
                                      z))
                                   z))
                                z))
                             z))
                          z))
                       z))
                    z))
                 z))
              z))
           z))))
    (define sum
      (closure
        (lambda (f)
          (closure
            (lambda (n) (if (= n 0) 1 (+ n ((value f) (- n 1)))))
            (lambda (n)
              (if (= n 0)
		#18(0 0 0 0 0 0 0 0 0 0 1 2 1 1 0)
		(c+ #18(0 0 0 0 0 1 1 0 0 0 1 2 4 1 0 0 1 0)
		  ((cost f) (- n 1)))))))
        (lambda (f) #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
    (lambda (size0)
      (let ()
        (cost-vector->exp
          (c+ ((cost fix) sum)
	    ((cost ((value fix) sum)) size0)
	    #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 2 0)))))))

(define cost-index
  (let ()
    (define null?^
      (lambda (x) (if (eq? x 'unknown) 'unknown (null? x))))
    (define car^
      (lambda (x) (if (eq? x 'unknown) 'unknown (car x))))
    (define cdr^
      (lambda (x) (if (eq? x 'unknown) 'unknown (cdr x))))
    (define eq?^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (eq? x y))))
    (define +^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (+ x y))))
    (define -^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (- x y))))
    (define *^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (* x y))))
    (define >^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (> x y))))
    (define <^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (< x y))))
    (define =^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (= x y))))
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define index
      (cons
        (lambda (item ls)
          (letrec ([index-cps
		     (cons
		       (lambda (ls k)
			 (let ([test (null?^ ls)])
			   (if (eq? test 'unknown)
			     (lub -1
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (lub ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))
				   (if test
				     ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v))))))))))
			     (if test
			       -1
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (lub ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))
				   (if test
				     ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))))))))
		       (lambda (ls k)
			 (let ([test (null?^ ls)])
			   (if (eq? test 'unknown)
			     (cmax #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (cmax (c+ #18(1
						  0
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  4
						  2
						  0
						  0
						  1
						  0)
					   ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v))))))))
				   (if test
				     (c+ #18(1
					      0
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      1
					      4
					      2
					      0
					      0
					      1
					      0)
				       ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v)))))))))))
			     (if test
			       #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (cmax (c+ #18(1
						  0
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  4
						  2
						  0
						  0
						  1
						  0)
					   ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1 v))))))))
				   (if test
				     (c+ #18(1
					      0
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      1
					      4
					      2
					      0
					      0
					      1
					      0)
				       ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k) (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v)))))))))))))))])
            ((car index-cps)
             ls
             (cons
               (lambda (x) x)
               (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0))))))
        (lambda (item ls)
          (letrec ([index-cps
		     (cons
		       (lambda (ls k)
			 (let ([test (null?^ ls)])
			   (if (eq? test 'unknown)
			     (lub -1
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (lub ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))
				   (if test
				     ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v))))))))))
			     (if test
			       -1
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (lub ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))
				   (if test
				     ((car k) 0)
				     ((car index-cps)
				      (cdr^ ls)
				      (cons
					(lambda (v)
					  ((car k) (+^ 1 v)))
					(lambda (v)
					  (c+ #18(0
						   0
						   0
						   0
						   0
						   1
						   0
						   0
						   0
						   0
						   0
						   1
						   2
						   0
						   0
						   0
						   1
						   0)
					    ((cdr k)
					     (+^ 1 v)))))))))))))
		       (lambda (ls k)
			 (let ([test (null?^ ls)])
			   (if (eq? test 'unknown)
			     (cmax #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (cmax (c+ #18(1
						  0
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  4
						  2
						  0
						  0
						  1
						  0)
					   ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v))))))))
				   (if test
				     (c+ #18(1
					      0
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      1
					      4
					      2
					      0
					      0
					      1
					      0)
				       ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v)))))))))))
			     (if test
			       #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			       (let ([test (eq?^ item (car^ ls))])
				 (if (eq? test 'unknown)
				   (cmax (c+ #18(1
						  0
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  4
						  2
						  0
						  0
						  1
						  0)
					   ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k)
					     (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1 v))))))))
				   (if test
				     (c+ #18(1
					      0
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      1
					      4
					      2
					      0
					      0
					      1
					      0)
				       ((cdr k) 0))
				     (c+ #18(1
					      1
					      0
					      1
					      1
					      0
					      0
					      0
					      0
					      0
					      0
					      0
					      5
					      2
					      0
					      0
					      1)
				       ((cdr index-cps)
					(cdr^ ls)
					(cons
					  (lambda (v)
					    ((car k) (+^ 1 v)))
					  (lambda (v)
					    (c+ #18(0
						     0
						     0
						     0
						     0
						     1
						     0
						     0
						     0
						     0
						     0
						     1
						     2
						     0
						     0
						     0
						     1
						     0)
					      ((cdr k)
					       (+^ 1
						 v)))))))))))))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1 1 2)
	      ((cdr index-cps)
	       ls
	       (cons
		 (lambda (x) x)
		 (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0)))))))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 1 0)
	    ((cdr index) 'unknown list-size0)))))))

(define cost-lattice
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define null?^
      (lambda (x) (if (eq? x 'unknown) 'unknown (null? x))))
    (define car^
      (lambda (x) (if (eq? x 'unknown) 'unknown (car x))))
    (define cdr^
      (lambda (x) (if (eq? x 'unknown) 'unknown (cdr x))))
    (define eq?^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (eq? x y))))
    (define +^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (+ x y))))
    (define -^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (- x y))))
    (define *^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (* x y))))
    (define >^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (> x y))))
    (define <^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (< x y))))
    (define =^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (= x y))))
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define lattice
      (closure
        (lambda ()
          (let ([l2
		  ((value make-lattice)
		   '(low high)
		   (closure
		     (lambda (lhs rhs)
		       (if (eq? lhs 'low)
			 (if (eq? rhs 'low)
			   'equal
			   (if (eq? rhs 'high) 'less 'error))
			 (if (eq? lhs 'high)
			   (if (eq? rhs 'low)
			     'more
			     (if (eq? rhs 'high) 'equal 'error))
			   'error)))
		     (lambda (lhs rhs)
		       (if (eq? lhs 'low)
			 (if (eq? rhs 'low)
			   #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0)
			   (if (eq? rhs 'high)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)))
			 (if (eq? lhs 'high)
			   (if (eq? rhs 'low)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			     (if (eq? rhs 'high)
			       #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)
			       #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)))
			   #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0))))))])
            (let ([l3 ((value maps) l2 l2)])
              (let ([l4 ((value maps) l3 l3)])
                (cons ((value count-maps) l2 l2)
		  (cons ((value count-maps) l3 l3)
		    (cons ((value count-maps) l2 l3)
		      (cons ((value count-maps) l3 l2)
			(cons ((value count-maps) l4 l4)
			  '())))))))))
        (lambda ()
          (let ([l2
		  ((value make-lattice)
		   '(low high)
		   (closure
		     (lambda (lhs rhs)
		       (if (eq? lhs 'low)
			 (if (eq? rhs 'low)
			   'equal
			   (if (eq? rhs 'high) 'less 'error))
			 (if (eq? lhs 'high)
			   (if (eq? rhs 'low)
			     'more
			     (if (eq? rhs 'high) 'equal 'error))
			   'error)))
		     (lambda (lhs rhs)
		       (if (eq? lhs 'low)
			 (if (eq? rhs 'low)
			   #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0)
			   (if (eq? rhs 'high)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)))
			 (if (eq? lhs 'high)
			   (if (eq? rhs 'low)
			     #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			     (if (eq? rhs 'high)
			       #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)
			       #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)))
			   #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0))))))])
            (c+ ((cost make-lattice)
                 '(low high)
                 (closure
                   (lambda (lhs rhs)
                     (if (eq? lhs 'low)
		       (if (eq? rhs 'low)
			 'equal
			 (if (eq? rhs 'high) 'less 'error))
		       (if (eq? lhs 'high)
			 (if (eq? rhs 'low)
			   'more
			   (if (eq? rhs 'high) 'equal 'error))
			 'error)))
                   (lambda (lhs rhs)
                     (if (eq? lhs 'low)
		       (if (eq? rhs 'low)
			 #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0)
			 (if (eq? rhs 'high)
			   #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			   #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)))
		       (if (eq? lhs 'high)
			 (if (eq? rhs 'low)
			   #18(0 0 0 0 3 0 0 0 0 0 0 4 3 3 0)
			   (if (eq? rhs 'high)
			     #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)
			     #18(0 0 0 0 4 0 0 0 0 0 0 5 4 4 0)))
			 #18(0 0 0 0 2 0 0 0 0 0 0 3 2 2 0))))))
	      (let ([l3 ((value maps) l2 l2)])
		(c+ (let ([l4 ((value maps) l3 l3)])
		      (c+ ((cost maps) l3 l3)
			((cost count-maps) l2 l2)
			((cost count-maps) l2 l3)
			((cost count-maps) l4 l4)
			((cost count-maps) l3 l2)
			((cost count-maps) l3 l3)
			#18(0 0 5 0 0 0 0 0 0 0 0 2 22 0 3 0 8 1)))
		  ((cost maps) l2 l2))))))))
    (define lexico
      (closure
        (lambda (base)
          (letrec ([lex-first
		     (closure
		       (lambda (lhs rhs)
			 (if (null? lhs)
			   'equal
			   (let ([probe
                                   ((value base) (car lhs) (car rhs))])
			     (if (if (eq? probe 'less)
				   #t
				   (eq? probe 'more))
			       ((value lex-fixed)
				probe
				(cdr lhs)
				(cdr rhs))
			       (if (eq? probe 'equal)
				 ((value lex-first)
				  (cdr lhs)
				  (cdr rhs))
				 (if (eq? probe 'uncomparable)
				   'uncomparable
				   'undefined))))))
		       (lambda (lhs rhs)
			 (if (null? lhs)
			   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			   (let ([probe
                                   ((value base) (car lhs) (car rhs))])
			     (c+ (if (if (eq? probe 'less)
				       #t
				       (eq? probe 'more))
				   (c+ ((cost lex-fixed)
					probe
					(cdr lhs)
					(cdr rhs))
				     (if (eq? probe 'less)
				       #18(2
					    2
					    0
					    1
					    1
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    9
					    3
					    1
					    0
					    2
					    0)
				       #18(2
					    2
					    0
					    1
					    2
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    10
					    3
					    1
					    0
					    2
					    0)))
				   (c+ (if (eq? probe 'equal)
					 (c+ #18(0
						  2
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  5
						  3
						  0
						  0
						  1
						  0)
					   ((cost lex-first)
					    (cdr lhs)
					    (cdr rhs)))
					 (if (eq? probe 'uncomparable)
					   #18(0
						0
						0
						1
						2
						0
						0
						0
						0
						0
						0
						3
						3
						4
						0)
					   #18(0
						0
						0
						1
						2
						0
						0
						0
						0
						0
						0
						3
						3
						4
						0)))
				     (if (eq? probe 'less)
				       #18(2
					    0
					    0
					    0
					    1
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    4
					    1
					    1
					    0
					    1
					    0)
				       #18(2
					    0
					    0
					    0
					    2
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    5
					    1
					    1
					    0
					    1
					    0))))
			       ((cost base) (car lhs) (car rhs)))))))]
                   [lex-fixed
		     (closure
		       (lambda (fixed lhs rhs)
			 (letrec ([check
				    (closure
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          fixed
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (if (if (eq? probe 'equal)
						  #t
						  (eq? probe fixed))
					      ((value check)
					       (cdr lhs)
					       (cdr rhs))
					      'uncomparable))))
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          #18(0
					       0
					       0
					       1
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       2
					       1
					       0)
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (c+ (if (if (eq? probe 'equal)
						      #t
						      (eq? probe fixed))
						  (c+ ((cost check)
						       (cdr lhs)
						       (cdr rhs))
						    (if (eq? probe
							  'equal)
						      #18(2
							   2
							   0
							   1
							   1
							   0
							   0
							   0
							   0
							   0
							   0
							   2
							   8
							   3
							   1
							   0
							   2
							   0)
						      #18(2
							   2
							   0
							   1
							   2
							   0
							   0
							   0
							   0
							   0
							   0
							   1
							   10
							   3
							   1
							   0
							   2
							   0)))
						  (if (eq? probe 'equal)
						    #18(2
							 0
							 0
							 1
							 1
							 0
							 0
							 0
							 0
							 0
							 0
							 3
							 5
							 3
							 1
							 0
							 1
							 0)
						    #18(2
							 0
							 0
							 1
							 2
							 0
							 0
							 0
							 0
							 0
							 0
							 2
							 7
							 3
							 1
							 0
							 1
							 0)))
					      ((cost base)
					       (car lhs)
					       (car rhs)))))))])
			   ((value check) lhs rhs)))
		       (lambda (fixed lhs rhs)
			 (letrec ([check
				    (closure
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          fixed
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (if (if (eq? probe 'equal)
						  #t
						  (eq? probe fixed))
					      ((value check)
					       (cdr lhs)
					       (cdr rhs))
					      'uncomparable))))
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          #18(0
					       0
					       0
					       1
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       2
					       1
					       0)
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (c+ (if (if (eq? probe 'equal)
						      #t
						      (eq? probe fixed))
						  (c+ ((cost check)
						       (cdr lhs)
						       (cdr rhs))
						    (if (eq? probe
							  'equal)
						      #18(2
							   2
							   0
							   1
							   1
							   0
							   0
							   0
							   0
							   0
							   0
							   2
							   8
							   3
							   1
							   0
							   2
							   0)
						      #18(2
							   2
							   0
							   1
							   2
							   0
							   0
							   0
							   0
							   0
							   0
							   1
							   10
							   3
							   1
							   0
							   2
							   0)))
						  (if (eq? probe 'equal)
						    #18(2
							 0
							 0
							 1
							 1
							 0
							 0
							 0
							 0
							 0
							 0
							 3
							 5
							 3
							 1
							 0
							 1
							 0)
						    #18(2
							 0
							 0
							 1
							 2
							 0
							 0
							 0
							 0
							 0
							 0
							 2
							 7
							 3
							 1
							 0
							 1
							 0)))
					      ((cost base)
					       (car lhs)
					       (car rhs)))))))])
			   (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 1)
			     ((cost check) lhs rhs)))))])
            lex-first))
        (lambda (base)
          (letrec ([lex-first
		     (closure
		       (lambda (lhs rhs)
			 (if (null? lhs)
			   'equal
			   (let ([probe
                                   ((value base) (car lhs) (car rhs))])
			     (if (if (eq? probe 'less)
				   #t
				   (eq? probe 'more))
			       ((value lex-fixed)
				probe
				(cdr lhs)
				(cdr rhs))
			       (if (eq? probe 'equal)
				 ((value lex-first)
				  (cdr lhs)
				  (cdr rhs))
				 (if (eq? probe 'uncomparable)
				   'uncomparable
				   'undefined))))))
		       (lambda (lhs rhs)
			 (if (null? lhs)
			   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			   (let ([probe
                                   ((value base) (car lhs) (car rhs))])
			     (c+ (if (if (eq? probe 'less)
				       #t
				       (eq? probe 'more))
				   (c+ ((cost lex-fixed)
					probe
					(cdr lhs)
					(cdr rhs))
				     (if (eq? probe 'less)
				       #18(2
					    2
					    0
					    1
					    1
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    9
					    3
					    1
					    0
					    2
					    0)
				       #18(2
					    2
					    0
					    1
					    2
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    10
					    3
					    1
					    0
					    2
					    0)))
				   (c+ (if (eq? probe 'equal)
					 (c+ #18(0
						  2
						  0
						  1
						  1
						  0
						  0
						  0
						  0
						  0
						  0
						  1
						  5
						  3
						  0
						  0
						  1
						  0)
					   ((cost lex-first)
					    (cdr lhs)
					    (cdr rhs)))
					 (if (eq? probe 'uncomparable)
					   #18(0
						0
						0
						1
						2
						0
						0
						0
						0
						0
						0
						3
						3
						4
						0)
					   #18(0
						0
						0
						1
						2
						0
						0
						0
						0
						0
						0
						3
						3
						4
						0)))
				     (if (eq? probe 'less)
				       #18(2
					    0
					    0
					    0
					    1
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    4
					    1
					    1
					    0
					    1
					    0)
				       #18(2
					    0
					    0
					    0
					    2
					    0
					    0
					    0
					    0
					    0
					    0
					    2
					    5
					    1
					    1
					    0
					    1
					    0))))
			       ((cost base) (car lhs) (car rhs)))))))]
                   [lex-fixed
		     (closure
		       (lambda (fixed lhs rhs)
			 (letrec ([check
				    (closure
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          fixed
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (if (if (eq? probe 'equal)
						  #t
						  (eq? probe fixed))
					      ((value check)
					       (cdr lhs)
					       (cdr rhs))
					      'uncomparable))))
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          #18(0
					       0
					       0
					       1
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       2
					       1
					       0)
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (c+ (if (if (eq? probe 'equal)
						      #t
						      (eq? probe fixed))
						  (c+ ((cost check)
						       (cdr lhs)
						       (cdr rhs))
						    (if (eq? probe
							  'equal)
						      #18(2
							   2
							   0
							   1
							   1
							   0
							   0
							   0
							   0
							   0
							   0
							   2
							   8
							   3
							   1
							   0
							   2
							   0)
						      #18(2
							   2
							   0
							   1
							   2
							   0
							   0
							   0
							   0
							   0
							   0
							   1
							   10
							   3
							   1
							   0
							   2
							   0)))
						  (if (eq? probe 'equal)
						    #18(2
							 0
							 0
							 1
							 1
							 0
							 0
							 0
							 0
							 0
							 0
							 3
							 5
							 3
							 1
							 0
							 1
							 0)
						    #18(2
							 0
							 0
							 1
							 2
							 0
							 0
							 0
							 0
							 0
							 0
							 2
							 7
							 3
							 1
							 0
							 1
							 0)))
					      ((cost base)
					       (car lhs)
					       (car rhs)))))))])
			   ((value check) lhs rhs)))
		       (lambda (fixed lhs rhs)
			 (letrec ([check
				    (closure
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          fixed
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (if (if (eq? probe 'equal)
						  #t
						  (eq? probe fixed))
					      ((value check)
					       (cdr lhs)
					       (cdr rhs))
					      'uncomparable))))
				      (lambda (lhs rhs)
					(if (null? lhs)
                                          #18(0
					       0
					       0
					       1
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       0
					       2
					       1
					       0)
                                          (let ([probe
						  ((value base)
						   (car lhs)
						   (car rhs))])
                                            (c+ (if (if (eq? probe 'equal)
						      #t
						      (eq? probe fixed))
						  (c+ ((cost check)
						       (cdr lhs)
						       (cdr rhs))
						    (if (eq? probe
							  'equal)
						      #18(2
							   2
							   0
							   1
							   1
							   0
							   0
							   0
							   0
							   0
							   0
							   2
							   8
							   3
							   1
							   0
							   2
							   0)
						      #18(2
							   2
							   0
							   1
							   2
							   0
							   0
							   0
							   0
							   0
							   0
							   1
							   10
							   3
							   1
							   0
							   2
							   0)))
						  (if (eq? probe 'equal)
						    #18(2
							 0
							 0
							 1
							 1
							 0
							 0
							 0
							 0
							 0
							 0
							 3
							 5
							 3
							 1
							 0
							 1
							 0)
						    #18(2
							 0
							 0
							 1
							 2
							 0
							 0
							 0
							 0
							 0
							 0
							 2
							 7
							 3
							 1
							 0
							 1
							 0)))
					      ((cost base)
					       (car lhs)
					       (car rhs)))))))])
			   (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 1)
			     ((cost check) lhs rhs)))))])
            #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 2)))))
    (define make-lattice
      (closure
        (lambda (elem-list cmp-func) (cons elem-list cmp-func))
        (lambda (elem-list cmp-func)
          #18(0 0 1 0 0 0 0 0 0 0 0 0 2 0))))
    (define lattice->elements
      (closure car (lambda args #18(1 0))))
    (define lattice->cmp (closure cdr (lambda args #18(0 1 0))))
    (define zulu-select
      (closure
        (lambda (test lst)
          (letrec ([select-a
		     (closure
		       (lambda (ac lst)
			 (if (null? lst)
			   ((value reverse) ac)
			   ((value select-a)
			    (let ([head (car lst)])
			      (if ((value test) head) (cons head ac) ac))
			    (cdr lst))))
		       (lambda (ac lst)
			 (if (null? lst)
			   (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0)
			     ((cost reverse) ac))
			   (c+ ((cost select-a)
				(let ([head (car lst)])
				  (if ((value test) head)
				    (cons head ac)
				    ac))
				(cdr lst))
			     (let ([head (car lst)])
			       (if ((value test) head)
				 (c+ #18(1
					  1
					  1
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  8
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  7
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))))))))])
            ((value select-a) '() lst)))
        (lambda (test lst)
          (letrec ([select-a
		     (closure
		       (lambda (ac lst)
			 (if (null? lst)
			   ((value reverse) ac)
			   ((value select-a)
			    (let ([head (car lst)])
			      (if ((value test) head) (cons head ac) ac))
			    (cdr lst))))
		       (lambda (ac lst)
			 (if (null? lst)
			   (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0)
			     ((cost reverse) ac))
			   (c+ ((cost select-a)
				(let ([head (car lst)])
				  (if ((value test) head)
				    (cons head ac)
				    ac))
				(cdr lst))
			     (let ([head (car lst)])
			       (if ((value test) head)
				 (c+ #18(1
					  1
					  1
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  8
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  7
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))))))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 1)
	      ((cost select-a) '() lst))))))
    (define reverse
      (letrec ([rev
		 (closure
		   (lambda (ls answ)
		     (if (null? ls)
		       answ
		       ((value rev) (cdr ls) (cons (car ls) answ))))
		   (lambda (ls answ)
		     (if (null? ls)
		       #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
		       (c+ #18(1 1 1 1 0 0 0 0 0 0 0 0 5 1 0 0 1 0)
			 ((cost rev) (cdr ls) (cons (car ls) answ))))))])
        (closure
          (lambda (ls) ((value rev) ls '()))
          (lambda (ls)
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 1 0)
	      ((cost rev) ls '()))))))
    (define append
      (closure
        (lambda (ls1 ls2)
          (letrec ([append
		     (closure
		       (lambda (ls1)
			 (if (null? ls1)
			   ls2
			   (cons (car ls1) ((value append) (cdr ls1)))))
		       (lambda (ls1)
			 (if (null? ls1)
			   #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
			   (c+ #18(1 1 1 1 0 0 0 0 0 0 0 0 4 1 0 0 1 0)
			     ((cost append) (cdr ls1))))))])
            ((value append) ls1)))
        (lambda (ls1 ls2)
          (letrec ([append
		     (closure
		       (lambda (ls1)
			 (if (null? ls1)
			   ls2
			   (cons (car ls1) ((value append) (cdr ls1)))))
		       (lambda (ls1)
			 (if (null? ls1)
			   #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
			   (c+ #18(1 1 1 1 0 0 0 0 0 0 0 0 4 1 0 0 1 0)
			     ((cost append) (cdr ls1))))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
	      ((cost append) ls1))))))
    (define map
      (closure
        (lambda (f ls)
          (letrec ([map
		     (closure
		       (lambda (ls)
			 (if (null? ls)
			   '()
			   (cons ((value f) (car ls))
			     ((value map) (cdr ls)))))
		       (lambda (ls)
			 (if (null? ls)
			   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			   (c+ ((cost f) (car ls))
			     ((cost map) (cdr ls))
			     #18(1
				  1
				  1
				  1
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  5
				  1
				  0
				  0
				  2
				  0)))))])
            ((value map) ls)))
        (lambda (f ls)
          (letrec ([map
		     (closure
		       (lambda (ls)
			 (if (null? ls)
			   '()
			   (cons ((value f) (car ls))
			     ((value map) (cdr ls)))))
		       (lambda (ls)
			 (if (null? ls)
			   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
			   (c+ ((cost f) (car ls))
			     ((cost map) (cdr ls))
			     #18(1
				  1
				  1
				  1
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  5
				  1
				  0
				  0
				  2
				  0)))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
	      ((cost map) ls))))))
    (define select-map
      (closure
        (lambda (test func lst)
          (letrec ([select-a
		     (closure
		       (lambda (ac lst)
			 (if (null? lst)
			   ((value reverse) ac)
			   ((value select-a)
			    (let ([head (car lst)])
			      (if ((value test) head)
				(cons ((value func) head) ac)
				ac))
			    (cdr lst))))
		       (lambda (ac lst)
			 (if (null? lst)
			   (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0)
			     ((cost reverse) ac))
			   (c+ ((cost select-a)
				(let ([head (car lst)])
				  (if ((value test) head)
				    (cons ((value func) head) ac)
				    ac))
				(cdr lst))
			     (let ([head (car lst)])
			       (if ((value test) head)
				 (c+ ((cost test) head)
				   ((cost func) head)
				   #18(1
					1
					1
					1
					0
					0
					0
					0
					0
					0
					0
					0
					9
					2
					1
					0
					3
					0))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  7
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))))))))])
            ((value select-a) '() lst)))
        (lambda (test func lst)
          (letrec ([select-a
		     (closure
		       (lambda (ac lst)
			 (if (null? lst)
			   ((value reverse) ac)
			   ((value select-a)
			    (let ([head (car lst)])
			      (if ((value test) head)
				(cons ((value func) head) ac)
				ac))
			    (cdr lst))))
		       (lambda (ac lst)
			 (if (null? lst)
			   (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0)
			     ((cost reverse) ac))
			   (c+ ((cost select-a)
				(let ([head (car lst)])
				  (if ((value test) head)
				    (cons ((value func) head) ac)
				    ac))
				(cdr lst))
			     (let ([head (car lst)])
			       (if ((value test) head)
				 (c+ ((cost test) head)
				   ((cost func) head)
				   #18(1
					1
					1
					1
					0
					0
					0
					0
					0
					0
					0
					0
					9
					2
					1
					0
					3
					0))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  7
					  2
					  1
					  0
					  2
					  0)
				   ((cost test) head))))))))])
            (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 1)
	      ((cost select-a) '() lst))))))
    (define map-and
      (closure
        (lambda (proc lst)
          (if (null? lst)
	    #t
	    (letrec ([drudge
		       (closure
			 (lambda (lst)
			   (let ([rest (cdr lst)])
			     (if (null? rest)
			       ((value proc) (car lst))
			       (if ((value proc) (car lst))
				 ((value drudge) rest)
				 #f))))
			 (lambda (lst)
			   (let ([rest (cdr lst)])
			     (if (null? rest)
			       (c+ #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					4
					1
					1
					0
					1
					0)
				 ((cost proc) (car lst)))
			       (if ((value proc) (car lst))
				 (c+ ((cost drudge) rest)
				   ((cost proc) (car lst))
				   #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					6
					2
					1
					0
					2
					0))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  1
					  4
					  2
					  1
					  0
					  1
					  0)
				   ((cost proc) (car lst))))))))])
	      ((value drudge) lst))))
        (lambda (proc lst)
          (if (null? lst)
	    #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
	    (letrec ([drudge
		       (closure
			 (lambda (lst)
			   (let ([rest (cdr lst)])
			     (if (null? rest)
			       ((value proc) (car lst))
			       (if ((value proc) (car lst))
				 ((value drudge) rest)
				 #f))))
			 (lambda (lst)
			   (let ([rest (cdr lst)])
			     (if (null? rest)
			       (c+ #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					4
					1
					1
					0
					1
					0)
				 ((cost proc) (car lst)))
			       (if ((value proc) (car lst))
				 (c+ ((cost drudge) rest)
				   ((cost proc) (car lst))
				   #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					6
					2
					1
					0
					2
					0))
				 (c+ #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  1
					  4
					  2
					  1
					  0
					  1
					  0)
				   ((cost proc) (car lst))))))))])
	      (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 1)
		((cost drudge) lst)))))))
    (define maps-1
      (closure
        (lambda (source target pas new)
          (let ([scmp ((value lattice->cmp) source)]
                [tcmp ((value lattice->cmp) target)])
            (let ([less
		    ((value select-map)
		     (closure
		       (lambda (p) (eq? 'less ((value scmp) (car p) new)))
		       (lambda (p)
			 (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			   ((cost scmp) (car p) new))))
		     (closure cdr (lambda args #18(0 1 0)))
		     pas)]
                  [more
		    ((value select-map)
		     (closure
		       (lambda (p) (eq? 'more ((value scmp) (car p) new)))
		       (lambda (p)
			 (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			   ((cost scmp) (car p) new))))
		     (closure cdr (lambda args #18(0 1 0)))
		     pas)])
              ((value zulu-select)
               (closure
                 (lambda (t)
                   (if ((value map-and)
                        (closure
                          (lambda (t2)
                            (let ([tcmpt2 ((value tcmp) t2 t)])
                              (if (eq? tcmpt2 'less)
				#t
				(eq? tcmpt2 'equal))))
                          (lambda (t2)
                            (let ([tcmpt2 ((value tcmp) t2 t)])
                              (c+ ((cost tcmp) t2 t)
				(if (eq? tcmpt2 'less)
				  #18(0
				       0
				       0
				       0
				       1
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       4
				       1
				       1
				       0
				       1
				       0)
				  #18(0
				       0
				       0
				       0
				       2
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       5
				       1
				       1
				       0
				       1
				       0))))))
                        less)
		     ((value map-and)
		      (closure
			(lambda (t2)
			  (let ([tcmpt2 ((value tcmp) t2 t)])
			    (if (eq? tcmpt2 'more)
			      #t
			      (eq? tcmpt2 'equal))))
			(lambda (t2)
			  (let ([tcmpt2 ((value tcmp) t2 t)])
			    (c+ ((cost tcmp) t2 t)
			      (if (eq? tcmpt2 'more)
				#18(0
				     0
				     0
				     0
				     1
				     0
				     0
				     0
				     0
				     0
				     0
				     2
				     4
				     1
				     1
				     0
				     1
				     0)
				#18(0
				     0
				     0
				     0
				     2
				     0
				     0
				     0
				     0
				     0
				     0
				     2
				     5
				     1
				     1
				     0
				     1
				     0))))))
		      more)
		     #f))
                 (lambda (t)
                   (if ((value map-and)
                        (closure
                          (lambda (t2)
                            (let ([tcmpt2 ((value tcmp) t2 t)])
                              (if (eq? tcmpt2 'less)
				#t
				(eq? tcmpt2 'equal))))
                          (lambda (t2)
                            (let ([tcmpt2 ((value tcmp) t2 t)])
                              (c+ ((cost tcmp) t2 t)
				(if (eq? tcmpt2 'less)
				  #18(0
				       0
				       0
				       0
				       1
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       4
				       1
				       1
				       0
				       1
				       0)
				  #18(0
				       0
				       0
				       0
				       2
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       5
				       1
				       1
				       0
				       1
				       0))))))
                        less)
		     (c+ ((cost map-and)
			  (closure
			    (lambda (t2)
			      (let ([tcmpt2 ((value tcmp) t2 t)])
				(if (eq? tcmpt2 'more)
				  #t
				  (eq? tcmpt2 'equal))))
			    (lambda (t2)
			      (let ([tcmpt2 ((value tcmp) t2 t)])
				(c+ ((cost tcmp) t2 t)
				  (if (eq? tcmpt2 'more)
				    #18(0
					 0
					 0
					 0
					 1
					 0
					 0
					 0
					 0
					 0
					 0
					 2
					 4
					 1
					 1
					 0
					 1
					 0)
				    #18(0
					 0
					 0
					 0
					 2
					 0
					 0
					 0
					 0
					 0
					 0
					 2
					 5
					 1
					 1
					 0
					 1
					 0))))))
			  more)
		       ((cost map-and)
			(closure
			  (lambda (t2)
			    (let ([tcmpt2 ((value tcmp) t2 t)])
			      (if (eq? tcmpt2 'less)
				#t
				(eq? tcmpt2 'equal))))
			  (lambda (t2)
			    (let ([tcmpt2 ((value tcmp) t2 t)])
			      (c+ ((cost tcmp) t2 t)
				(if (eq? tcmpt2 'less)
				  #18(0
				       0
				       0
				       0
				       1
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       4
				       1
				       1
				       0
				       1
				       0)
				  #18(0
				       0
				       0
				       0
				       2
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       5
				       1
				       1
				       0
				       1
				       0))))))
			less)
		       #18(0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 2))
		     (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 1 0 0 1)
		       ((cost map-and)
			(closure
			  (lambda (t2)
			    (let ([tcmpt2 ((value tcmp) t2 t)])
			      (if (eq? tcmpt2 'less)
				#t
				(eq? tcmpt2 'equal))))
			  (lambda (t2)
			    (let ([tcmpt2 ((value tcmp) t2 t)])
			      (c+ ((cost tcmp) t2 t)
				(if (eq? tcmpt2 'less)
				  #18(0
				       0
				       0
				       0
				       1
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       4
				       1
				       1
				       0
				       1
				       0)
				  #18(0
				       0
				       0
				       0
				       2
				       0
				       0
				       0
				       0
				       0
				       0
				       2
				       5
				       1
				       1
				       0
				       1
				       0))))))
			less)))))
               ((value lattice->elements) target)))))
        (lambda (source target pas new)
          (let ([scmp ((value lattice->cmp) source)]
                [tcmp ((value lattice->cmp) target)])
            (c+ ((cost lattice->cmp) target)
	      ((cost lattice->cmp) source)
	      (let ([less
		      ((value select-map)
		       (closure
			 (lambda (p)
			   (eq? 'less ((value scmp) (car p) new)))
			 (lambda (p)
			   (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			     ((cost scmp) (car p) new))))
		       (closure cdr (lambda args #18(0 1 0)))
		       pas)]
		    [more
		      ((value select-map)
		       (closure
			 (lambda (p)
			   (eq? 'more ((value scmp) (car p) new)))
			 (lambda (p)
			   (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			     ((cost scmp) (car p) new))))
		       (closure cdr (lambda args #18(0 1 0)))
		       pas)])
		(c+ ((cost lattice->elements) target)
		  ((cost zulu-select)
		   (closure
		     (lambda (t)
		       (if ((value map-and)
			    (closure
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (if (eq? tcmpt2 'less)
				    #t
				    (eq? tcmpt2 'equal))))
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (c+ ((cost tcmp) t2 t)
				    (if (eq? tcmpt2 'less)
				      #18(0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   4
					   1
					   1
					   0
					   1
					   0)
				      #18(0
					   0
					   0
					   0
					   2
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   5
					   1
					   1
					   0
					   1
					   0))))))
			    less)
			 ((value map-and)
			  (closure
			    (lambda (t2)
			      (let ([tcmpt2 ((value tcmp) t2 t)])
				(if (eq? tcmpt2 'more)
				  #t
				  (eq? tcmpt2 'equal))))
			    (lambda (t2)
			      (let ([tcmpt2 ((value tcmp) t2 t)])
				(c+ ((cost tcmp) t2 t)
				  (if (eq? tcmpt2 'more)
				    #18(0
					 0
					 0
					 0
					 1
					 0
					 0
					 0
					 0
					 0
					 0
					 2
					 4
					 1
					 1
					 0
					 1
					 0)
				    #18(0
					 0
					 0
					 0
					 2
					 0
					 0
					 0
					 0
					 0
					 0
					 2
					 5
					 1
					 1
					 0
					 1
					 0))))))
			  more)
			 #f))
		     (lambda (t)
		       (if ((value map-and)
			    (closure
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (if (eq? tcmpt2 'less)
				    #t
				    (eq? tcmpt2 'equal))))
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (c+ ((cost tcmp) t2 t)
				    (if (eq? tcmpt2 'less)
				      #18(0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   4
					   1
					   1
					   0
					   1
					   0)
				      #18(0
					   0
					   0
					   0
					   2
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   5
					   1
					   1
					   0
					   1
					   0))))))
			    less)
			 (c+ ((cost map-and)
			      (closure
				(lambda (t2)
				  (let ([tcmpt2 ((value tcmp) t2 t)])
				    (if (eq? tcmpt2 'more)
				      #t
				      (eq? tcmpt2 'equal))))
				(lambda (t2)
				  (let ([tcmpt2 ((value tcmp) t2 t)])
				    (c+ ((cost tcmp) t2 t)
				      (if (eq? tcmpt2 'more)
					#18(0
					     0
					     0
					     0
					     1
					     0
					     0
					     0
					     0
					     0
					     0
					     2
					     4
					     1
					     1
					     0
					     1
					     0)
					#18(0
					     0
					     0
					     0
					     2
					     0
					     0
					     0
					     0
					     0
					     0
					     2
					     5
					     1
					     1
					     0
					     1
					     0))))))
			      more)
			   ((cost map-and)
			    (closure
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (if (eq? tcmpt2 'less)
				    #t
				    (eq? tcmpt2 'equal))))
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (c+ ((cost tcmp) t2 t)
				    (if (eq? tcmpt2 'less)
				      #18(0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   4
					   1
					   1
					   0
					   1
					   0)
				      #18(0
					   0
					   0
					   0
					   2
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   5
					   1
					   1
					   0
					   1
					   0))))))
			    less)
			   #18(0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 2))
			 (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 1 0 0 1)
			   ((cost map-and)
			    (closure
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (if (eq? tcmpt2 'less)
				    #t
				    (eq? tcmpt2 'equal))))
			      (lambda (t2)
				(let ([tcmpt2 ((value tcmp) t2 t)])
				  (c+ ((cost tcmp) t2 t)
				    (if (eq? tcmpt2 'less)
				      #18(0
					   0
					   0
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   4
					   1
					   1
					   0
					   1
					   0)
				      #18(0
					   0
					   0
					   0
					   2
					   0
					   0
					   0
					   0
					   0
					   0
					   2
					   5
					   1
					   1
					   0
					   1
					   0))))))
			    less)))))
		   ((value lattice->elements) target))
		  ((cost select-map)
		   (closure
		     (lambda (p)
		       (eq? 'less ((value scmp) (car p) new)))
		     (lambda (p)
		       (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			 ((cost scmp) (car p) new))))
		   (closure cdr (lambda args #18(0 1 0)))
		   pas)
		  ((cost select-map)
		   (closure
		     (lambda (p)
		       (eq? 'more ((value scmp) (car p) new)))
		     (lambda (p)
		       (c+ #18(1 0 0 0 1 0 0 0 0 0 0 1 3 0 0 0 1 0)
			 ((cost scmp) (car p) new))))
		   (closure cdr (lambda args #18(0 1 0)))
		   pas)
		  #18(0 0 0 0 0 0 0 0 0 0 0 0 11 0 2 0 6 5))))))))
    (define maps-rest
      (closure
        (lambda (source target pas rest to-1 to-collect)
          (if (null? rest)
	    ((value to-1) pas)
	    (let ([next (car rest)] [rest (cdr rest)])
	      ((value to-collect)
	       ((value map)
		(closure
		  (lambda (x)
		    ((value maps-rest)
		     source
		     target
		     (cons (cons next x) pas)
		     rest
		     to-1
		     to-collect))
		  (lambda (x)
		    (c+ #18(0 0 2 0 0 0 0 0 0 0 0 0 9 0 0 0 1 0)
		      ((cost maps-rest)
		       source
		       target
		       (cons (cons next x) pas)
		       rest
		       to-1
		       to-collect))))
		((value maps-1) source target pas next))))))
        (lambda (source target pas rest to-1 to-collect)
          (if (null? rest)
	    (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0)
	      ((cost to-1) pas))
	    (let ([next (car rest)] [rest (cdr rest)])
	      (c+ ((cost map)
		   (closure
		     (lambda (x)
		       ((value maps-rest)
			source
			target
			(cons (cons next x) pas)
			rest
			to-1
			to-collect))
		     (lambda (x)
		       (c+ #18(0 0 2 0 0 0 0 0 0 0 0 0 9 0 0 0 1 0)
			 ((cost maps-rest)
			  source
			  target
			  (cons (cons next x) pas)
			  rest
			  to-1
			  to-collect))))
		   ((value maps-1) source target pas next))
		((cost maps-1) source target pas next)
		((cost to-collect)
		 ((value map)
		  (closure
		    (lambda (x)
		      ((value maps-rest)
		       source
		       target
		       (cons (cons next x) pas)
		       rest
		       to-1
		       to-collect))
		    (lambda (x)
		      (c+ #18(0 0 2 0 0 0 0 0 0 0 0 0 9 0 0 0 1 0)
			((cost maps-rest)
			 source
			 target
			 (cons (cons next x) pas)
			 rest
			 to-1
			 to-collect))))
		  ((value maps-1) source target pas next)))
		#18(1 1 0 1 0 0 0 0 0 0 0 0 10 1 1 0 3 1)))))))
    (define maps
      (closure
        (lambda (source target)
          ((value make-lattice)
           ((value maps-rest)
            source
            target
            '()
            ((value lattice->elements) source)
            (closure
              (lambda (x)
                (cons ((value map)
                       (closure cdr (lambda args #18(0 1 0)))
                       x)
		  '()))
              (lambda (x)
                (c+ #18(0 0 1 0 0 0 0 0 0 0 0 1 2 0 0 0 1)
		  ((cost map)
		   (closure cdr (lambda args #18(0 1 0)))
		   x))))
            (closure
              (lambda (x)
                (letrec ([loop
			   (closure
			     (lambda (x)
			       (if (null? x)
				 '()
				 ((value append)
				  (car x)
				  ((value loop) (cdr x)))))
			     (lambda (x)
			       (if (null? x)
				 #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				 (c+ ((cost append)
				      (car x)
				      ((value loop) (cdr x)))
				   ((cost loop) (cdr x))
				   #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					5
					1
					0
					0
					2
					0)))))])
                  ((value loop) x)))
              (lambda (x)
                (letrec ([loop
			   (closure
			     (lambda (x)
			       (if (null? x)
				 '()
				 ((value append)
				  (car x)
				  ((value loop) (cdr x)))))
			     (lambda (x)
			       (if (null? x)
				 #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				 (c+ ((cost append)
				      (car x)
				      ((value loop) (cdr x)))
				   ((cost loop) (cdr x))
				   #18(1
					1
					0
					1
					0
					0
					0
					0
					0
					0
					0
					0
					5
					1
					0
					0
					2
					0)))))])
                  (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
		    ((cost loop) x))))))
           ((value lexico) ((value lattice->cmp) target))))
        (lambda (source target)
          (c+ ((cost lexico) ((value lattice->cmp) target))
	    ((cost lattice->cmp) target)
	    ((cost maps-rest)
	     source
	     target
	     '()
	     ((value lattice->elements) source)
	     (closure
	       (lambda (x)
		 (cons ((value map)
			(closure cdr (lambda args #18(0 1 0)))
			x)
		   '()))
	       (lambda (x)
		 (c+ #18(0 0 1 0 0 0 0 0 0 0 0 1 2 0 0 0 1)
		   ((cost map)
		    (closure cdr (lambda args #18(0 1 0)))
		    x))))
	     (closure
	       (lambda (x)
		 (letrec ([loop
			    (closure
			      (lambda (x)
				(if (null? x)
				  '()
				  ((value append)
				   (car x)
				   ((value loop) (cdr x)))))
			      (lambda (x)
				(if (null? x)
				  #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				  (c+ ((cost append)
				       (car x)
				       ((value loop) (cdr x)))
				    ((cost loop) (cdr x))
				    #18(1
					 1
					 0
					 1
					 0
					 0
					 0
					 0
					 0
					 0
					 0
					 0
					 5
					 1
					 0
					 0
					 2
					 0)))))])
		   ((value loop) x)))
	       (lambda (x)
		 (letrec ([loop
			    (closure
			      (lambda (x)
				(if (null? x)
				  '()
				  ((value append)
				   (car x)
				   ((value loop) (cdr x)))))
			      (lambda (x)
				(if (null? x)
				  #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				  (c+ ((cost append)
				       (car x)
				       ((value loop) (cdr x)))
				    ((cost loop) (cdr x))
				    #18(1
					 1
					 0
					 1
					 0
					 0
					 0
					 0
					 0
					 0
					 0
					 0
					 5
					 1
					 0
					 0
					 2
					 0)))))])
		   (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
		     ((cost loop) x))))))
	    ((cost lattice->elements) source)
	    ((cost make-lattice)
	     ((value maps-rest)
	      source
	      target
	      '()
	      ((value lattice->elements) source)
	      (closure
		(lambda (x)
		  (cons ((value map)
			 (closure cdr (lambda args #18(0 1 0)))
			 x)
		    '()))
		(lambda (x)
		  (c+ #18(0 0 1 0 0 0 0 0 0 0 0 1 2 0 0 0 1)
		    ((cost map)
		     (closure cdr (lambda args #18(0 1 0)))
		     x))))
	      (closure
		(lambda (x)
		  (letrec ([loop
			     (closure
			       (lambda (x)
				 (if (null? x)
				   '()
				   ((value append)
				    (car x)
				    ((value loop) (cdr x)))))
			       (lambda (x)
				 (if (null? x)
				   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				   (c+ ((cost append)
					(car x)
					((value loop) (cdr x)))
				     ((cost loop) (cdr x))
				     #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  5
					  1
					  0
					  0
					  2
					  0)))))])
		    ((value loop) x)))
		(lambda (x)
		  (letrec ([loop
			     (closure
			       (lambda (x)
				 (if (null? x)
				   '()
				   ((value append)
				    (car x)
				    ((value loop) (cdr x)))))
			       (lambda (x)
				 (if (null? x)
				   #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
				   (c+ ((cost append)
					(car x)
					((value loop) (cdr x)))
				     ((cost loop) (cdr x))
				     #18(1
					  1
					  0
					  1
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  0
					  5
					  1
					  0
					  0
					  2
					  0)))))])
		    (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
		      ((cost loop) x))))))
	     ((value lexico) ((value lattice->cmp) target)))
	    #18(0 0 0 0 0 0 0 0 0 0 0 1 9 0 0 0 5 2)))))
    (define print-frequency 10000)
    (define count-maps
      (closure
        (lambda (source target)
          ((value maps-rest)
           source
           target
           '()
           ((value lattice->elements) source)
           (closure
             (lambda (x) 1)
             (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 1 0)))
           (closure
             (lambda (x)
               (letrec ([loop
			  (closure
			    (lambda (x c)
			      (if (null? x)
				c
				((value loop) (cdr x) (+ (car x) c))))
			    (lambda (x c)
			      (if (null? x)
				#18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
				(c+ #18(1
                                         1
                                         0
                                         1
                                         0
                                         1
                                         0
                                         0
                                         0
                                         0
                                         0
                                         0
                                         5
                                         1
                                         0
                                         0
                                         1
                                         0)
				  ((cost loop)
				   (cdr x)
				   (+ (car x) c))))))])
                 ((value loop) x 0)))
             (lambda (x)
               (letrec ([loop
			  (closure
			    (lambda (x c)
			      (if (null? x)
				c
				((value loop) (cdr x) (+ (car x) c))))
			    (lambda (x c)
			      (if (null? x)
				#18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
				(c+ #18(1
                                         1
                                         0
                                         1
                                         0
                                         1
                                         0
                                         0
                                         0
                                         0
                                         0
                                         0
                                         5
                                         1
                                         0
                                         0
                                         1
                                         0)
				  ((cost loop)
				   (cdr x)
				   (+ (car x) c))))))])
                 (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 1)
		   ((cost loop) x 0)))))))
        (lambda (source target)
          (c+ ((cost lattice->elements) source)
	    ((cost maps-rest)
	     source
	     target
	     '()
	     ((value lattice->elements) source)
	     (closure
	       (lambda (x) 1)
	       (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 1 0)))
	     (closure
	       (lambda (x)
		 (letrec ([loop
			    (closure
			      (lambda (x c)
				(if (null? x)
				  c
				  ((value loop) (cdr x) (+ (car x) c))))
			      (lambda (x c)
				(if (null? x)
				  #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
				  (c+ #18(1
					   1
					   0
					   1
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   5
					   1
					   0
					   0
					   1
					   0)
				    ((cost loop)
				     (cdr x)
				     (+ (car x) c))))))])
		   ((value loop) x 0)))
	       (lambda (x)
		 (letrec ([loop
			    (closure
			      (lambda (x c)
				(if (null? x)
				  c
				  ((value loop) (cdr x) (+ (car x) c))))
			      (lambda (x c)
				(if (null? x)
				  #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
				  (c+ #18(1
					   1
					   0
					   1
					   0
					   1
					   0
					   0
					   0
					   0
					   0
					   0
					   5
					   1
					   0
					   0
					   1
					   0)
				    ((cost loop)
				     (cdr x)
				     (+ (car x) c))))))])
		   (c+ #18(0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 1)
		     ((cost loop) x 0))))))
	    #18(0 0 0 0 0 0 0 0 0 0 0 1 5 0 0 0 2)))))
    (lambda ()
      (let ()
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0)
	    ((cost lattice))))))))

(define cost-map
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define +^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (+ x y))))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define map
      (closure
        (lambda (f ls)
	  (if (null? ls)
	    '()
	    (cons ((value f) (car ls))
	      ((value map) f (cdr ls)))))
        (lambda (f ls)
	  (if (null? ls)
	    #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
	    (c+ ((cost f) (car ls))
	      ((cost map) f (cdr ls))
	      #18(1 1 1 1 0 0 0 0 0 0 0 0 6 1 0 0 2 0))))))
    (define c-add
      (closure
        (lambda (n)
          (closure
            (lambda (m) (+^ n m))
            (lambda (m) #18(0 0 0 0 0 1 0 0 0 0 0 0 2 0))))
        (lambda (n) #18(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ ((cost c-add) 4)
	    ((cost map) ((value c-add) 4) list-size0)
	    #18(0 0 0 0 0 0 0 0 0 0 0 1 3 0 0 0 2 0)))))))

(define cost-reverse
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define null?^
      (lambda (x) (if (eq? x 'unknown) 'unknown (null? x))))
    (define car^
      (lambda (x) (if (eq? x 'unknown) 'unknown (car x))))
    (define cdr^
      (lambda (x) (if (eq? x 'unknown) 'unknown (cdr x))))
    (define eq?^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (eq? x y))))
    (define +^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (+ x y))))
    (define -^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (- x y))))
    (define *^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (* x y))))
    (define >^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (> x y))))
    (define <^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (< x y))))
    (define =^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (= x y))))
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define reverse
      (closure
        (lambda (ls)
          (if (null? ls)
	    ls
	    ((value append)
	     ((value reverse) (cdr ls))
	     (cons (car ls) '()))))
        (lambda (ls)
          (if (null? ls)
	    #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
	    (c+ ((cost append)
		 ((value reverse) (cdr ls))
		 (cons (car ls) '()))
	      ((cost reverse) (cdr ls))
	      #18(1 1 1 1 0 0 0 0 0 0 0 1 5 1 0 0 2 0))))))
    (define append
      (closure
        (lambda (ls1 ls2)
          (if (null? ls1)
	    ls2
	    (cons (car ls1) ((value append) (cdr ls1) ls2))))
        (lambda (ls1 ls2)
          (if (null? ls1)
	    #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
	    (c+ #18(1 1 1 1 0 0 0 0 0 0 0 0 5 1 0 0 1 0)
	      ((cost append) (cdr ls1) ls2))))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0)
	    ((cost reverse) list-size0)))))))

(define cost-reverse-cps
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define null?^
      (lambda (x) (if (eq? x 'unknown) 'unknown (null? x))))
    (define car^
      (lambda (x) (if (eq? x 'unknown) 'unknown (car x))))
    (define cdr^
      (lambda (x) (if (eq? x 'unknown) 'unknown (cdr x))))
    (define eq?^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (eq? x y))))
    (define +^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (+ x y))))
    (define -^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (- x y))))
    (define *^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (* x y))))
    (define >^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (> x y))))
    (define <^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (< x y))))
    (define =^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (= x y))))
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define reverse-cps
      (closure
        (lambda (ls)
          ((value reverse-cps-hlp)
           ls
           (closure
             (lambda (x) x)
             (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0)))))
        (lambda (ls)
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1)
	    ((cost reverse-cps-hlp)
	     ls
	     (closure
	       (lambda (x) x)
	       (lambda (x) #18(0 0 0 0 0 0 0 0 0 0 0 0 1 0))))))))
    (define reverse-cps-hlp
      (closure
        (lambda (ls k)
          (if (null? ls)
	    ((value k) '())
	    ((value reverse-cps-hlp)
	     (cdr ls)
	     (closure
	       (lambda (v) ((value append-cps) v (cons (car ls) '()) k))
	       (lambda (v)
		 (c+ #18(1 0 1 0 0 0 0 0 0 0 0 1 4 0 0 0 1 0)
		   ((cost append-cps) v (cons (car ls) '()) k)))))))
        (lambda (ls k)
          (if (null? ls)
	    (c+ #18(0 0 0 1 0 0 0 0 0 0 0 1 2 1 0 0 1 0) ((cost k) '()))
	    (c+ #18(0 1 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1)
	      ((cost reverse-cps-hlp)
	       (cdr ls)
	       (closure
		 (lambda (v)
		   ((value append-cps) v (cons (car ls) '()) k))
		 (lambda (v)
		   (c+ #18(1 0 1 0 0 0 0 0 0 0 0 1 4 0 0 0 1 0)
		     ((cost append-cps)
		      v
		      (cons (car ls) '())
		      k))))))))))
    (define append-cps
      (closure
        (lambda (ls1 ls2 k)
          (if (null? ls1)
	    ((value k) ls2)
	    ((value append-cps)
	     (cdr ls1)
	     ls2
	     (closure
	       (lambda (v) ((value k) (cons (car ls1) v)))
	       (lambda (v)
		 (c+ #18(1 0 1 0 0 0 0 0 0 0 0 0 3 0 0 0 1 0)
		   ((cost k) (cons (car ls1) v))))))))
        (lambda (ls1 ls2 k)
          (if (null? ls1)
	    (c+ #18(0 0 0 1 0 0 0 0 0 0 0 0 3 1 0 0 1 0) ((cost k) ls2))
	    (c+ #18(0 1 0 1 0 0 0 0 0 0 0 0 4 1 0 0 1)
	      ((cost append-cps)
	       (cdr ls1)
	       ls2
	       (closure
		 (lambda (v) ((value k) (cons (car ls1) v)))
		 (lambda (v)
		   (c+ #18(1 0 1 0 0 0 0 0 0 0 0 0 3 0 0 0 1 0)
		     ((cost k) (cons (car ls1) v)))))))))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0)
	    ((cost reverse-cps) list-size0)))))))

(define cost-split
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define >^
      (lambda (x y)
        (if (or (eq? x 'unknown) (eq? y 'unknown))
	  'unknown
	  (> x y))))
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define split
      (closure
        (lambda (ls pred k)
	  (if (null? ls)
	    ((value k) '() '())
	    ((value split)
	     (cdr ls)
	     pred
	     (closure
	       (lambda (passed failed)
		 (let ([test ((value pred) (car ls))])
		   (if (eq? test 'unknown)
		     (lub ((value k)
			   (cons (car ls) passed)
			   failed)
		       ((value k)
			passed
			(cons (car ls) failed)))
		     (if test
		       ((value k)
			(cons (car ls) passed)
			failed)
		       ((value k)
			passed
			(cons (car ls) failed))))))
	       (lambda (passed failed)
		 (let ([test ((value pred) (car ls))])
		   (if (eq? test 'unknown)
		     (cmax (c+ ((cost k)
				(cons (car ls) passed)
				failed)
			     ((cost pred) (car ls))
			     #18(2
				  0
				  1
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  0
				  6
				  1
				  0
				  0
				  2
				  0))
		       (c+ ((cost k)
			    passed
			    (cons (car ls) failed))
			 ((cost pred) (car ls))
			 #18(2
			      0
			      1
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      6
			      1
			      0
			      0
			      2
			      0)))
		     (if test
		       (c+ ((cost k)
			    (cons (car ls) passed)
			    failed)
			 ((cost pred) (car ls))
			 #18(2
			      0
			      1
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      6
			      1
			      0
			      0
			      2
			      0))
		       (c+ ((cost k)
			    passed
			    (cons (car ls) failed))
			 ((cost pred) (car ls))
			 #18(2
			      0
			      1
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      0
			      6
			      1
			      0
			      0
			      2
			      0))))))))))
        (lambda (ls pred k)
	  (if (null? ls)
	    (c+ #18(0 0 0 1 0 0 0 0 0 0 0 2 2 1 0 0 1 0)
	      ((cost k) '() '()))
	    (c+ #18(0 1 0 1 0 0 0 0 0 0 0 0 4 1 0 0 1)
	      ((cost split)
	       (cdr ls)
	       pred
	       (closure
		 (lambda (passed failed)
		   (let ([test ((value pred) (car ls))])
		     (if (eq? test 'unknown)
		       (lub ((value k)
			     (cons (car ls) passed)
			     failed)
			 ((value k)
			  passed
			  (cons (car ls) failed)))
		       (if test
			 ((value k)
			  (cons (car ls) passed)
			  failed)
			 ((value k)
			  passed
			  (cons (car ls) failed))))))
		 (lambda (passed failed)
		   (let ([test ((value pred) (car ls))])
		     (if (eq? test 'unknown)
		       (cmax (c+ ((cost k)
				  (cons (car ls) passed)
				  failed)
			       ((cost pred) (car ls))
			       #18(2
				    0
				    1
				    0
				    0
				    0
				    0
				    0
				    0
				    0
				    0
				    0
				    6
				    1
				    0
				    0
				    2
				    0))
			 (c+ ((cost k)
			      passed
			      (cons (car ls) failed))
			   ((cost pred) (car ls))
			   #18(2
				0
				1
				0
				0
				0
				0
				0
				0
				0
				0
				0
				6
				1
				0
				0
				2
				0)))
		       (if test
			 (c+ ((cost k)
			      (cons (car ls) passed)
			      failed)
			   ((cost pred) (car ls))
			   #18(2
				0
				1
				0
				0
				0
				0
				0
				0
				0
				0
				0
				6
				1
				0
				0
				2
				0))
			 (c+ ((cost k)
			      passed
			      (cons (car ls) failed))
			   ((cost pred) (car ls))
			   #18(2
				0
				1
				0
				0
				0
				0
				0
				0
				0
				0
				0
				6
				1
				0
				0
				2
				0)))))))))))))
    (define interval-tester
      (closure
        (lambda (x)
          (let ([test (>^ 10 x)])
            (if (eq? test 'unknown)
	      (lub #f
		(let ([test (>^ x 20)])
		  (if (eq? test 'unknown)
		    (lub #f #t)
		    (if test #f #t))))
	      (if test
		#f
		(let ([test (>^ x 20)])
		  (if (eq? test 'unknown)
		    (lub #f #t)
		    (if test #f #t)))))))
        (lambda (x)
          (let ([test (>^ 10 x)])
            (if (eq? test 'unknown)
	      (cmax #18(0 0 0 0 0 0 0 0 1 0 0 2 1 1 0)
		(let ([test (>^ x 20)])
		  (if (eq? test 'unknown)
		    #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0)
		    (if test
		      #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0)
		      #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0)))))
	      (if test
		#18(0 0 0 0 0 0 0 0 1 0 0 2 1 1 0)
		(let ([test (>^ x 20)])
		  (if (eq? test 'unknown)
		    #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0)
		    (if test
		      #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0)
		      #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0))))))))))
    (define initial-k
      (closure
        (lambda (good bad) (cons good (cons bad '())))
        (lambda (good bad) #18(0 0 2 0 0 0 0 0 0 0 0 1 2 0))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 1 0)
	    ((cost split) list-size0 interval-tester initial-k)))))))

(define cost-split
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define split
      (closure
        (lambda (ls pred k) 'ignored)
        (lambda (ls pred k)
          (if (null? ls)
	    (c+ #18(0 0 0 1 0 0 0 0 0 0 0 2 2 1 0 0 1 0)
	      ((cost k) '() '()))
	    (c+ #18(0 1 0 1 0 0 0 0 0 0 0 0 4 1 0 0 1)
	      ((cost split)
	       (cdr ls)
	       pred
	       (closure
		 (lambda (passed failed) 'ignored)
		 (lambda (passed failed)
		   (c+ ((cost k) (cons (car ls) passed) failed)
		     ((cost pred) (car ls))
		     #18(2 0 1 0 0 0 0 0 0 0 0 0 6 1 0 0 2 0))))))))))
    (define interval-tester
      (closure
        (lambda (x) 'unknown)
        (lambda (x) #18(0 0 0 0 0 0 0 0 2 0 0 3 2 2 0))))
    (define initial-k
      (closure
        (lambda (good bad) (cons good (cons bad '())))
        (lambda (good bad) #18(0 0 2 0 0 0 0 0 0 0 0 1 2 0))))
    (lambda (size0)
      (let ([list-size0 (make-list size0)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 1 0)
	    ((cost split) list-size0 interval-tester initial-k)))))))

(define cost-union
  (let ()
    (define closure cons)
    (define value car)
    (define cost cdr)
    (define lub
      (lambda (x y)
        (cond
          [(equal? x y) x]
          [(atom? x) 'unknown]
          [(atom? y) 'unknown]
          [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))
    (define make-list
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (make-list (- n 1))))))
    (define union
      (closure
        (lambda (set1 set2)
          (if (null? set1)
	    set2
	    (let ([rr ((value union) (cdr set1) set2)])
	      (let ([test ((value member?) (car set1) set2)])
		(if (eq? test 'unknown)
		  'unknown
		  (if test rr (cons (car set1) rr)))))))
        (lambda (set1 set2)
          (if (null? set1)
	    #18(0 0 0 1 0 0 0 0 0 0 0 0 2 1 0)
	    (c+ ((cost member?) (car set1) set2)
	      ((cost union) (cdr set1) set2)
	      #18(2 1 1 1 0 0 0 0 0 0 0 0 9 2 1 0 2 0))))))
    (define member?
      (closure
        (lambda (item ls) (if (null? ls) #f 'unknown))
        (lambda (item ls)
          (if (null? ls)
	    #18(0 0 0 1 0 0 0 0 0 0 0 1 1 1 0)
	    (c+ #18(1 1 0 1 1 0 0 0 0 0 0 0 6 2 0 0 1 0)
	      ((cost member?) item (cdr ls)))))))
    (lambda (size0 size1)
      (let ([list-size0 (make-list size0)]
            [list-size1 (make-list size1)])
        (cost-vector->exp
          (c+ #18(0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 1 0)
	    ((cost union) list-size0 list-size1)))))))

(optimize-level 0)
