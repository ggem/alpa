(define symcost_union
  (lambda (size)
    (case size
      [(0) (+ cost_varref cost_null cost_constant cost_cond)]
      [(1)
       (+ (* 14 cost_varref)
          (* 4 cost_null)
          (* 3 cost_funcall)
          (* 2 cost_constant)
          cost_cons
          (* 6 cost_cond)
          (* 2 cost_cdr)
          (* 3 cost_car)
          cost_booleanop
          cost_binding)]
      [(2)
       (+ (* 37 cost_varref)
          (* 9 cost_null)
          (* 8 cost_funcall)
          (* 3 cost_constant)
          (* 2 cost_cons)
          (* 15 cost_cond)
          (* 6 cost_cdr)
          (* 8 cost_car)
          (* 4 cost_booleanop)
          (* 2 cost_binding))])))

