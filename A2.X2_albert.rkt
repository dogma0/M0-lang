(define (call)
  (list
   (popq temp) ; temp = function pointer
   (pushq env)
   (movq (★ temp 1) env) ; env = closure env
    
   (movq env (★ next 0)) ; (env, result) on heap
   (movq result (★ next 1)) 
   (movq next env) ; newly created env
   (addq (constant 16) next)
   (callq temp)
   ; ((void)(*)()(f[0[))
   (popq env)
  ))
