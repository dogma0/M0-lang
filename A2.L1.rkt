#lang racket #| Compile A2's Language L0 to A2's Language L1 |#

(provide debruijn index L0→L1)
(module+ test (require rackunit))

#| A2's language L0 is A1's language L0 with one additional conditional expression. |#

; If <e1> is true then evaluate <e2>, else evaluate <e3>.
#;(L0: if <e1> <e2> <e3>)

#| A2's language L1 is A1's language L1 with one additional conditional expression. |#

; The nth if expression.
#;(L1: if <n> <e1> <e2> <e3>)

#| DeBruijn Indexing of an L0 Expression

 This is the same as in A1. |#

(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (match e
    [`(L0: λ (,id) ,e_pri)
     (set! env (list* id env))
     `(L1: λ (,id) ,(debruijn e_pri env))]
    [`(L0: app ,e1 ,e2)
     `(L1: app ,(debruijn e1 env) ,(debruijn e2 env))]
    [`(L0: set! ,id ,e_pri)
     `(L1: set! ,(index-of env id) ,(debruijn e_pri env))]
    [`(L0: var ,id)
     `(L1: var ,(if (index-of env id) (index-of env id) id))]
    [`(L0: if ,e1 ,e2 ,e3)
     `(L1: if ,(debruijn e1 env) ,(debruijn e2 env) ,(debruijn e3 env))]
    [_ e]))

(module+ test
  ; If the L0 expression is
  ; i) λ
  ; ii) app
  ; iii) set!,
  ; then rewrite it to the corresponding L1 expression with referencing variable names replaced with
  ; a natural number indicating how man scopes up the variable is.
  ; If a variable is free, i.e. unbound, then leave it as is.
  (check-equal? (debruijn '(L0: λ (x) (L0: var x)))
                '(L1: λ (x) (L1: var 0)))
  (check-equal? (debruijn '(L0: λ (x) (L0: λ (y) (L0: app (L0: var x) (L0: var y)))))
                '(L1: λ (x) (L1: λ (y) (L1: app (L1: var 1) (L1: var 0)))))
  (check-equal? (debruijn '(L0: λ (x) (L0: λ (_) (L0: set! x (L0: λ (x) (L0: var x))))))
                '(L1: λ (x) (L1: λ (_) (L1: set! 1 (L1: λ (x) (L1: var 0))))))
  (check-equal? (debruijn '(L0: λ (x) (L0: var y)))
                '(L1: λ (x) (L1: var y)))
  (check-equal? (debruijn '(L0: if (L0: λ (x) (L0: var x)) (L0: λ (x) (L0: var x)) (L0: λ (x) (L0: var x))))
                '(L1: if (L1: λ (x) (L1: var 0)) (L1: λ (x) (L1: var 0)) (L1: λ (x) (L1: var 0)))))

#| Indexing of a Debruijnized L0 Expression

 For the A1 subset of L0 this is the same.
 The new conditional expressions are also given unique indices. |#

(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

(module+ test
  (define c (counter))
  (check-equal? (c) 0)
  (check-equal? (c) 1)
  (define c′ (counter))
  (check-equal? (c′) 0)
  (check-equal? (c) 2))

; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.
(define (index e [count (counter)] [if-count (counter)])
  (match e
    [`(L1: λ (,_) ,e_pri)
     (define res (index e_pri count if-count))
     `(L1: λ ,(count) ,res)]
    [`(L1: app ,e1 ,e2)
     `(L1: app ,(index e1 count if-count) ,(index e2 count if-count))]
    [`(L1: set! ,id ,e_pri)
     `(L1: set! ,id ,(index e_pri count if-count))]
    [`(L1: if ,e1 ,e2 ,e3)
     (define res1 (index e1 count if-count))
     (define res2 (index e2 count if-count))
     (define res3 (index e3 count if-count))
     `(L1: if ,(if-count) ,res1 ,res2 ,res3)]
    [_ e]))

(module+ test
  (check-equal? (index (debruijn '(L0: λ (x) (L0: var x))))
                '(L1: λ 0 (L1: var 0)))
  (check-equal? (index (debruijn '(L0: λ (x) (L0: λ (y) (L0: app (L0: var x) (L0: var y))))))
                '(L1: λ 1 (L1: λ 0 (L1: app (L1: var 1) (L1: var 0)))))
  (check-equal? (index (debruijn '(L0: λ (x) (L0: λ (y) (L0: app (L0: λ (x) (L0: var x)) (L0: λ (y) (L0: var y)))))))
                '(L1: λ 3 (L1: λ 2 (L1: app (L1: λ 0 (L1: var 0)) (L1: λ 1 (L1: var 0))))))
  (check-equal? (index (debruijn '(L0: λ (x) (L0: λ (_) (L0: set! x (L0: λ (x) (L0: var x)))))))
                '(L1: λ 2 (L1: λ 1 (L1: set! 1 (L1: λ 0 (L1: var 0))))))
  (check-equal? (index (debruijn '(L0: if (L0: λ (x) (L0: var x)) (L0: λ (x) (L0: var x)) (L0: λ (x) (L0: var x)))))
                '(L1: if 0 (L1: λ 0 (L1: var 0)) (L1: λ 1 (L1: var 0)) (L1: λ 2 (L1: var 0))))
  (check-equal? (index (debruijn '(L0: if (L0: λ (x) (L0: var x))
                                       (L0: λ (x) (L0: var x))
                                       (L0: if (L0: λ (x) (L0: var x))
                                            (L0: λ (x) (L0: var x))
                                            (L0: λ (x) (L0: var x))))))
                '(L1: if 1 (L1: λ 0 (L1: var 0))
                      (L1: λ 1 (L1: var 0))
                      (L1: if 0 (L1: λ 2 (L1: var 0))
                           (L1: λ 3 (L1: var 0))
                           (L1: λ 4 (L1: var 0))))))

#| L0→L1
 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

(define (L0→L1 e)
  (define (L0→L1′ e)
    (match e
      [`(L1: λ ,n ,e_pri) `(L1: λ ,n ,(L0→L1 e_pri))]
      [`(L1: app ,e1 ,e2) `(L1: app ,(L0→L1 e1) ,(L0→L1 e2))]
      [`(L1: set! ,id ,e_pri) `(L1: set! ,id ,(L0→L1 e_pri))]
      [`(L1: if ,n ,e1 ,e2 ,e3) `(L1: if ,n ,(L0→L1 e1) ,(L0→L1 e2) ,(L0→L1 e3))]
      [`(L1: var ,id) e]
      [`(L0: datum ,i) `(L1: datum ,i)]))
  (L0→L1′ (index (debruijn e))))

(module+ test
  (check-equal? (L0→L1 '(L0: λ (x) (L0: var y)))
                '(L1: λ 0 (L1: var y)))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: λ (_) (L0: set! x (L0: λ (x) (L0: app (L0: var z) (L0: var x)))))))
                '(L1: λ 2 (L1: λ 1 (L1: set! 1 (L1: λ 0 (L1: app (L1: var z) (L1: var 0)))))))
  (check-equal? (L0→L1 '(L0: λ (x)
                             (L0: λ (_)
                                  (L0: set! x
                                       (L0: if (L0: λ (x) (L0: var x))
                                            (L0: λ (x) (L0: var x))
                                            (L0: if (L0: λ (x) (L0: var x))
                                                 (L0: λ (x) (L0: var x))
                                                 (L0: λ (x) (L0: var x))))))))
                '(L1: λ 6
                      (L1: λ 5
                           (L1: set! 1
                                (L1: if 1 (L1: λ 0 (L1: var 0))
                                     (L1: λ 1 (L1: var 0))
                                     (L1: if 0 (L1: λ 2 (L1: var 0))
                                          (L1: λ 3 (L1: var 0))
                                          (L1: λ 4 (L1: var 0)))))))))
