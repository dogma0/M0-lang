#lang racket #| Macros and Runtime Defining Language M0 |#

(provide T:*id* T:*datum*
         T:set! T:if
         T:λ T:*app*
         T:block
         T:let T:local
         T:cond T:when T:while
         T:breakable T:continuable T:returnable
         T:and T:or
         Ts ; List of all the transformations, defined after all of them.
         standard-library
         M0→L0)

(require "A2.L0.rkt")

; Compile an M0 expression to an L0 expression.
(define (M0→L0 e)
  (expand (standard-library e) Ts))

#| Language M0
   ===========

 M0 is really a language and standard library: the language is essentially an extension of L0,
  that macro-compiles to L0, along with a small standard library written in that language.

 M0 is meant to be for humans to write programs in, so we won't tag it.

 There are seventeen kinds of expression, corresponding to the provides of the form ‘T:<id>’ above.
 For each of those, except T:*id*, T:*datum*, and T:*app*, there's an M0 expression (<id> <part> ...). TODO: What (<id> <part>) ?

 Function application is then any other expression of the form: (<part> ...).
   During expansion, the macro system turns that into (*app* <id>), for T:*app* to transform.

 A simple identifier is an M0 expression meaning variable access.
   During expansion, the macro system turns that into (*id* <id>), for T:*id* to transform.

 An integer is an M0 expression meaning a constant.
  During expansion, the macro system turns that into (*datum* <id>), for T:*datum* to transform.

 It's assumed that M0 programmers will not use identifiers surrounded by asterisks. TODO Why does that matter?:|#

; An M0 expression:
#;(λ (f a b)
    (set! a b)
    (f a 488))
; During expansion, when some parts of that are expanded, they'll be treated as if that was:
#;(λ (f a b)
    (set! a (*id* b))
    (*app* (*id* f) (*id* a) (*datum* 488)))

#| Compiling M0 to L0
   ==================

 Implement:

   1. The transformers mentioned in the ‘provide’ above, for the forms described below.

   2. The standard library, which is a function that attaches the library to an expression,
       described at the end of this file.

 Each transformer ‘T:<id>’ transforms code of the form ‘(<id> <e> ...)’.

 In the given patterns:
   ‘...’  means zero or more of the previous component
   ‘...+’ means one  or more of the previous component

 Other than T:*id*, T:*datum*, T:set!, T:*if*, T:*λ*, and T:*app*, transformers should not
  directly produce L0 forms.

 New identifiers introduced by transformations
 ---------------------------------------------
 In some of your templates you will find that you need to make up the name of an identifier.

 For new “dummy” identifiers that aren't referenced, use ‘_’.
   • we'll assume that user code does not reference any identifier with that name either

 For temporary identifiers that are referenced, use an identifier of the form "*<id>*",
  i.e. surrounded by asterisks. But don't use the names ‘*app*’, ‘*id*’, nor ‘*datum*’.
   • it will be assumed that user code does not use such identifiers |#

(module+ test (require rackunit))

; *id* *datum* set! if
; --------------------
; The following two special cases should expand to integers 0 and 1:
#;(*id* false)
#;(*id* true)
; Otherwise, they are straightforward untagged versions of L0 expressions.
; Transform those directly to their L0 form.

(define-transformer T:*id* *id*
  [`(*id* false) `(L0: datum 0)]
  [`(*id* true)  `(L0: datum 1)]
  [`(*id* ,name) `(L0: var ,name)])

(module+ test
  (check-equal?
   ((transformer-function T:*id*) '(*id* x))
   '(L0: var x)))

(define-transformer T:*datum* *datum*
  [`(*datum* ,e) `(L0: datum ,e)])

(module+ test
  (check-equal?
   ((transformer-function T:*datum*) '(*datum* 99))
   '(L0: datum 99)))

(define-transformer T:set! set!
  [`(set! ,id ,e) `(L0: set! ,id ,e)])

(module+ test
  (check-equal?
   ((transformer-function T:set!) '(set! x (*datum* 99)))
   '(L0: set! x (*datum* 99))))

(define-transformer T:if if
  [`(if ,e1 ,e2 ,e3) `(L0: if ,e1 ,e2 ,e3)])

(module+ test
  (check-equal?
   ((transformer-function T:if) '(if 0 10 11))
   '(L0: if 0 10 11)))

(module+ test
  (check-equal?
   (expand '(set! x (*datum* 99)) (list T:*datum* T:*id* T:set!))
   '(L0: set! x (L0: datum 99))))

; For fun
#;(define-transformer T:>> >>
    (`(>> ,e1) `(*app* (λ () ,e1)))
    (`(>> ,e1 ,e2 ...) `(*app* (λ ()
                                 (*app* (λ () ,e1))
                                 ,(append '(>> ) e2)))
                       ))
#;(define-transformer T:>>= >>=
    (`(>> ,e1) `(*app* (λ () ,e1)))
    (`(>> ,e1 ,e2 ...) `(*app* (λ ()
                                 (*app* (λ () ,e1))
                                 ,(append '(>> ) e2)))
                       ))

; λ
; -
; Extends L0's λ by:
;   allowing more than one body expression
;   allowing zero parameters, and more than one parameter
;
; Transform within the M0 language by wrapping the body in a ‘block’,
;  adding a dummy parameter if there are zero parameters,
;  and currying if there are two or more parameters.
; Transform the unary single-body-expression form to the L0 form.

(define-transformer T:λ λ
  [`(λ (,id) ,body ...)
   `(L0: λ (,id) ,(append '(block) body))]
  [`(λ () ,body ...)
   (append `(λ (_)) body)]
  [`(λ (,id ...) ,body ...)
   `(λ (,(first id))
      ,(append `(λ ,(rest id)) body))])

(module+ test
  (check-equal? ((transformer-function T:λ) '(λ (x) (*datum* 98)))
                '(L0: λ (x) (block (*datum* 98))))
  (check-equal? (expand '(λ (x) (*datum* 98)) Ts)
                '(L0: λ (x) (L0: datum 98)))
  (check-equal? ((transformer-function T:λ) '(λ () (*datum* 98)))
                '(λ (_) (*datum* 98)))
  (check-equal? (expand '(λ () (*datum* 98)) Ts)
                '(L0: λ (_) (L0: datum 98)))
  (check-equal? (expand '(λ () (*datum* 98) (*datum* 99)) Ts)
                '(L0: λ (_) (L0: app (L0: λ (_) (L0: datum 99)) (L0: datum 98))))
  (check-equal? ((transformer-function T:λ) '(λ (a b c) (*datum* 99)))
                '(λ (a)
                   (λ (b c)
                     (*datum* 99))))
  (check-equal? (expand '(λ (a b c) (*datum* 99)) Ts)
                '(L0: λ (a)
                      (L0: λ (b)
                           (L0: λ (c)
                                (L0: datum 99))))))

; *app*
; -----
; Extends L0's app by allowing zero arguments, or more than one argument.
; Transform the form with more than one argument into its curried equivalent.
; Transform the no-argument form into a one-argument form with a dummy argument [see ‘block’].
; Transform the unary form to the L0 form.

(define-transformer T:*app* *app*
  [`(*app* ,f ,a) `(L0: app ,f ,a)]
  [`(*app* ,f) `(L0: app ,f (block))]
  [`(*app* ,f ,a ...) (append `(*app* (*app* ,f ,(first a))) (rest a))])

(module+ test
  (check-equal? ((transformer-function T:*app*) '(*app* (λ (x) (*datum* 98)) (*datum* 99)))
                '(L0: app (λ (x) (*datum* 98)) (*datum* 99)))
  (check-equal? (expand '(*app* (λ (x y) (*datum* 98)) (*datum* 99)) Ts)
                '(L0: app (L0: λ (x) (L0: λ (y) (L0: datum 98))) (L0: datum 99)))
  (check-equal? ((transformer-function T:*app*) '(*app* (λ (x y) (*datum* 98)) (*datum* 99) (*datum* 100)))
                '(*app* (*app* (λ (x y) (*datum* 98)) (*datum* 99)) (*datum* 100)))
  (check-equal? (expand '(*app* (λ (x y) (*datum* 98)) (*datum* 99) (*datum* 100)) Ts)
                '(L0: app (L0: app (L0: λ (x) (L0: λ (y) (L0: datum 98))) (L0: datum 99))
                      (L0: datum 100)))
  (check-equal? (expand '(*app* (λ (x y) (*datum* 0) (*datum* 1)) (*datum* 99) (*datum* 100)) Ts)
                '(L0: app
                      (L0: app
                           (L0: λ (x)
                                (L0: λ (y)
                                     (L0: app
                                          (L0: λ (_) (L0: datum 1))
                                          (L0: datum 0))))
                           (L0: datum 99))
                      (L0: datum 100)))
  (check-equal? (expand '(*app* (λ (a) (*datum* 98)) (*datum* 99)) Ts)
                '(L0: app (L0: λ (a) (L0: datum 98)) (L0: datum 99)))
  (check-equal? (expand '(*app* (λ (a b) (λ () (*datum* 98))) (λ (c d) (*datum* 99))) Ts)
                '(L0: app (L0: λ (a) (L0: λ (b) (L0: λ (_) (L0: datum 98))))
                      (L0: λ (c) (L0: λ (d) (L0: datum 99)))))
  (check-equal? (expand '(*app* (λ (x) (block (*datum* 3))) 100) Ts)
                '(L0: app (L0: λ (x) (L0: datum 3)) (L0: datum 100))))

; block
; -----
#;(block <e>
         ...)
; A sequence of zero or more expressions to be evaluated in order,
;  producing the value of the last expression,
;  or the integer 0 if there are none.
;
; Transform the form with no expressions to the integer 0.
; Transform the form with one expression to just the expression.
; Transform the form with more than one expression to a ‘let’ naming the first expression
;  with a dummy variable.
;
; For other M0 forms that need dummy values [e.g. as mentioned for *app*], use (block) for
;  the dummy value.

(define-transformer T:block block
  [`(block ,e ...) #:when (empty? e) 0]
  [`(block ,e ...) #:when (> (length e) 1 ) `(let ([_ ,(first e)]) ,(append '(block) (rest e)))]
  [`(block ,e) e])

(module+ test
  (check-equal? ((transformer-function T:block) '(block)) 0)
  (check-equal? ((transformer-function T:block) '(block (*datum* 99)))
                '(*datum* 99))
  (check-equal? ((transformer-function T:block) '(block (*datum* 99) (*datum* 100)))
                '(let ([_ (*datum* 99)]) (block (*datum* 100)))))

(module+ test
  (check-equal? ((transformer-function T:block)
                 '(block (*datum* 99)
                         (*datum* 100)))
                '(let ([_ (*datum* 99)])
                   (block (*datum* 100))))
  (check-equal? ((transformer-function T:block) `(block)) 0)
  #;(check-equal? (expand '(>> (*datum* 1) (*datum* 2)) Ts)
                  '(L0: app
                        (L0: λ (_)
                             (L0: app
                                  (L0: λ (_) (L0: app (L0: λ (_) (L0: datum 2)) (L0: datum 0)))
                                  (L0: app (L0: λ (_) (L0: datum 1)) (L0: datum 0))))
                        (L0: datum 0))))

; let
; ---
#;(let ([<id> <init>]
        ...+)
    <body>
    ...+)
; Evaluates the <init>s in order, then introduces the distinctly named local variables <id>s,
;  initialized by the values of the <init>s, then evaluates the <body>s as a block.
;
; Transform using the standard LC transformation: to an expression that makes and immediately calls
;  a function.

(define-transformer T:let let
  [`(let ([,id ,init]
          ..1)
      ,body
      ..1)
   `((λ ,id . ,body) . ,init)])

(module+ test
  (check-equal?
   (expand '(let ([x 99]) 100) Ts)
   '(L0: app (L0: λ (x) (L0: datum 100)) (L0: datum 99)))
  (check-equal?
   (expand '(let ([x 99] [y 488]) 100) Ts)
   '(L0:
     app
     (L0: app (L0: λ (x) (L0: λ (y) (L0: datum 100))) (L0: datum 99))
     (L0: datum 488)))
  (check-equal?
   (expand '(let ([x 99] [y 488]) ((λ (x y) 0 1) 99 100)) Ts)
   '(L0:
     app
     (L0: app (L0: λ (x) (L0: λ (y) (L0: app (L0: app (L0: λ (x) (L0: λ (y) (L0: app (L0: λ (_) (L0: datum 1)) (L0: datum 0)))) (L0: datum 99)) (L0: datum 100)))) (L0: datum 99))
     (L0: datum 488))))

; local
; -----
#;(local [(define (<f-id> (<id> ...))
            <f-body>
            ...+)
          ...+]
    <body>
    ...+)
; Introduces the distinctly named local <f-id>s into scope, to functions created in that scope,
;  then evaluates the <body>s as a block.
;
; Transform using the standard LC+set! transformation: to an expression that initializes
;  all the <f-id>s to dummy values, sets them to their functions, then evaluates the body.

(define-transformer T:local local
  [`(local [(define (,f-id ,id ..1)
              ,f-body ..1)
            ..1]
      ,body
      ..1)
   (append `(let ,(map (λ (fid) `(,fid (block))) f-id) .
              ,(map (λ (fid id f-body) `(set! ,fid (λ ,id . ,f-body))) f-id id f-body))
           `((block . ,body)))])

(module+ test
  (check-equal? (expand '(local [(define (f a b) (a 99))]
                           (f 100 101)) Ts)
                '(L0:
                  app
                  (L0: λ (f)
                       (L0: app
                            (L0: λ (_) (L0: app (L0: app (L0: var f) (L0: datum 100)) (L0: datum 101)))
                            (L0: set! f (L0: λ (a) (L0: λ (b) (L0: app (L0: var a) (L0: datum 99)))))))
                  (L0: datum 0)))
  
  (check-equal? ((transformer-function T:local) '(local [(define (f a) 99)
                                                         (define (g c) 101)]
                                                   100))
                '(let ((f (block)) (g (block))) (set! f (λ (a) 99)) (set! g (λ (c) 101)) (block 100)))
  
  (check-equal? (expand '(local [(define (f a) 99)
                                 (define (g c) 101)]
                           100)
                        Ts)
                '(L0:
                  app
                  (L0:
                   app
                   (L0: λ (f)
                        (L0: λ (g)
                             (L0: app
                                  (L0: λ (_) (L0: app (L0: λ (_) (L0: datum 100)) (L0: set! g (L0: λ (c) (L0: datum 101)))))
                                  (L0: set! f (L0: λ (a) (L0: datum 99))))))
                   (L0: datum 0))
                  (L0: datum 0))))



; and or
; ------
#;(and <e0> <e> ...+)
#;(or  <e0> <e> ...+)
; Standard short-circuiting operators for two or more boolean expressions.
; Transform to ‘if’s or ‘cond’s.

(define-transformer T:and and
  [`(and ,e1 ,e2) `(L0: if ,e1
                        (if ,e2
                            (*id* true)
                            (*id* false))
                        (*id* false))]
  [`(and ,e1 ,e2 ...)
   `(L0: if ,e1
         ,(append `(and) e2)
         (*id* false))])

(define-transformer T:or or
  [`(or ,e1 ,e2) `(L0: if ,e1
                       (*id* true)
                       (if ,e2
                           (*id* true)
                           (*id* false)))]
  [`(or ,e1 ,e2 ...)
   `(L0: if ,e1
         (*id* true)
         ,(append `(or) e2))])

(module+ test
  (check-equal? ((transformer-function T:and) '(and 1 0 1))
                `(L0: if 1
                      (and 0 1)
                      (*id* false)))
  (check-equal? ((transformer-function T:or) '(or 1 0 1))
                `(L0: if 1
                      (*id* true)
                      (or 0 1))))


; cond
; ----
#;(cond [<condition> <result>
                     ...+]
        ...+)
#;(cond [<condition> <result>
                     ...+]
        ...
        [else <else-result>
              ...+])
; Evaluates the boolean <condition>s in order, until the first true one or possibly the else,
;  then evaluates the corresponding <result>s as a block.
;
; Transform using ‘if’s, ‘when’s, and/or ‘block’s.




(define-transformer T:cond cond
  [`(cond [,cond ,res ...])
   (append `(when ,cond)
           res)] 
  [`(cond [,cond ,res ...]
          ,conds ...
          [else ,else-res])
   (append `(cond (,cond . ,res))
           conds
           `((1 ,else-res)))]
  [`(cond [,cond ,res ...]
          ,conds ...)
   `(L0: if ,cond
         ,(append '(block) res)
         ,(append '(cond) conds))])

(module+ test
  (check-equal? ((transformer-function T:cond) '(cond [(*datum* 0) (*datum 99)]))
                '(when (*datum* 0)
                   (*datum 99)))
  (check-equal? ((transformer-function T:cond)
                 '(cond [(*datum* 0) (*datum* 99)]
                        [(*datum* 1) (*datum* 100)]
                        [(*datum* 2) (*datum* 102)]))
                '(L0: if (*datum* 0)
                      (block (*datum* 99))
                      (cond [(*datum* 1) (*datum* 100)]
                            [(*datum* 2) (*datum* 102)])))
  (check-equal? ((transformer-function T:cond)
                 '(cond [(*datum* 0) (*datum* 99)]
                        [(*datum* 1) (*datum* 100)]
                        [else (*datum* 102)]))
                '(cond [(*datum* 0) (*datum* 99)]
                        [(*datum* 1) (*datum* 100)]
                        [1 (*datum* 102)])))
; when
; ----
#;(when <condition>
    <body>
    ...+)
; If boolean <condition> is true evaluates the <body>s as a block, otherwise produces a dummy value.

(define-transformer T:when when
  [`(when ,<condition>
      ,<body>
      ...)
   (append `(L0: if ,<condition>)
           (list (append '(block) <body>))
           `((block)))])
(module+ test
  (check-equal? ((transformer-function T:when) '(when (*datum* 0) (*datum* 99) (*datum* 100)))
                '(L0: if (*datum* 0)
                      (block (*datum* 99)
                             (*datum* 100))
                      (block))))
(module+ test
  (check-equal? (expand '(*app*
                          (λ () (cond [(and (*id* true) (*id* true)) (*datum* 42)])))
                        Ts)
                '(L0: app
                      (L0: λ (_) 
                           (L0: if
                                ; and
                                (L0: if (L0: datum 1)
                                     (L0: if (L0: datum 1)
                                          (L0: datum 1)
                                          (L0: datum 0))
                                     (L0: datum 0))
                                (L0: datum 42)
                                (L0: datum 0)))
                      (L0: datum 0))))

; while
; -----
#;(while <condition>
         <body>
         ...+)
; A standard while loop.
; Transform to a recursive no-argument function that is immediately called.

(define-transformer T:while while
  [`(while ,<condition>
           ,<body>
           ,<body2>
           ...)
   `(local ([(define (f (_))
               (L0: if ,<condition>
                    (block)
                    ,(append `(block
                               ,<body>
                               )
                             <body2>
                             '((*app* (*id* f))))))])
      (*app* (*id* f)))])

(module+ test
  (check-equal? ((transformer-function T:while)
                 '(while (*id* true)
                         (*datum* 99)
                         (*datum* 100)))
                '(local ([(define (f (_)) (L0: if (*id* true)
                                               (block)
                                               (block
                                                (*datum* 99)
                                                (*datum* 100)
                                                (*app* (*id* f)))))])
                   (*app* (*id* f)))))


; returnable breakable continuable
; --------------------------------
#;(returnable <e>
              ...+)
#;(breakable <e>
             ...+)
#;(continuable <e>
               ...+)
; Evaluates the <e>s as a block, in a local scope containing the identifier ‘return’,
;  ‘break’, or ‘continue’ bound to the continuation that escapes the entire expression.
; These are meant to be used manually by the programmer: around a function body, loop, or loop body,
;  to return early, break, or continue.

(define-transformer T:returnable returnable
  [`(returnable ,e
                ...)
   `(call/ec (λ (return) . ,e))])

(define-transformer T:breakable breakable
  [`(breakable ,e
               ...)
   `(call/ec (λ (break) . ,e))])

(define-transformer T:continuable continuable
  [`(continuable ,e
                 ...)
   `(call/ec (λ (continue) . ,e))])


; List of all the transformations.
(define Ts (list T:*id* T:*datum*
                 T:set! T:if
                 T:λ T:*app* 
                 T:block
                 T:let T:local
                 T:cond T:when T:while
                 T:breakable T:continuable T:returnable
                 T:and T:or))

; Standard Library
; ----------------
; Add definitions for the functions described by the comments in the body.
(define (standard-library e)
  `(local [
           ; Boolean logic
           ; -------------
           ; (not b) : the negation of b, implemented with ‘if’
           (define (not b) (if b 0 1)) ; Added to testsuite
           ; Arithmetic
           ; ----------
           ; (⊖ a) : the negative of a
           (define (⊖ a) (* -1 a)) ; Added to testsuite
           ; (- a b) : the difference between a and b
           (define (- a b) (+ a (⊖ b))) ; Added to testsuite
           ; (= a b) : whether a is equal to b
           ;(define (= a b) (if (and (not (< a b)) (not (< (- b a) 0))) 1 0))
           ; (not (< (- 5 1) 0)) :== (not (< 252 0)) because $-2 wraps around in x86. TODO: How to represent negative integers in x86?
           (define (= a b) (if (and (not (< a b)) (not (< (⊖ a) (⊖ b)))) 1 0)) ; Added to testsuite
           ; (> a b) : whether a is greater than b
           #;(define (> a b) (if (and (not (< a b)) (not (= a b))) 1 0)) ; Added to testsuite
           ; (>= a b) : whether a is greater than or equal to b
           (define (>= a b) (if (not (< a b)) 1 0)) ; Added to testsuite
           ]
     ,e))
