#lang racket
;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
(provide run compiler)
(define-syntax newconf
  (syntax-rules ()
    [(_) (hasheq)]
    [(_ [c v] x ...) (hash-set (newconf x ...) (quote c) v)]
    [(_ c x ...) (hash-set (newconf x ...) (quote c) #t)]))
(define (conf-get c x) (hash-ref c x #f))

(define preludes '())
(define-syntax-rule (prelude get x)
  (set! preludes
        (cons (λ (c)
                (let ([get (λ (v) (conf-get c v))])
                  x))
              preludes)))
(define (runprelude conf)
  (foldl append '() (map (λ (f) (f conf)) preludes)))

(prelude
 get
 '((defmacro and
     (λ xs
       (if (null? xs)
           #t
           (if (null? (cdr xs))
               (car xs)
               (let ([s (gensym)])
                 `(let ([,s ,(car xs)])
                    (if ,s
                        (and ,@(cdr xs))
                        #f)))))))
   (defmacro or
     (λ xs
       (if (null? xs)
           #f
           (if (null? (cdr xs))
               (car xs)
               (let ([s (gensym)])
                 `(let ([,s ,(car xs)])
                    (if ,s
                        ,s
                        (or ,@(cdr xs)))))))))
   (defmacro let
     (λ (p . xs)
       `((λ ,(map car p)
           ,@xs) ,@(map second p))))
   (defmacro letrec
     (λ (p . xs)
       `(begin
          ,@(map
             (λ (x)
               `(define ,(car x) ,(second x)))
             p)
          ,@xs)))
   (defmacro let*
     (λ (p . xs)
       (if (null? p)
           `(begin ,@xs)
           (let ([x (car p)])
             `(let ([,(car x) ,(second x)])
                (let* ,(cdr p) ,@xs))))))
   (defmacro cond
     (λ xs
       (if (null? xs)
           `(error "cond")
           (let ([c (car xs)])
             (let ([g (car c)] [v (cdr c)])
               (if (eq? g 'else)
                   `(begin ,@v)
                   `(if ,g
                        (begin ,@v)
                        (cond ,@(cdr xs)))))))))
   ))

(prelude
 get
 '((defmacro define-record-type
     (λ (name constructor pred . fs)
       (define (mkc fs)
         (if (null? fs)
             '()
             (cons
              (match (car fs)
                [`(,x ,a) x]
                [`(,x ,a ,set) `(atom! ,x)])
              (mkc (cdr fs)))))
       (define (deffs name pred c fs)
         (if (null? fs)
             '()
             (append
              (match (car fs)
                [`(,f ,a)
                 `((define (,a x)
                     (if (,pred x)
                         (_vec_ref_ x ,c)
                         (error ,(symbol->string name) ,(symbol->string a) x))))]
                [`(,f ,a ,set)
                 `((define (,a x)
                     (if (,pred x)
                         (atom-get (_vec_ref_ x ,c))
                         (error ,(symbol->string name) ,(symbol->string a) x)))
                   (define (,set x v)
                     (if (,pred x)
                         (atom-set! (_vec_ref_ x ,c) v)
                         (error ,(symbol->string name) ,(symbol->string a) x))))])
              (deffs name pred (+ 1 c) (cdr fs)))))
       `(begin
          (define ,constructor
            (vector
             (cons '_%struct%_ (quote ,name))
             ,@(mkc fs)))
          (define (,pred x)
            (and
             (struct? x)
             (eq? (cdr (_vec_ref_ x 0))
                  (quote ,name))))
          ,@(deffs name pred 1 fs))))
   (define (struct? x)
     (and (_vec?_ x)
          (not (_vec_len_0?_ x))
          (let ([x (_vec_ref_ x 0)])
            (and (pair? x)
                 (eq? (car x) '_%struct%_)))))
   (define (_struct_type_ x)
     (if (struct? x)
         (cdr (_vec_ref_ x 0))
         (error "isn't struct" x)))
   (define (struct->list x)
     (if (struct? x)
         (cdr (_vec->lst_ x))
         (error "struct->list: isn't struct" x)))
   (define (vector? x) (and (_vec?_ x) (not (struct? x))))
   (define (vector-length v)
     (if (vector? v)
         (_vec_len_ v)
         (error "vector-length: isn't vector" x)))
   (define (vector-ref v i)
     (if (vector? x)
         (_vec_ref_ v i)
         (error "vector-ref: isn't vector" x)))
   (define (vector->list v)
     (if (vector? v)
         (_vec->lst_ v)
         (error "vector->list: isn't vector" v)))

   (defmacro struct
     (λ (name fs . c)
       `(define-record-type ,name
          (,name ,@(map (λ (x) (if (symbol? x) x (car x))) fs))
          ,(string->symbol (string-append (symbol->string name) "?"))
          ,@(map
             (λ (f)
               (if (symbol? f)
                   `(,f ,(string->symbol
                          (string-append (symbol->string name) "-" (symbol->string f))))
                   (let ([f (car f)]) ;mutable
                     (let ([g (string-append (symbol->string name) "-" (symbol->string f))])
                       `(,f ,(string->symbol g)
                            ,(string->symbol
                              (string-append "set-" g "!")))))))
             fs))))
   ))

(prelude
 get
 '((define (null? x) (__null? x))
   (define (raise x) (__raise x))
   (define (with-exception-handler handler thunk)
     (if (procedure? handler)
         (if (procedure? thunk)
             (__with-exception-handler handler thunk)
             (error "with-exception-handler: isn't procedure" thunk))
         (error "with-exception-handler: isn't procedure" handler)))
   (define (cons a d) (__cons a d))
   (define (procedure? x) (__procedure? x))
   (define (number? x) (__number? x))
   (define (number->string x)
     (if (number? x)
         (__number->string x)
         (error "number->string: isn't number" x)))
   (define (string->number x)
     (if (string? x)
         (__string->number x)
         (error "string->number: isn't string" x)))
   (define (string-append x y)
     (if (string? x)
         (if (string? y)
             (__string-append x y)
             (error "string-append: isn't string" y))
         (error "string-append: isn't string" x)))
   (define (string? x) (__string? x))
   (define (symbol? x) (__symbol? x))
   (define (boolean? x) (__boolean? x))
   (define (symbol->string x)
     (if (symbol? x)
         (__symbol->string x)
         (erro "symbol->string: isn't symbol" x)))
   (define (string->symbol x)
     (if (string? x)
         (__string->symbol x)
         (error "string->symbol: isn't string" x)))
   (define (apply f xs)
     (if (procedure? f)
         (if (list? xs)
             (__apply f xs)
             (error "apply: isn't list" xs))
         (error "apply: isn't procedure" f)))
   (define (car p)
     (if (pair? p)
         (__car p)
         (error "car: isn't pair" p)))
   (define (cdr p)
     (if (pair? p)
         (__cdr p)
         (error "cdr: isn't pair" p)))

   (define (error . xs) (raise (cons 'error xs)))
   (define (not x) (if x #f #t))
   (define (string . xs) (list->string xs))
   (define (displayln x) (display x) (newline))
   (define eqv? equal?)
   (defmacro delay-force
     (λ (x)
       `(delay (force ,x))))
   (define (make-promise x) (if (promise? x) x (delay x)))
   (define (hash-update hash key updater . f)
     (hash-set
      hash
      key
      (updater
       (if (null? f)
           (hash-ref hash key)
           (hash-ref hash key (car f))))))
   (define (memroizeeq f) f) ; zaoqil-core
   (define (memorize1eq f) f) ; zaoqil-core

   (define (zero? x) (eq? x 0))
   (define (positive? x) (> x 0))
   (define (negative? x) (< x 0))
   (define (%max x y) (if (> x y) x y))
   (define (max x . xs) (foldl %max x xs))
   (define (%min x y) (if (< x y) x y))
   (define (min x . xs) (foldl %min x xs))

   (define (list . xs) xs)
   (define (list? xs) (or (null? xs) (and (pair? xs) (list? (cdr xs)))))
   (define (map f xs)
     (if (null? xs)
         '()
         (cons (f (car xs)) (map f (cdr xs)))))
   (define (append xs ys)
     (if (null? xs)
         ys
         (cons (car xs) (append (cdr xs) ys))))
   (define (filter f xs)
     (if (null? xs)
         '()
         (if (f (car xs))
             (cons (car xs) (filter f (cdr xs)))
             (filter f (cdr xs)))))
   (define (foldl f x xs)
     (if (null? xs)
         x
         (foldl f (f (car xs) x) (cdr xs))))
   (define (length xs)
     (if (null? xs)
         0
         (+ 1 (length (cdr xs)))))
   (define (list-ref xs i)
     (if (zero? i)
         (car xs)
         (list-ref (cdr xs) (- i 1))))
   (define (%reverse xs rs)
     (if (null? xs)
         rs
         (%reverse (cdr xs) (cons (car xs) rs))))
   (define (reverse xs)
     (%reverse xs '()))
   (define (member x xs)
     (if (null? xs)
         #f
         (or (equal? (car xs) x) (member x (cdr xs)))))
   (define (ormap f xs)
     (if (null? xs)
         (or)
         (or (f (car xs)) (ormap f (cdr xs)))))
   (define (andmap f xs)
     (cond
       [(null? xs) (and)]
       [(null? (cdr xs)) (and (f (car xs)))]
       [else (and (f (car xs)) (andmap f (cdr xs)))]))
   (define (caar x) (car (car x)))
   (define (cadr x) (car (cdr x)))
   (define (cdar x) (cdr (car x)))
   (define (cddr x) (cdr (cdr x)))
   (define (caaar x) (car (car (car x))))
   (define (caadr x) (car (car (cdr x))))
   (define (cadar x) (car (cdr (car x))))
   (define (caddr x) (car (cdr (cdr x))))
   (define (cdaar x) (cdr (car (car x))))
   (define (cdadr x) (cdr (car (cdr x))))
   (define (cddar x) (cdr (cdr (car x))))
   (define (cdddr x) (cdr (cdr (cdr x))))
   (define first car)
   (define second cadr)
   (define third caddr)
   (define (assf f xs)
     (if (null? xs)
         #f
         (if (f (caar xs))
             (car xs)
             (assf f (cdr xs)))))
   (define (assoc x xs) (assf (λ (y) (equal? x y)) xs))

   (define gensym% (atom! 0))
   (define (gensym) (atom-map! (λ (x) (+ x 1)) gensym%))
   ))

(prelude
 get
 (if (get 'vector)
     '((define (pair? x) (__pair? x))
       (define vector __vector)
       (define (_vec?_ x) (__vector? x))
       (define (_vec_len_ x) (__vector-length x))
       (define (_vec_ref_ vector k) (__vector-ref vector k))
       (define (list->vector x)
         (if (list? x)
             (__list->vector x)
             (error "list->vector: isn't list" x)))
       (define (_vec->lst_ x) (__vector->list x))
       (define (_vec_len_0?_ v) (zero? (_vec_len_ v)))
       )
     '((define (pair? x) (and (__pair? x) (not (_vec?_ x))))
       (define (vector . xs) (cons '_%vec%_ xs))
       (define (_vec?_ x) (and (__pair? x) (eq? (__car x) '_%vec%_)))
       (define (_vec_len_ x) (length (__cdr x)))
       (define (_vec_ref_ x i) (list-ref (__cdr x) i))
       (define (list->vector xs) (cons '_%vec%_ xs))
       (define (_vec->lst_ x) (__cdr x))
       (define (_vec_len_0?_ x) (null? (_vec->lst_ x)))
       )))

(prelude
 get
 (if (get 'equal)
     '((define (eq? x y) (__equal? x y))
       (define (equal? x y) (__equal? x y))
       )
     '((define (eq? x y) (__eq? x y))
       (define (equal? x y)
         (cond
           [(eq? x y) #t]
           [(pair? x) (and (pair? y)
                           (equal? (car x) (car y))
                           (equal? (cdr x) (cdr y)))]
           [(_vec?_ x) (and (_vec?_ y)
                            (equal? (_vec->lst_ x) (_vec->lst_ y)))]
           [else #f]))
       )))

(prelude
 get
 (if (get '+-*/<>=)
     '((define < __<)
       (define > __>)
       (define = __=)
       (define <= __<=)
       (define >= __>=)
       (define + __+)
       (define - __-)
       (define * __*)
       (define / __/)
       )
     (let loop ([c '((define (_+_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__+/2 x y)
                               (error "+: isn't number" y))
                           (error "+: isn't number" x)))
                     (define (_-_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__-2 x y)
                               (error "-: isn't number" y))
                           (error "-: isn't number" x)))
                     (define (_*_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__*2 x y)
                               (error "*: isn't number" y))
                           (error "*: isn't number" x)))
                     (define (_/_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__/2 x y)
                               (error "/: isn't number" y))
                           (error "/: isn't number" x)))
                     (define (+ . xs) (foldl _+_ 0 xs))
                     (define (* . xs) (foldl _*_ 1 xs))
                     (define (- x . xs)
                       (if (null? xs)
                           (_-_ 0 x)
                           (foldl (λ (n x) (_-_ x n)) x xs)))
                     (define (/ x . xs)
                       (if (null? xs)
                           (_/_ 1 x)
                           (foldl (λ (n x) (_/_ x n)) x xs))))]
                [ops (list
                      (cons '< '__<2)
                      (cons '> '__>2)
                      (cons '= 'eq?)
                      (cons '<= '__<=2)
                      (cons '>= '__>=2))])
       (if (null? ops)
           c
           (let* ([op (car ops)]
                  [f (cdr op)]
                  [fn (gensym f)]
                  [d (car op)]
                  [doop (gensym d)])
             (loop
              (append
               `((define (,fn x y)
                   (if (number? x)
                       (if (number? y)
                           (,f x y)
                           (error "isn't number" y))
                       (error "isn't number" x)))
                 (define (,doop x y xs)
                   (if (null? xs)
                       (,fn x y)
                       (and (,fn x y) (,doop y (car xs) (cdr xs)))))
                 (define (,d x y . xs) (,doop x y xs)))
               c)
              (cdr ops)))))))

(prelude
 get
 (if (get 'atom)
     (match (get 'atom)
       [#t
        '((define (atom! x) (__atom! x))
          (define (atom-get x)
            (if (atom? x)
                (__atom-get x)
                (error "atom-get: isn't atom" x)))
          (define (atom-set! a x)
            (if (atom? a)
                (begin
                  (__atom-set! a x)
                  (void))
                (error "atom-set!: isn't atom" a)))
          (define (atom-map! f a)
            (if (atom? a)
                (if (procedure? f)
                    (__atom-map! f a)
                    (error "atom-map!: isn't procedure" f))
                (error "atom-map!: isn't atom" a)))
          (define (atom? x) (__atom? x)))]
       ['set!
        '((define-record-type atom
            (%atom! get set)
            atom?
            (get %atom-get)
            (set %atom-set!))
          (define (atom! x)
            (define v x)
            (%atom! (λ () v) (λ (nx) (set! v nx))))
          (define (atom-get x) ((%atom-get x)))
          (define (atom-set! x v) ((%atom-set! x) v))
          (define (atom-map! f x)
            (let ([r (f (atom-get x))])
              (atom-set! x r)
              r)))])
     '((define atom! (error "atom"))
       (define atom-get (error "atom"))
       (define atom-set! (error "atom"))
       (define atom-map! (error "atom"))
       (define atom? #f)
       )))

(prelude
 get
 (if (get 'display)
     '((define (_putstr_ x) (__putstr x))
       (define (display x)
         (cond
           [(string? x) (_putstr_ x)]
           [(symbol? x) (_putstr_ (symbol->string x))]
           [(number? x) (_putstr_ (number->string x))]
           [(boolean? x) (if x (_putstr_ "#t") (_putstr_ "#f"))]
           [(char? x) (_putstr_ (string x))]
           [(struct? x) (begin
                          (_putstr_ "(")
                          (_putstr_ (symbol->string (_struct_type_ x)))
                          (%dis%* (struct->list x))
                          (_putstr_ ")"))]
           [(pair? x) (begin
                        (_putstr_ "(")
                        (display (car x))
                        (%dis%* (cdr x))
                        (_putstr_ ")"))]
           [(vector? x) (begin
                          (_putstr_ "#")
                          (display (vector->list x)))]
           [(atom? x) (begin
                        (_putstr_ "#<atom:")
                        (display (atom-get x))
                        (_putstr_ ">"))]
           [else (error "display" x)]))
       (define (%dis%* x)
         (if (null? x)
             (void)
             (if (pair? x)
                 (begin
                   (_putstr_ " ")
                   (display (car x))
                   (%dis%* (cdr x)))
                 (begin
                   (_putstr_ " . ")
                   (display x)))))
       (define (newline) (__newline)))
     '((define (display x) (error "display: can't display" x))
       (define (newline) (error "newline: can't newline" x)))))

(prelude
 get
 (if (get 'void)
     '((define void __void)
       (define void? __void?))
     '((define-record-type void
         (void)
         void?))))

(prelude
 get
 (if (get 'atom)
     '((define-record-type delay-v
         (%delay-v lazy)
         promise?
         (lazy %lazydelay-vv %set%delay!))
       (defmacro delay
         (λ (x)
           `(%delay-v (lambda () ,x))))
       (define (promise-forced? x) (pair? (%lazydelay-vv x)))
       (define (force x)
         (let ([v (%lazydelay-vv x)])
           (if (pair? v)
               (car v)
               (let ([f (v)])
                 (begin
                   (%set%delay! x (list f))
                   f))))))
     '((define-record-type delay-v
         (%delay-v lazy)
         promise?
         (lazy %lazydelay-vv))
       (defmacro delay
         (λ (x)
           `(%delay-v (lambda () ,x))))
       (define (promise-forced? x) #f)
       (define (force x) ((%lazydelay-vv x))))))

(prelude
 get
 (if (get 'hash)
     '((define (make-immutable-hash x) (__make-immutable-hash x))
       (define hash __hash)
       (define (hash->list h)
         (if (hash? h)
             (__hash->list h)
             (error "hash->list: isn't hash" h)))
       (define (hash-set h k v)
         (if (hash? h)
             (__hash-set h k v)
             (error "hash-set: isn't hash" h)))
       (define (hash-ref hash key . f)
         (if (hash-hash-key? hash key)
             (__hash-ref hash key)
             (if (procedure? (car f))
                 ((car f))
                 (car f))))
       (define (hash-hash-key? h k)
         (if (hash? h)
             (__hash-hash-key? h k)
             (error "hash-hash-key?: isn't hash" h))))
     '((define-record-type hash
         (%make-immutable-hash xs)
         hash?
         (xs hash->list))
       (define (make-immutable-hash xs)
         (if (null? xs)
             (%make-immutable-hash '())
             (let ([x (car xs)])
               (hash-set (make-immutable-hash (cdr xs)) (car x) (cdr x)))))
       (define (hash-set hash key v)
         (let ([h (%hash-set hash key (λ (x) v))])
           (if h
               h
               (%make-immutable-hash (cons (cons key v) (hash->list hash))))))
       (define (%hash-set hash key v)
         (%%hash-set (hash->list hash) key v
                     %make-immutable-hash
                     (λ () #f)))
       (define (%%hash-set hash key v s u)
         (if (null? hash)
             (u)
             (let ([x (car hash)])
               (if (equal? (car x) key)
                   (s (cons (cons (car x) v) (cdr hash)))
                   (%%hash-set (cdr hash) key v (λ (r) (cons x r)) u)))))
       (define (hash-ref hash key . f)
         (let ([r (assoc key (hash->list hash))])
           (if r
               (cdr r)
               (if (null? f)
                   (error "hash-ref" hash key)
                   (let ([x (car f)])
                     (if (procedure? x)
                         (x)
                         x))))))
       (define (hash-has-key? hash key)
         (if (hash-ref hash key #f) ; 非boolean,所以要if
             #t
             #f))

       (define (%hash xs)
         (if (null? xs)
             '()
             (cons (cons (car xs) (cadr xs)) (%hash (cddr xs)))))
       (define (hash . xs) (make-immutable-hash (%hash xs))))))
(prelude
 get
 (match (get 'charstr)
   ['nochar ;不能和quote一起使用
    '((define-record-type char
        (%char v)
        char?
        (v %g%char))
      (define (_str->list_ x) (__str->lst x))
      (define (string->list s)
        (if (string? s)
            (map %char (_str->list_ s))
            (error "string->list: isn't string" s)))
      (define (list->string s) (foldl string-append "" (map %g%char s))))]
   [_
    '((define (char? x) (__char? x))
      (define (list->string x) (__list->string x))
      (define (string->list s) (__string->list s)))]))

(require racket/sandbox)
(define evalp (make-evaluator 'racket))
(define (macroexpand macros x)
  (cond
    [(and (pair? x) (eq? (car x) 'defmacro))
     (hash-set! macros (second x) (evalp (third x)))
     '(void)]
    [(and (pair? x) (hash-ref macros (car x) #f)) => (λ (mf) (macroexpand macros (apply mf (cdr x))))]
    [else x]))
(define (c? x)
  (cond
    [(symbol? x) #f]
    [(pair? x) (and (eq? 'quote (car x)) (symbol? (second x)))]
    [else #t]))
(define-syntax %mkfs
  (syntax-rules ()
    [(_) '()]
    [(_ [x0 v] x ...) (cons (cons (quote x0) v) (%mkfs x ...))]
    [(_ x0 x ...) (cons (cons (quote x0) x0) (%mkfs x ...))]))
(define-syntax-rule (mkfs x ...) (make-hash (%mkfs x ...)))
(define fs
  (mkfs
   + - * / < > <= >= = string string-append
   [symbol->string
    (match-lambda
      [`(quote ,v) (symbol->string v)])]
   [string->symbol (λ (x) `(quote ,(string->symbol x)))]
   ))
(define (EVAL conf macros x)
  (let ([x (macroexpand macros x)])
    (cond
      [(pair? x) (APPLY conf macros (car x) (cdr x))]
      [(and (char? x)
            (eq? (conf-get conf 'charstr) 'nochar)) `(%char ,(string x))]
      [else x])))
(define (APPLY conf macros f xs)
  (match f
    [(or 'λ 'lambda) `(lambda ,(car xs) . ,(BEGIN conf macros (cdr xs)))]
    ['begin (if (null? (cdr xs))
                (EVAL conf macros (car xs))
                `(begin ,@(BEGIN conf macros xs)))]
    ['define (error 'APPLY f xs)]
    ['quote (if (conf-get conf 'quote) `(quote ,(car xs)) (QUOTE macros conf (car xs)))]
    [_ (let ([nxs (map (λ (x) (EVAL conf macros x)) xs)])
         (if (and (hash-has-key? fs f) (andmap c? nxs))
             (apply (hash-ref fs f) nxs)
             (cons (EVAL conf macros f) nxs)))]))
(define (DEFINE conf macros f xs)
  (if (symbol? f)
      (if (null? (cdr xs))
          `(define ,f ,(EVAL conf macros (car xs)))
          (raise `(define ,f ,@xs)))
      (DEFINE conf macros (car f) `((λ ,(cdr f) ,@xs)))))
(define (define? x) (and (pair? x) (eq? (car x) 'define)))
(define (lambda? x) (and (pair? x) (eq? (car x) 'lambda)))
(define (begin? x) (and (pair? x) (eq? (car x) 'begin)))
; Symbol -> Exp -> Bool
(define (GCfind? s x)
  (match x
    [`(lambda ,_ ,@v) (GCfind? s v)]
    [(? symbol? x) (eq? s x)]
    [`(define ,_ ,v) (GCfind? s v)]
    [`(quote ,_) #f]
    [(? pair? x) (or (GCfind? s (car x)) (GCfind? s (cdr x)))]
    [_ #f]))
(define (BEGINgc xs)
  (let* ([lastv (last xs)] [lastvoid (or (equal? lastv '(void)) (define? lastv))])
    (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)])
      (let-values ([(rdefs rnotdefs) (partition define? xs)])
        (let ([defs (map (λ (x) (cons (second x) (third x))) rdefs)])
          (let-values ([(marked rest)
                        (partition (λ (x)
                                     (or (and lastvoid (eq? (car x) 'void))
                                         (GCfind? (car x) rnotdefs))) defs)])
            (let loop ([marked marked] [rest rest])
              (if (null? rest)
                  xs
                  (let-values ([(new newrest) (partition (λ (x) (GCfind? (car x) marked)) rest)])
                    (if (null? new)
                        (let ([marked (map car marked)])
                          (let ([xs (filter
                                     (λ (x)
                                       (if (define? x)
                                           (set-member? marked (second x))
                                           #t)) xs)])
                            (if lastvoid
                                (append xs (list '(void)))
                                xs)))
                        (loop (append new marked) newrest)))))))))))
;(set! BEGINgc (λ (x) x))
(define (BEGINappend macros cs)
  (if (null? cs)
      '()
      (let ([c (macroexpand macros (car cs))])
        (if (begin? c)
            (append (BEGINappend macros (cdr c)) (BEGINappend macros (cdr cs)))
            (cons c (BEGINappend macros (cdr cs)))))))
(define (BEGIN conf macros xs)
  (BEGINgc
   (BEGINappend
    macros
    (map
     (λ (x)
       (if (define? x)
           (DEFINE conf macros (cadr x) (cddr x))
           (EVAL conf macros x))) ;这里会GC
     (BEGINappend
      macros
      xs))))) ;在GC前append
(define (QUOTE1 conf x)
  (cond
    [(pair? x) (list 'cons (QUOTE1 conf (car x)) (QUOTE1 conf (cdr x)))]
    [(symbol? x) `(quote ,x)]
    [(null? x) '(quote ())]
    [(and (char? x)
          (eq? (conf-get conf 'charstr) 'nochar)) `(%char ,(string x))]
    [else x]))
(define (%QUOTE max-count conf x count f)
  (if (pair? x)
      (if (> count max-count)
          (let ([v (gensym)])
            (%QUOTE max-count conf (car x) 0
                    (λ (a c2)
                      (%QUOTE max-count conf (cdr x) c2
                              (λ (d c3)
                                `(begin
                                   (define ,v (cons ,a ,d))
                                   ,(f v (+ c3 1))))))))
          (%QUOTE max-count conf (car x) count
                  (λ (a c2)
                    (%QUOTE max-count conf (cdr x) c2
                            (λ (d c3)
                              (f `(cons ,a ,d) (+ c3 1)))))))
      (f (QUOTE1 conf x) (+ count 1))))
(define (QUOTE macros conf x)
  (if (conf-get conf 'split)
      (if (pair? x)
          (EVAL macros conf (%QUOTE (conf-get conf 'split) conf x 0 (λ (x c) x)))
          (QUOTE1 conf x))
      (QUOTE1 conf x)))

(define (run conf xs)
  (EVAL conf (make-hash) (cons 'begin (append (runprelude conf) xs))))

(define-syntax-rule (compiler name [c ...] evalf)
  (define (name p) (evalf (run (newconf c ...) p))))
