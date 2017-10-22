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

   (defmacro define-record-type
     (begin
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
       (λ (name constructor pred . fs)
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
            ,@(deffs name pred 1 fs)))))
   (define (struct? x)
     (and (_vec?_ x)
          (not (_vec_len_0?_ x))
          (let ([x (_vec_ref_ x 0)])
            (and (pair? x)
                 (eq? (car x) '_%struct%_)))))
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
   ))

(prelude
 get
 '((define null? __null?)
   (define error __error)
   (define cons __cons)
   (define procedure? __procedure?)
   (define number? __number?)
   (define char? __char?)
   (define string? __string?)
   (define string->list __string->list)
   (define list->string __list->string)
   (define string __string)

   (define (not x) (if x #f #t))

   (define eqv? equal?)

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
   ))

(prelude
 get
 (if (get 'vector)
     '((define pair? __pair?)
       (define car __car)
       (define cdr __cdr)
       (define vector __vector)
       (define _vec?_ __vector?)
       (define _vec_len_ __vector-length)
       (define _vec_ref_ __vector-ref)
       (define list->vector __list->vector)
       (define _vec->lst_ __vector->list)
       (define (_vec_len_0?_ v) (zero? (_vec_len_ v)))
       )
     '((define (pair? x) (and (__pair? x) (not (_vec?_ x))))
       (define (car p)
         (if (pair? p)
             (__car p)
             (error "car: isn't pair" p)))
       (define (cdr p)
         (if (pair? p)
             (__cdr p)
             (error "cdr: isn't pair" p)))
       (define (vector . xs) (cons '_%vec%_ xs))
       (define (_vec?_ x) (and (__pair? x) (eq? (__car x) '_%vec%_)))
       (define (_vec_len_ x)
         (if (_vec?_ x)
             (length (__cdr x))
             (error "vector-length: isn't vector" x)))
       (define (_vec_ref_ x i)
         (if (_vec?_ x)
             (list-ref (__cdr x) i)
             (error "vector-ref: isn't vector" x)))
       (define (list->vector xs) (cons '_%vec%_ xs))
       (define (_vec->lst_ x)
         (if (_vec?_ x)
             (__cdr x)
             (error "vector->list: isn't vector" x)))
       (define (_vec_len_0?_ x) (null? (_vec->lst_ x)))
       )))

(prelude
 get
 (if (get 'equal)
     '((define eq? __equal?)
       (define equal? __equal?)
       )
     '((define eq? __eq?)
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
     (let loop ([c '((define (+ . xs) (foldl __+2 0 xs))
                     (define (* . xs) (foldl __*2 1 xs))
                     (define (- x . xs)
                       (if (null? xs)
                           (__-2 0 x)
                           (foldl (λ (n x) (__-2 x n)) x xs)))
                     (define (/ x . xs)
                       (if (null? xs)
                           (__/2 1 x)
                           (foldl (λ (n x) (__/2 x n)) x xs))))]
                [ops (list
                      (cons '< '__<2)
                      (cons '> '__>2)
                      (cons '= 'eq?)
                      (cons '<= '__<=2)
                      (cons '>= '__>=2))])
       (if (null? ops)
           c
           (let* ([op (car ops)] [f (cdr op)] [doop (gensym (car op))])
             (loop
              (append
               `((define (,doop x y xs)
                   (if (null? xs)
                       (,f x y)
                       (and (,f x y) (,doop y (car xs) (cdr xs)))))
                 (define (,(car op) x y . xs) (,doop x y xs)))
               c)
              (cdr ops)))))))

(prelude
 get
 (if (get 'atom)
     '((define atom! __atom!)
       (define atom-get __atom-get)
       (define atom-set! __atom-set!)
       (define atom-map! __atom-map!)
       (define atom? __atom?)
       )
     '((define atom! (error "atom"))
       (define atom-get (error "atom"))
       (define atom-set! (error "atom"))
       (define atom-map! (error "atom"))
       (define atom? #f)
       )))

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
      [else x])))
(define (APPLY conf macros f xs)
  (match f
    [(or 'λ 'lambda) `(lambda ,(car xs) . ,(BEGIN conf macros (cdr xs)))]
    ['begin (if (null? (cdr xs))
                (EVAL conf macros (car xs))
                `(begin ,@(BEGIN conf macros xs)))]
    ['define (error 'APPLY f xs)]
    ['quote (if (conf-get conf 'quote) `(quote ,(car xs)) (QUOTE (car xs)))]
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
  (let ([lastv (last xs)])
    (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)])
      (let-values ([(rdefs rnotdefs) (partition define? xs)])
        (let ([defs (map (λ (x) (cons (second x) (third x))) rdefs)])
          (let-values ([(marked rest) (partition (λ (x) (or (eq? (car x) 'void) (GCfind? (car x) rnotdefs))) defs)])
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
                            (if (or (equal? lastv '(void)) (define? lastv))
                                (append xs (list '(void)))
                                xs)))
                        (loop (append new marked) newrest)))))))))))
;(set! BEGINgc (λ (x) x))
(define (BEGINappend cs)
  (if (null? cs)
      '()
      (let ([c (car cs)])
        (if (begin? c)
            (append (cdr c) (BEGINappend (cdr cs)))
            (cons c (BEGINappend (cdr cs)))))))
(define (BEGIN conf macros xs)
  (BEGINgc
   (BEGINappend
    (map
     (λ (x)
       (if (define? x)
           (DEFINE conf macros (cadr x) (cddr x))
           (EVAL conf macros x)))
     (BEGINappend
      (map (λ (x) (macroexpand macros x)) xs)))))) ;在GC前合并begin
(define (QUOTE x)
  (cond
    [(pair? x) (list 'cons (QUOTE (car x)) (QUOTE (cdr x)))]
    [(symbol? x) `(quote ,x)]
    [(null? x) '(quote ())]
    [else x]))

(define (run conf xs)
  (EVAL conf (make-hash) (cons 'begin (append (runprelude conf) xs))))

(run (newconf) '((define-record-type <pare>
                   (kons x y)
                   pare?
                   (x kar set-kar!)
                   (y kdr))
                 (pare? (kons 0 1))))
