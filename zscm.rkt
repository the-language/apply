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
(require "conf.rkt")

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
   (define (memroizeeq f) f) ; zaoqil-core
   (define (memorize1eq f) f) ; zaoqil-core

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
           [(pair? x) (begin
                        (_putstr_ "(")
                        (display (car x))
                        (%dis%* (cdr x))
                        (_putstr_ ")"))]
           [(null? x) (_putstr_ "()")]
           [(string? x) (_putstr_ x)]
           [(symbol? x) (_putstr_ (symbol->string x))]
           [(number? x) (_putstr_ (number->string x))]
           [(vector? x) (begin
                          (_putstr_ "#")
                          (display (vector->list x)))]
           [(boolean? x) (if x (_putstr_ "#t") (_putstr_ "#f"))]
           [(struct? x) (display (struct->vector x))]
           [(char? x) (_putstr_ (string x))]
           [(atom? x) (begin
                        (_putstr_ "#<atom:")
                        (display (atom-get x))
                        (_putstr_ ">"))]
           [(procedure? x) (_putstr_ "#<procedure>")]
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
     '((define-record-type delay
         (%delay v)
         promise?
         (v %force))
       (defmacro delay
         (λ (x)
           `(%delay (atom! (lambda () ,x)))))
       (define (promise-forced? x) (pair? (atom-get x)))
       (define (force x)
         (car
          (atom-map!
           (λ (x) (if (pair? x)
                      x
                      (list (x))))
           (%force x))))
       )
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

(require "prelude/prelude.rkt")

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
