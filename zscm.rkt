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
(require "alexpander.rkt")
;; feature:

;; vector
;; + %vector->list
;; + list->vector
;; + vector
;; + %vector?
;; + %vector-length
;; + %vector-ref

;; number
;; + +
;; + -
;; + *
;; + /
;; + <
;; + >
;; + <=
;; + >=
;; - %+
;; - %-
;; - %*
;; - %/
;; - %<
;; - %>
;; - %=

;; equal
;; + equal?

;; list
;; + list?
;; + list
;; + map

;; display
;; + displayln : String -> Void

;; atom
;; + atom?
;; + atom! : Any -> Atom
;; + atom-map! : Atom -> (Any -> Any) -> Any
;; + atom-set! : Atom -> Any -> Void
;; + atom-get

;; ffi
;; + ffi

;; hash
;; + hash?
;; + hash
;; + hash-set
;; + hash-ref
;; + hash->list
;; + hash-has-key?
;; + make-immutable-hash

;; nochar
;; - char?
;; - string->list
;; + %str->strlist

;; void
;; + (void)
;; + void?

(define (init features)
  (define (has-feature? x) (set-member? features x))
  (set-null-prog!
   '((define-syntax define-syntax-rule
       (syntax-rules ()
         ((_ (f . args) x) (define-syntax f
                             (syntax-rules ()
                               ((_ . args) x))))))
     (define-syntax-rule (λ . xs) (lambda . xs))
     (let-syntax ([def define])
       (define-syntax define
         (syntax-rules ()
           ((_ var init) (def var init))
           ((_ (var . args) . body) (define var (λ args . body))))))
     )
   `((let-syntax ([if3 if])
       (define-syntax if
         (syntax-rules ()
           [(_ c x) (when c x)]
           [(_ c x y) (if3 c x y)])))
     (define-syntax-rule (when c x) (if c x (void)))
     ,@(if (has-feature? 'vector)
           '((define pair? %pair?)
             (define car %car)
             (define cdr %cdr)
             (define (%vector-length-0? x) (zero? (%vector-length x)))
             (define (vector->list x) (if (vector? x) (%vector->list x) (error "vector->list: isn't vector?" x)))
             )
           '((define-syntax-rule (vec) '_vector_)
             (define (vector . xs) (cons (vec) xs))
             (define (%vector? x) (and (%pair? x) (eq? (%car x) (vec))))
             (define (list->vector xs)
               (if (list? xs)
                   (cons (vec) xs)
                   (error "list->vector: isn't list?" xs)))
             (define (vector->list x) (if (vector? x) (%cdr x) (error "vector->list: isn't vector?" x)))
             (define (%vector-ref v k)
               (cond
                 [(< k 0) (error "vector-ref: isn't exact-nonnegative-integer?" v k)]
                 [(%vector? v) (%%vector-ref v k (%cdr v) k)]
                 [else (error "vector-ref: isn't vector?" v k)]))
             (define (%%vector-ref v k xs x)
               (cond
                 [(null? xs) (error "vector-ref: index is out of range" v k)]
                 [(zero? x) (%car xs)]
                 [(< k 0) (error "vector-ref: isn't exact-nonnegative-integer?" v k)]
                 [else (%%vector-ref v k (%cdr xs) (- x 1))]))
             (define (pair? x) (and (%pair? x) (not (eq? (%car x) (vec)))))
             (define (%vector-length x)
               (if (%vector? x)
                   (length (%cdr x))
                   (error "vector-length: isn't vector?" x)))
             (define (%vector-length-0? x)
               (if (%vector? x)
                   (null? (%cdr x))
                   (error "vector-length: isn't vector?" x)))
             (define (car p)
               (if (pair? p)
                   (%car p)
                   (error "car: isn't pair?" p)))
             (define (cdr p)
               (if (pair? p)
                   (%cdr p)
                   (error "cdr: isn't pair?" p)))
             ))
     ,@(if (has-feature? 'number)
           '()
           '((define (+ . xs) (foldl %+ 0 xs))
             (define (* . xs) (foldl %* 1 xs))
             (define (- x . xs)
               (if (null? xs)
                   (%- 0 x)
                   (foldl (λ (n x) (%- x n)) x xs)))
             (define (/ x . xs)
               (if (null? xs)
                   (%/ 1 x)
                   (foldl (λ (n x) (%/ x n)) x xs)))
             (define (%<= x y) (or (%< x y) (%= x y)))
             (define (%>= x y) (or (%> x y) (%= x y)))
             (define-syntax-rule (%def<>= n f)
               (begin
                 (define (comt x y xs)
                   (if (null? xs)
                       (f x y)
                       (and (f x y) (f y (car xs) (cdr xs)))))
                 (define (f x y . xs) (comt x y xs))))
             (%def<>= < %<)
             (%def<>= > %>)
             (%def<>= >= %>=)
             (%def<>= <= %<=)
             (%def<>= = %=)
             ))
     (define (not x) (if x #f #t))
     (define (zero? x) (eq? x 0))
     (define (positive? x) (> x 0))
     (define (negative? x) (< x 0))
     (define (%max x y) (if (> x y) x y))
     (define (max x . xs) (foldl %max x xs))
     (define (%min x y) (if (< x y) x y))
     (define (min x . xs) (foldl %min x xs))
     (define (append xs ys)
       (if (null? xs)
           ys
           (cons (car xs) (append (cdr xs) ys))))
     ,@(if (has-feature? 'list)
           '()
           '((define (list . xs)
               xs)
             (define (list? xs)
               (or (null? xs) (and (pair? xs) (list? (cdr xs)))))
             (define (map f xs)
               (if (null? xs)
                   '()
                   (cons (f (car xs)) (map f (cdr xs)))))))
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
     ,@(if (has-feature? 'equal)
           '()
           '((define (equal? x y)
               (if (pair? x)
                   (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y)))
                   (eq? x y)))))
     (define eqv? equal?)
     (define (list-ref xs k)
       (cond
         [(< k 0) (error "list-ref: isn't exact-nonnegative-integer?" xs k)]
         [(null? xs) (error "list-ref: index is out of range" xs k)]
         [(zero? k) (car xs)]
         [else (list-ref (cdr xs) k)]))
     )
   `((define-syntax-rule (define-record-type
                           name
                           (constructor cf ...)
                           pred
                           f ...)
       (begin
         (define (constructor cf ...) (vector (cons '_struct:_ 'name) (%%define-record-type%c%% f) ...))
         (define (pred x) (and (%vector? x) (equal? (%vector-ref x 0) (cons '_struct:_ 'name))))
         (%define-record-type name pred 1 f ...)))
     (define-syntax %%define-record-type%c%%
       (syntax-rules ()
         [(_ (f a)) f]
         [(_ (f a setv)) (atom! f)]))
     (define-syntax %define-record-type
       (syntax-rules ()
         [(_ name pred x) (void)]
         [(_ name pred c (f a) fs ...)
          (begin
            (define (a x) (if (pred x) (%vector-ref x c) (error (symbol->string (quote a)) (quote pred) x)))
            (%define-record-type name pred (+ 1 c) fs ...))]
         [(_ name pred c (f a setv) fs ...)
          (begin
            (define (a x) (if (pred x) (atom-get (%vector-ref x c)) (error (symbol->string (quote a)) (quote pred) x)))
            (define (setv x v) (if (pred x) (atom-set! (%vector-ref x c) v) (error (symbol->string (quote setv)) (quote pred) x)))
            (%define-record-type name pred (+ 1 c) fs ...))]))
     (define (struct? x)
       (and (%vector? x)
            (not (%vector-length-0? x))
            (let ([x (%vector-ref x 0)])
              (and (pair? x)
                   (eq? (car x) '_struct:_)))))
     (define (vector? x)
       (and (%vector? x)
            (not (struct? x))))
     (define (vector-length x)
       (if (vector? x)
           (%vector-length x)
           (error "vector-length: isn't vector?" x)))
     (define (vector-ref v k)
       (if (vector? v)
           (%vector-ref v k)
           (error "vector-ref: isn't vector?" x)))

     ,@(if (has-feature? 'atom)
           '((define-record-type delay-v
               (%delay-v lazy)
               promise?
               (lazy %lazydelay-vv %set%delay!))
             (define-syntax-rule (delay x) (%delay-v (lambda () x)))
             (define (force x)
               (let ([v (%lazydelay-vv x)])
                 (if (procedure? v)
                     (let ([f (v)])
                       (begin
                         (%set%delay! x (list f))
                         f))
                     (car v)))))
           '((define-record-type delay-v
               (%delay-v lazy)
               promise?
               (lazy %lazydelay-vv))
             (define-syntax-rule (delay x) (%delay-v (lambda () x)))
             (define (force x) ((%lazydelay-vv x)))))
     (define-syntax-rule (delay-force x) (delay (force x)))
     (define (make-promise x) (if (promise? x) x (delay x)))

     ,@(if (has-feature? 'display)
           '()
           '((define (displayln x) (error "displayln: can't displayln" x))))
     (define (newline) (displayln ""))

     ,@(if (has-feature? 'atom)
           '()
           '((define (atom? x) #f)
             (define (atom! x) (error "atom!: doesn't support atom" x))
             (define (atom-map! x f) (error "atom-map!: doesn't support atom" x f))
             (define (atom-set! a x) (error "atom-set!: doesn't support atom" a x))
             (define (atom-get a) (error "atom-get: doesn't support atom" a x))))

     ,@(if (has-feature? 'ffi)
           '()
           '((define-syntax-rule (ffi x) (error "ffi: doesn't support" (quote x)))))

     (define-syntax with-handlers
       (syntax-rules ()
         [(_ () body ...) (begin body ...)]
         [(_ ([pred-expr handler-expr] ph ...) body ...)
          (call-with-exception-handler
           (λ (e)
             (if (pred-expr e)
                 (handler-expr e)
                 (with-handlers (ph ...) (raise e))))
           (λ ()
             body ...))]))

     (define *gennum-counter* (atom! 0))
     (define (gennum)
       (atom-map! *gennum-counter* (λ (x) (+ x 1))))

     (define-record-type %call/cc-v
       (%call/cc-v id v)
       %call/cc-v?
       (v %call/cc-v-v)
       (id %call/cc-id))
     (define (call/cc p)
       (let ([id (gennum)])
         (with-handlers ([(λ (x) (and (%call/cc-v? x) (eq? (%call/cc-id x) id))) %call/cc-v-v])
           (p (λ (x) (raise (%call/cc-v id x)))))))
     (define call-with-current-continuation call/cc)

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

     (define (hash-update hash key updater . f)
       (hash-set
        hash
        key
        (updater (if (null? f)
                     (hash-ref hash key)
                     (hash-ref hash key (car f))))))
     ,@(if (has-feature? 'hash)
           '()
           '((define-record-type hash
               (%make-immutable-hash xs)
               hash?
               (xs hash->list))
             (define (%make-immutable-hash-loop rs xs)
               (if (null? xs)
                   (%make-immutable-hash rs)
                   (let* ([x (car xs)] [xa (car xs)])
                     (if (ormap (λ (y) (equal? (car y) xa)) rs)
                         (%make-immutable-hash-loop rs (cdr xs))
                         (%make-immutable-hash-loop (cons x rs) (cdr xs))))))
             (define (make-immutable-hash xs)
               (%make-immutable-hash-loop '() (reverse xs)))
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
                         (f (cons (cons (car x) v) (cdr hash)))
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
             (define (hash-union h1 h2)
               (foldl (λ (x h) (hash-set h (car x) (cdr x))) h1 (hash->list h2)))
             (define (hash . xs) (make-immutable-hash xs))
             (define (hash-has-key? hash key)
               (if (hash-ref hash key #f)
                   #t
                   #f))))

     ,@(if (has-feature? 'nochar)
           '((define-record-type char
               (%char v)
               char?
               (v %g%char))
             (define (string->list s) (map %char (%str->strlist s)))
             )
           '())

     ,@(if (has-feature? 'void)
           '()
           '((define-record-type void
               (void)
               void?)))
     )))
(define (c? x)
  (cond
    [(symbol? x) #f]
    [(pair? x)
     (if (eq? 'quote (car x))
         #t
         #f)]
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
(require racket/sandbox)
(define evalp (make-evaluator	'racket))
(define (EVAL fe x)
  (cond
    [(pair? x) (APPLY (λ (x) (EVAL fe x)) (car x) (cdr x))]
    [(and (set-member? fe 'nochar) (char? x)) (list '%char (string x))]
    [else x]))
(define (APPLY evall f xs)
  (cond
    [(eq? f 'lambda) `(lambda ,(car xs) ,(BEGIN evall (cdr xs)))]
    [(eq? f 'begin) (BEGIN evall xs)]
    [(eq? f 'define) (error "APPLY: define" f xs)]
    [(eq? f 'quote) (if (null? (cdr xs)) (QUOTE (car xs)) (error "APPLY: quote" f xs))]
    [(eq? f 'letrec) (LETREC evall xs)]
    [else
     (let ([nxs (map evall xs)])
       (if (and (hash-has-key? fs f) (andmap c? nxs))
           (apply (hash-ref fs f) nxs)
           (cons (evall f) (map evall xs))))]))
(define (LETREC evall xs)
  (match xs
    [`(,xvs ,@body)
     (BEGIN
      evall
      (append
       (map (match-lambda
              [`[,x ,v] `(define ,x ,v)])
            xvs)
       body))]))
(define (QUOTE x)
  (cond
    [(pair? x) (list 'cons (QUOTE (car x)) (QUOTE (cdr x)))]
    [(symbol? x) (list 'quote x)]
    [(null? x) '(quote ())]
    [else x]))
(define (define? x) (and (pair? x) (eq? (car x) 'define)))
(define (lambda? x) (and (pair? x) (eq? (car x) 'lambda)))
(define (BEGIN evall xs)
  (if (and (null? (cdr xs)) (not (define? (car xs))))
      (evall (car xs))
      (cons
       'begin
       (BEGINgc
        (map
         (λ (x)
           (if (define? x)
               `(define ,(second x) ,(evall (third x)))
               (evall x)))
         xs)))))
; Symbol -> Exp -> Bool
(define (GCfind? s x)
  (match x
    [`(lambda ,_ ,v) (GCfind? s v)]
    [(? symbol? x) (equal? s x)]
    [`(define ,_ ,v) (GCfind? s v)]
    [`(quote ,_) #f]
    [(cons 'begin xs) (ormap (λ (x) (GCfind? s x)) xs)]
    [(? list? x) (ormap (λ (x) (GCfind? s x)) x)]
    [_ #f]))
(define (notpurefunctional? x)
  (cond
    [(and (pair? x) (eq? (car x) 'atom!)) (notpurefunctional? (second x))]
    [(lambda? x) #f]
    [(pair? x) #t]
    [else #f]))
(define (BEGINgc xs)
  (let ([lastv (last xs)])
    (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)])
      (let ([defs (map (λ (x) (cons (second x) (third x))) (filter define? xs))])
        (let-values ([(marked rest) (partition (λ (x) (or (eq? (car x) 'void) (notpurefunctional? (cdr x)) (GCfind? (car x) lastv))) defs)])
          (let loop ([marked marked] [rest rest])
            (if (null? rest)
                xs
                (let-values ([(new newrest) (partition (λ (x) (GCfind? (car x) marked)) rest)])
                  (if (null? new)
                      (let ([marked (map car marked)]
                            [end (λ (xs)
                                   (if (or (equal? lastv '(void)) (define? lastv))
                                       (append xs (list '(void)))
                                       xs))])
                        (end (filter
                              (λ (x)
                                (if (define? x)
                                    (set-member? marked (second x))
                                    #t)) xs)))
                      (loop (append new marked) newrest))))))))))
(define (run fe x)
  (init fe)
  (EVAL fe (cons 'begin (expand-program (runmacro (append pre x))))))
(define-syntax-rule (compiler name [fe ...] evalf)
  (define (name p) (evalf (run (set (quote fe) ...) p))))

(define (unbegin x)
  (if (and (pair? x) (eq? (car x) 'begin))
      (cdr x)
      (list x)))
(define (runmacro xs)
  (unbegin (EVALmacro (cons 'begin xs))))
(define ms (make-hash))
(define (EVALmacro x)
  (let ([x (macroexpand x)])
    (cond
      [(pair? x) (APPLYmacro (car x) (cdr x))]
      [else x])))
(define (macroexpand x)
  (cond
    [(and (pair? x) (eq? (car x) 'defmacro))
     (hash-set! ms (second x) (evalp (third x)))
     '(void)]
    [(and (pair? x) (hash-ref ms (car x) #f)) => (λ (mf) (macroexpand (apply mf (cdr x))))]
    [else x]))
(define (APPLYmacro f xs)
  (cond
    [(eq? f 'lambda) `(lambda ,(car xs) ,(BEGINmacro (cdr xs)))]
    [(eq? f 'begin) (BEGINmacro xs)]
    [(eq? f 'define) (error "APPLYmacro: define" f xs)]
    [(eq? f 'quote) (if (null? (cdr xs)) (list 'quote (car xs)) (error "APPLYmacro: quote" f xs))]
    [else (cons (EVALmacro f) (map EVALmacro xs))]))
(define (BEGINmacro xs)
  (if (null? (cdr xs))
      (EVALmacro (car xs))
      (cons
       'begin
       (map
        (λ (x)
          (if (and (pair? x) (eq? (car x) 'define))
              (if (null? (cdddr x))
                  `(define ,(cadr x) ,(EVALmacro (caddr x)))
                  (error "BEGINmacro: define" xs))
              (EVALmacro x)))
        xs))))
(define pre
  '((defmacro gensymmacro
      (λ xs
        (let ([s (if (null? xs)
                     "g"
                     (if (and (pair? s) (null? (cddr s)) (eq? (car s) 'quote))
                         (symbol->string (cadr s))
                         (if (string? s)
                             s
                             (error "gensym"))))])
          (gensym s))))
    (defmacro struct
      (λ (name fs . conf)
        `(define-record-type ,name
           (,name ,@fs)
           ,(string->symbol (string-append (symbol->string name) "?"))
           ,@(map
              (λ (f)
                (list f (string->symbol (string-append (symbol->string name) "-" (symbol->string f)))))
              fs))))
    (define make-immutable-hasheq make-immutable-hash)
    (define hasheq hash)
    ))
