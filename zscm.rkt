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
(require "common.rkt")
(require "pass/pass.rkt")
(require "gc.rkt")

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
     '((define _putstr_ __putstr)
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
       (define newline __newline))
     '((define (display x) (error "display: can't display" x))
       (define (newline) (error "newline: can't newline")))))

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
   ['nochar
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
(define (prim? x)
  (and (symbol? x)
       (match (string->list (symbol->string x))
         [`(#\_ #\_ . ,f) (string->symbol (list->string f))]
         [_ #f])))
(struct just (x))
(struct RTV ())
(define rtv (RTV))
(define rtv? RTV?)
(require racket/sandbox)
(define evalp (make-evaluator 'racket))
(define ((undefined env x)) (raise (list x 'at (just-x (hash-ref env '|#AT|)))))
(define (env-ref env x) (hash-ref env x (undefined env x)))

(define (EVAL env x)
  (match x
    [`(,(or 'λ 'lambda) ,a ,@b) (LAMBDA env a b)]
    [`(begin ,@b) (mk-begin (BEGIN env b))]
    [`(if ,b ,x ,y) `(if ,(EVAL env b) ,(EVAL env x) ,(EVAL env y))]
    [(? define?) (error "bad syntax")]
    [(? defmacro?) (error "bad syntax")]
    [`(quote ,x) `(quote ,x)]
    ['() '(quote ())]
    [(cons f xs)
     (if (and (symbol? f) (not (prim? f)))
         (let ([x (env-ref env f)])
           (cond
             [(pair? x) (EVAL env (apply (car x) xs))]
             [(procedure? x) (EVAL env (apply x xs))]
             [else (map (λ (v) (EVAL env v)) (cons f xs))]))
         (map (λ (v) (EVAL env v)) (cons f xs)))]
    [(and (? symbol?) (not (? prim?)))
     (let ([k (env-ref env x)])
       (cond
         [(pair? k) (EVAL env (cdr k))]
         [(procedure? k) (error "bad syntax")]
         [(just? k) (EVAL env (just-x k))]
         [(rtv? k) x]))]
    [_ x]))
(define (BEGIN0 env xs)
  (if (null? xs)
      '()
      (let ([x (car xs)] [xs (cdr xs)])
        (match x
          [`(define ,(cons f args) ,@b) (BEGIN0 env (cons `(define ,f (lambda ,args ,@b)) xs))]
          [`(begin ,@b) (BEGIN0 env (append b xs))]
          [`(defmacro ,f ,x)
           (let-values ([(nf nx) (if (pair? f)
                                     (values (car f) `(λ ,(cdr f) ,x))
                                     (values f x))])
             (cons `(defmacro ,nf ,nx) (BEGIN0 (hash-set env nf (evalp nx)) xs)))] ; BUG evalp
          [`(void) (BEGIN0 env xs)]
          [(or `(,(or 'λ 'lambda) ,_ ,@_) `(define ,_ ,_) `(if ,_ ,_ ,_) `(quote ,_))
           (cons x (BEGIN0 env xs))]
          [(cons (and (? symbol?) f) args)
           (let ([y (hash-ref env f rtv)])
             (cond
               [(pair? y) (BEGIN0 env (cons (apply (car y) args) xs))]
               [(procedure? y) (BEGIN0 env (cons (apply y args) xs))]
               [(just? y) (BEGIN0 env (cons (cons (just-x y) args) xs))]
               [(rtv? y) (cons x (BEGIN0 env xs))]))]
          [_ (cons x (BEGIN0 env xs))]))))
(define (envv? x)
  (or (procedure? x)
      (just? x)
      (rtv? x)
      (and (pair? x)
           (procedure? (car x))
           (envv? (cdr x)))))
(require racket/hash)
(define/contract (hash+ h1 h2)
  (-> (hash/c symbol? envv?) (hash/c symbol? envv?) (hash/c symbol? envv?))
  (hash-union h1 h2 #:combine (λ (o n) n)))
(define (BEGIN env xs)
  (let ([xs (BEGIN0 env xs)] [las (last xs)])
    (let-values ([(defs) (filter define? xs)]
                 [(las-void) (or (equal? las '(void)) (defmacro? las) (define? las))]
                 [(macs xs) (partition defmacro? xs)])
      (let* ([envp
              (hash-union
               (make-immutable-hash
                (map
                 (match-lambda [`(defmacro ,(and (? symbol?) f) ,x) (cons f (evalp x))]) macs))
               (make-immutable-hash
                (map (match-lambda [`(define ,(and (? symbol?) f) ,x)
                                    (if (or (symbol? x) (char? x) (number? x))
                                        (cons f (just x))
                                        (cons f rtv))]) defs))
               #:combine cons)]
             [newenv (hash+ env envp)]
             [r (map
                 (match-lambda
                   [`(define ,f ,x) `(define ,f ,(EVAL (hash-set newenv '|#AT| (just f)) x))]
                   [x (EVAL newenv x)])
                 xs)])
        (if las-void
            (append r '((void)))
            r)))))
(define (LAMBDA env args body)
  (match args
    [(list-rest x ... r)
     `(lambda ,args
        ,(mk-begin
          (BEGIN
           (hash+ env
                  (make-immutable-hash
                   (map
                    (λ (id) (cons id rtv))
                    (if (null? r)
                        x
                        (cons r x)))))
           body)))]))

(define (run conf xs)
  (EVALgc
   (runpass conf (EVAL (hash) (cons 'begin (append (runprelude conf) xs))))))

(define-syntax-rule (compiler name [c ...] evalf)
  (define (name p) (evalf (run (newconf c ...) p))))
