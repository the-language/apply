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

;; if
;; - %if
;; + if

;; equal
;; + equal?

;; list
;; + list?
;; + list
;; + map

(define (init feature)
  (set-null-prog!
   '((define-syntax define-syntax-rule
       (syntax-rules ()
         ((_ (f . args) x) (define-syntax f
                             (syntax-rules ()
                               ((_ . args) x))))))
     (define-syntax-rule (λ . xs) (lambda . xs))
     (let-syntax ((def define))
       (define-syntax define
         (syntax-rules ()
           ((_ var init) (def var init))
           ((_ (var . args) . body) (define var (λ args . body))))))
     )
   `(,@(if (set-member? feature 'if)
           '()
           '((define-syntax if
               (syntax-rules ()
                 [(_ c x) (if c x (void))]
                 [(_ c x y) (%if c x y)]))))
     ,@(if (set-member? feature 'vector)
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
     ,@(if (set-member? feature 'number)
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
     ,@(if (set-member? feature 'list)
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
     ,@(if (set-member? feature 'equal)
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
   `((define-syntax %define-record-type
       (syntax-rules ()
         [(_ pred x) (void)]
         [(_ pred c (f a) fs ...)
          (begin
            (define (a x) (if (pred x) (%vector-ref x c) (error "type error" (quote f) x)))
            (%define-record-type pred (+ 1 c) fs ...))]))
     (define-syntax define-record-type
       (syntax-rules ()
         [(_ name (constructor cf ...) pred (f a) ...)
          (begin
            (define (constructor cf ...) (vector (cons '_struct:_ 'name) f ...))
            (define (pred x) (and (%vector? x) (equal? (%vector-ref x 0) (cons '_struct:_ 'name))))
            (%define-record-type pred 1 (f a) ...))]))
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

     (define-record-type delay-v
       (%delay-v f)
       promise?
       (lazy %lazydelay-vv))
     (define-syntax-rule (delay x) (%delay-v (lambda () x)))
     (define (force x) ((%lazydelay-vv x))) ;Fix: memorize
     (define-syntax-rule (delay-force x) (delay (force x)))
     (define (make-promise x) (if (promise? x) x (delay x)))
     )))
(define (c? x)
  (not
   (or
    (pair? x)
    (symbol? x))))
(define-syntax-rule (mkfs x ...) (make-hash (list (cons (quote x) x) ...)))
(define fs
  (mkfs
   + - * / < > <= >= =))
(require racket/sandbox)
(define evalp (make-evaluator	'racket))
(define (EVAL x)
  (cond
    [(pair? x) (APPLY (car x) (cdr x))]
    [else x]))
(define (APPLY f xs)
  (cond
    [(eq? f 'lambda) `(lambda ,(car xs) ,(BEGIN (cdr xs)))]
    [(eq? f 'begin) (BEGIN xs)]
    [(eq? f 'define) (error "APPLY: define" f xs)]
    [(eq? f 'quote) (if (null? (cdr xs)) (list 'quote (car xs)) (error "APPLY: quote" f xs))]
    [else
     (let ([nxs (map EVAL xs)])
       (if (and (hash-has-key? fs f) (andmap c? nxs))
           (apply (hash-ref fs f) nxs)
           (cons (EVAL f) (map EVAL xs))))]))
(define (BEGIN xs)
  (if (null? (cdr xs))
      (EVAL (car xs))
      (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)]
            [b (let ([x (take-right xs 2)])
                 (and (equal? (second x) '(void))
                      (not (and (pair? (first x)) (eq? (car (first x)) 'define)))))])
        (append
         (cons
          'begin
          (map
           (λ (x)
             (if (and (pair? x) (eq? (car x) 'define))
                 (if (null? (cdddr x))
                     `(define ,(cadr x) ,(EVAL (caddr x)))
                     (error "BEGIN: define" xs))
                 (EVAL x)))
           xs))
         (if b
             '((void))
             '())))))
(define (run fe x)
  (init fe)
  (EVAL (cons 'begin (expand-program (runmacro (append pre x))))))
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
  '((defmacro struct
      (λ (name fs)
        `(define-record-type ,name
           (,name ,@fs)
           ,(string->symbol (string-append (symbol->string name) "?"))
           ,@(map
              (λ (f)
                (list f (string->symbol (string-append (symbol->string name) "-" (symbol->string f)))))
              fs))))
    ))
