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
(define (init fe)
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
   `(,@(if (set-member? fe 'vector)
           '((define pair? %pair?)
             (define car %car)
             (define cdr %cdr))
           '((define-syntax-rule (vec) '_vector_)
             (define (vector . xs) (cons (vec) xs))
             (define (%vector? x) (and (%pair? x) (eq? (%car x) (vec))))
             (define (list->vector xs) (cons (vec) xs))
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
                 [else (%vector-ref v k (%cdr xs) (- x 1))]))
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
     (define (not x) (if x #f #t))
     (define (zero? x) (eq? x 0))
     (define (append xs ys)
       (if (null? xs)
           ys
           (cons (car xs) (append (cdr xs) ys))))
     (define (list . xs)
       xs)
     (define (map f xs)
       (if (null? xs)
           '()
           (cons (f (car xs)) (map f (cdr xs)))))
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
     (define (equal? x y)
       (if (pair? x)
           (and (pair? y)
                (equal? (car x) (car y))
                (equal? (cdr x) (cdr y)))
           (eq? x y)))
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
     ,(if (set-member? fe 'vector)
          '(define (vector? x)
             (and (%vector? x)
                  (or (zero? (%vector-length x))
                      (not (let ([x (%vector-ref x 0)])
                             (and (pair? x)
                                  (eq? (car x) '_struct:_)))))))
          '(define (vector? x)
             (and (%vector? x)
                  (or (%vector-length-0? x)
                      (not (let ([x (%vector-ref x 0)])
                             (and (pair? x)
                                  (eq? (car x) '_struct:_))))))))
     (define (vector-length x)
       (if (vector? x)
           (%vector-length x)
           (error "vector-length: isn't vector?" x)))
     (define (vector-ref v k)
       (if (vector? v)
           (%vector-ref v k)
           (error "vector-ref: isn't vector?" x)))
     )))
(define (c? x)
  (not
   (or
    (pair? x)
    (symbol? x))))
(define-syntax-rule (mkfs x ...) (make-hash (list (cons (quote x) x) ...)))
(define fs
  (mkfs
   + - * /))
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
    [(eq? f 'quote) (if (null? (cdr xs)) (car xs) (error "APPLY: quote" f xs))]
    [(eq? f '_EVAL_) (if (null? (cdr xs)) (evalp (car xs)) (error "APPLY: _EVAL_" f xs))]
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
  (EVAL (cons 'begin (expand-program x))))
(define-syntax-rule (compiler name [fe ...] evalf)
  (define (name p) (evalf (run (set (quote fe) ...) p))))
