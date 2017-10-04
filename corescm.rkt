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
   `((define (zero? x) (eq? x 0))
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
     ,@(if (set-member? fe 'vector)
           '()
           '((define-syntax-rule (vec) '_vector_)
             (define (vector . xs) (cons (vec) xs))
             (define (vector? x) (and (pair? x) (eq? (car x) (vec))))
             (define (list->vector xs) (cons (vec) xs))
             (define (vector->list x) (if (vector? x) (cdr x) (error "vector->list: isn't vector?" x)))
             (define (vector-ref v k)
               (cond
                 [(< k 0) (error "vector-ref: isn't exact-nonnegative-integer?" v k)]
                 [(vector? v) (%vector-ref v k (cdr v) k)]
                 [else (error "vector-ref: isn't vector?" v k)]))
             (define (%vector-ref v k xs x)
               (cond
                 [(null? xs) (error "vector-ref: index is out of range" v k)]
                 [(zero? x) (car xs)]
                 [(< k 0) (error "vector-ref: isn't exact-nonnegative-integer?" v k)]
                 [else (%vector-ref v k (cdr xs) (- x 1))]))))
     )
   '()))
(init (set))
(expand-program '())
