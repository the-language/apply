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
(provide c)
(require "corescm.rkt")
(define-syntax %newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons (cons (quote r) (quote s)) (%newns x ...))]
    [(_ r x ...) (cons (cons (quote r) (quote r)) (%newns x ...))]))
(define-syntax-rule (newns x ...)
  (make-hasheq
   (%newns x ...)))

(define ns (newns
            [cons pcons]
            [%car car]
            [%cdr cdr]
            [%pair? pair?]
            [null? empty?]
            +
            -
            *
            /
            <
            >
            <=
            >=
            =
            number?
            char?
            string?
            string->list
            if
            quote
            symbol?
            [eq? =]
            [equal? =]
            error
            boolean?
            procedure?
            apply
            [%vector->list vector->list]
            [list->vector list->vector]
            vector
            [%vector? vvector?]
            [%vector-length count]
            [%vector-ref nth]))
;(define (id x) (hash-ref ns x))
(define (id x) (newid x))
(define (newid x)
  (hash-ref! ns x (λ () (gensym x))))

(define (EVAL x)
  (cond
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x) (id x)]
    [(eq? x #t) 'true]
    [(eq? x #f) 'false]
    [else x]))
(define (APPLY f xs)
  (cond
    [(eq? f 'lambda) (if (null? (cddr xs))
                         (LAMBDA (car xs) (cadr xs))
                         (error "APPLY: lambda" f xs))]
    [(eq? f 'begin) (BEGIN xs)]
    [(eq? f 'define) (error "APPLY: define" f xs)]
    [(eq? f 'void) 'nil]
    [(eq? f 'quote) (if (null? (cdr xs)) (QUOTE (car xs)) (error "APPLY: quote" f xs))]
    [else (cons (EVAL f) (map EVAL xs))]))
(define (QUOTE x) (list 'quote x))
(define (BEGIN xs)
  (cond
    [(null? xs) (EVAL '(void))]
    [(null? (cdr xs)) (EVAL (car xs))]
    [else
     (cons 'do
           (map (λ (x)
                  (if (and (pair? x) (eq? (car x) 'define))
                      (if (null? (cdddr x))
                          `(def! ,(newid (cadr x)) ,(EVAL (caddr x)))
                          (error "BEGIN: define" xs))
                      (EVAL x))) xs))]))
(define (LAMBDA args x)
  (let loop ([a '()] [args args])
    (cond
      [(null? args) `(fn* ,a ,(EVAL x))]
      [(symbol? args) (loop (append a (list '& (newid args))) '())]
      [else (loop (append a (list (newid (car args)))) (cdr args))])))
(compiler c [number equal if vector] EVAL)

(c '((define-record-type <pare>
       (kons x y)
       pare?
       (x kar)
       (y kdr))))

(define pre
  '((def! list->vector
      (fn* (xs)
           (apply vector xs)))
    (def! vector->list
      (fn* (xs)
           (apply list xs)))
    (def! pcons
      (fn* (x xs)
           (if (or (list? xs) (vector? xs))
               (cons x xs)
               (vector '_pair_ x xs))))
    (def! jpair?
      (fn* (x)
           (if (vector? x)
               (if (> (count x) 0)
                   (= (nth x 0) '_pair_)
                   false)
               false)))
    (def! vvector?
      (fn* (v)
           (if (vector? x)
               (not (jpair? x))
               false)))
    (def! pair?
      (fn* (x)
           (or (vector? x) (list? x))))
    (def! car
      (fn* (x)
           (if (jpair? x)
               (nth x 1)
               (first x))))
    (def! cdr
      (fn* (x)
           (if (jpair? x)
               (nth x 2)
               (rest x))))
      ))
    