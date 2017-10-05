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
            [apply papply]
            [%vector->list vector->list]
            [list->vector list->vector]
            vector
            [%vector? vvector?]
            [%vector-length count]
            [%vector-ref nth]
            list
            list?
            map
            [displayln println]
            [atom! atom]
            [atom-get deref]
            [atom-set! reset!]
            [atom-map! swap!]
            raise
            with-exception-handler
            ))
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
    [(eq? f 'ffi) (if (null? (cdr xs)) (car xs) (error "APPLY: ffi" f xs))]
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
(compiler c [number equal if vector list display atom ffi] feval)

(define (unbegin x)
  (if (eq? (car x) 'do)
      (cdr x)
      (error "unbegin")))

(define (feval xs)
  (append pre (unbegin (EVAL xs))))

(define pre
  '((def! error
      (fn* (& xs)
           (throw (cons 'error xs))))
    (def! raise
      (fn* (x)
           (throw (list 'raise x))))
    (def! with-exception-handler
      (fn* (handler thunk)
           (try* (thunk)
                 (catch* e (if (if (list? e)
                               (if (not (empty? e))
                                   (= (first e) 'raise)
                                   false)
                               false)
                               (handler (first (rest e)))
                               (throw e))))))
    (def! list->vector
      (fn* (xs)
           (if (list? xs)
               (apply vector xs)
               (error "list->vector: isn't list?" xs))))
    (def! vector->list
      (fn* (xs)
           (if (vvector? xs)
               (apply list xs)
               (error "vector->list: isn't vector?" xs))))
    (def! pcons
      (fn* (x xs)
           (if (list? xs)
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
      (fn* (x)
           (if (vector? x)
               (not (jpair? x))
               false)))
    (def! pair?
      (fn* (x)
           (or (jpair? x) (list? x))))
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
    (def! papply
      (fn* (f xs)
           (if (list? xs)
               (apply f xs)
               (error "apply: isn't list?" f xs))))
    ))

(c '((define-record-type <pare>
       (kons x y)
       pare?
       (x kar)
       (y kdr))))
