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
(require "zscm.rkt")
(define-syntax %newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons (cons (quote r) (list 'sfunc (quote s))) (%newns x ...))]
    [(_ r x ...) (cons (cons (quote r) (list 'sfunc (quote r))) (%newns x ...))]))
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
            string?
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
            [displayln println]
            [atom! atom]
            [atom-get deref]
            [atom-set! reset!]
            [atom-map! atom-map!]
            raise
            with-exception-handler
            [hash? hash-map?]
            [hash hash-map]
            [hash-set assoc]
            [hash-ref hash-ref]
            [hash-has-key? contains?]
            make-immutable-hash
            hash->list
            [str->strlist str->strlist]
            ))
(define (id x) (newid x))
(define (newid x)
  (hash-ref! ns x (λ () (string->symbol (string-append "zs-" (symbol->string x))))))

(define (EVAL x)
  (cond
    [(eq? x 'host-language) "mal"]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(eq? x 'void) '(fn* () nil)]
    [(symbol? x) (id x)]
    [(eq? x #t) 'true]
    [(eq? x #f) 'false]
    [else x]))
(define (APPLY f xs)
  (match f
    ['lambda (LAMBDA (first xs) (second xs))]
    ['begin (BEGIN xs)]
    ['void 'nil]
    ['quote (QUOTE (car xs))]
    ['ffi (if (null? (cdr xs)) (car xs) (error "APPLY: ffi" f xs))]
    ['if `(let* (v ,(EVAL (first xs)) b (if (nil? v) false v))
                    (if b ,(EVAL (second xs)) ,(EVAL (third xs))))]
    [_ (cons (UNFUNC (EVAL f)) (map EVAL xs))]))
(define (UNFUNC x)
  (match x
    [`(sfunc ,v) v]
    [_ `(unfunc ,x)]))
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
      [(null? args) `(sfunc (fn* ,a ,(EVAL x)))]
      [(symbol? args) (loop (append a (list '& (newid args))) '())]
      [else (loop (append a (list (newid (car args)))) (cdr args))])))
(compiler c [number equal vector display atom ffi hash nochar] feval)

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
           (try* ((unfunc thunk))
                 (catch* e (if (if (list? e)
                               (if (not (empty? e))
                                   (= (first e) 'raise)
                                   false)
                               false)
                               ((unfunc handler) (first (rest e)))
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
    (def! sfunc
      (fn* (x)
           (vector '_lambda_ x)))
    (def! jpair?
      (fn* (x)
           (if (vector? x)
               (if (> (count x) 0)
                   (= (nth x 0) '_pair_)
                   false)
               false)))
    (def! procedure?
      (fn* (x)
           (if (vector? x)
               (if (> (count x) 0)
                   (= (nth x 0) '_lambda_)
                   false)
               false)))
    (def! unfunc
      (fn* (x)
           (if (procedure? x)
               (nth x 1)
               (error "apply: isn't procedure?"))))
    (def! vvector?
      (fn* (x)
           (if (vector? x)
               (not (or (jpair? x) (procedure? x)))
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
               (apply (unfunc f) xs)
               (error "apply: isn't list?" f xs))))
    (def! number?
      (fn* (x)
           (not (or (nil? x) (true? x) (false? x) (string? x) (symbol? x) (keyword? x) (list? x) (vector? x) (map? x) (atom? x)))))
    (def! hash-ref
      (fn* (h k & f)
           (if (contains? h k)
               (get h k)
               (if (null? f)
                   (error "hash-ref" h k)
                   (let* (x (car f))
                     (if (procedure? x)
                         ((unfunc x))
                         x))))))
    (def! make-immutable-hash
      (fn* (xs)
           (papply hash-map xs)))
    (def! hash->list
      (fn* (hash)
           (map
            (λ (k)
              (cons k (get hash k)))
            (keys hash))))
    (def! %str->strlist
      (fn* (s)
           (if (string? s)
               (let* (r (seq s))
                 (if (nil? r)
                     ()
                     r))
               (error "string->list: isn't string?" s))))
    (def! atom-map!
      (fn* (a f)
           (swap! s (unfunc f))))
    ))

(define (main)
  (let* ([exp (read)] [out (c exp)])
    (for ([x out])
      (display x))
    (newline)))

(main)
