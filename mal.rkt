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
            string?
            quote
            symbol?
            [eq? =]
            [equal? =]
            error
            boolean?
            [procedure? fn?]
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
            [atom-map! swap!]
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
    [(symbol? x) (id x)]
    [(eq? x #t) 'true]
    [(eq? x #f) 'false]
    [else x]))
(define (APPLY f xs)
  (match f
    ['lambda (LAMBDA (first xs) (second xs))]
    ['begin (BEGIN xs)]
    ['quote (QUOTE (car xs))]
    ['ffi (if (null? (cdr xs)) (car xs) (error "APPLY: ffi" f xs))]
    ['if `(if ,(EVAL (first xs)) ,(EVAL (second xs)) ,(EVAL (third xs)))]
    [_ (cons (EVAL f) (map EVAL xs))]))
(define (QUOTE x) (list 'quote x))
(define (BEGIN xs)
  (cons 'do
        (map (λ (x)
               (if (and (pair? x) (eq? (car x) 'define))
                   `(def! ,(newid (cadr x)) ,(EVAL (caddr x)))
                   (EVAL x))) xs)))
(define (LAMBDA args x)
  (let loop ([a '()] [args args])
    (cond
      [(null? args) `(fn* ,a ,(EVAL x))]
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
  '((def! all?
      (fn* [pred lst]
           (if (empty? lst)
               true
               (if (pred (first lst))
                   (all? pred (rest lst))
                   false))))
    (def! *digits-set* {"0" true "1" true "2" true "3" true "4" true
                            "5" true "6" true "7" true "8" true "9" true "." true "/" true "e" true "+" true})
    (def! all-digits?
      (fn* [chars]
           (all? (fn* [c] (contains? *digits-set* c)) chars)))
    (def! slow-number?
      (fn* [x]
           (let* [chars (seq (str o))]
             (if (= "-" (first chars))
                 (all-digits? (rest chars))
                 (all-digits? chars)))))
    (def! number/fn?
      (fn* [x]
           (not (or (nil? x) (true? x) (false? x) (string? x) (symbol? x) (keyword? x) (list? x) (vector? x) (map? x) (atom? x)))))
    (def! number?
      (fn* [x]
           (if (number/fn? x)
               (slow-number? x)
               false)))
    (def! fn?
      (fn* [x]
           (if (number/fn? x)
               (not (slow-number? x))
               false)))
    (def! error
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
    (def! hash-ref
      (fn* (h k & f)
           (if (contains? h k)
               (get h k)
               (if (null? f)
                   (error "hash-ref" h k)
                   (let* (x (car f))
                     (if (fn? x)
                         (x)
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
    ))

(writeln (cons 'do (c (read))))
