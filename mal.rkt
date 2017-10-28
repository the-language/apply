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
(provide mal)
(require "codegen.rkt")
(define id lisp-getid)
(define (EVAL x)
  (cond
    [(eq? x 'host-language) '(quote mal)]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x)
     (primcase
      x
      [null? 'empty?]
      [pair? 'jpair?]
      cons
      car
      cdr
      raise
      [with-exception-handler 'weh]
      procedure?
      apply
      [string-append 'str]
      string?
      symbol?
      [symbol->string 'str]
      [string->symbol 'symbol]
      [str->lst '(fn* (s)
                      (let* (r (seq s))
                        (if (nil? r)
                            ()
                            r)))]
      boolean?
      number?
      [number->string 'str]
      string->number ; 没有实现
      [equal? '=]
      [atom! 'atom]
      [atom-get 'deref]
      [atom-set! 'reset!]
      [atom-map! 'swap!]
      [hash? 'hash-map?]
      [hash-set 'assoc]
      [hash-ref 'get]
      [hash-has-key? 'contains?]
      [make-immutable-hash '(fn* (xs)
                                 (apply hash-map xs))]
      vector
      [vector? 'vvector?]
      [vector-length 'count]
      [vector-ref 'nth]
      [list->vector '(fn* (xs)
                          (apply vector xs))]
      [vector->list '(fn* (xs)
                          (apply list xs))]
      putstr
      newline
      +
      -
      *
      /
      <
      >
      <=
      >=
      [= 'eqs]
      (id x))]
    [(eq? x #t) 'true]
    [(eq? x #f) 'false]
    [else x]))
(define (APPLY f xs)
  (match f
    ['lambda (LAMBDA (first xs) (cdr xs))]
    ['begin (BEGIN xs)]
    ['quote (QUOTE (car xs))]
    ['ffi (FFI 'mal EVAL xs)]
    ['if `(if ,(EVAL (first xs)) ,(EVAL (second xs)) ,(EVAL (third xs)))]
    [_ (cons (EVAL f) (map EVAL xs))]))
(define (QUOTE x)
  (if (null? x)
      '()
      `(quote ,x)))
(define (BEGIN xs)
  (if (null? (cdr xs))
      (EVAL (car xs))
      (cons 'do
            (map (λ (x)
                   (if (and (pair? x) (eq? (car x) 'define))
                       `(def! ,(id (second x)) ,(EVAL (third x)))
                       (EVAL x))) xs))))
(define (LAMBDA args x)
  (let loop ([a '()] [args args])
    (cond
      [(null? args) `(fn* ,a ,(BEGIN x))]
      [(symbol? args) (loop (append a (list '& (id args))) '())]
      [else (loop (append a (list (id (car args)))) (cdr args))])))
(compiler mal [+-*/<>= equal vector display atom hash [charstr 'nochar]] feval)

(define (undo x)
  (if (and (pair? x) (eq? (car x) 'do))
      (cdr x)
      (list x)))

(define (feval xs)
  (cons 'do (append pre (undo (EVAL xs)))))

(define pre
  '((def! all?
      (fn* [pred lst]
           (if (empty? lst)
               true
               (if (pred (first lst))
                   (all? pred (rest lst))
                   false))))
    (def! *digits-set* (hash-map "0" true "1" true "2" true "3" true "4" true
                                 "5" true "6" true "7" true "8" true "9" true "." true "/" true "e" true "+" true))
    (def! all-digits?
      (fn* [chars]
           (all? (fn* [c] (contains? *digits-set* c)) chars)))
    (def! slow-number?
      (fn* [x]
           (let* [chars (seq (str x))]
             (if (= "-" (first chars))
                 (all-digits? (rest chars))
                 (all-digits? chars)))))
    (def! boolean?
      (fn* [x]
           (or (true? x) (false? x))))
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
    
    (def! raise
      (fn* (x)
           (throw (list 'raise x))))
    (def! weh
      (fn* (handler thunk)
           (try* (thunk)
                 (catch* e (if (if (list? e)
                                   (if (not (empty? e))
                                       (= (first e) 'raise)
                                       false)
                                   false)
                               (handler (first (rest e)))
                               (throw e))))))
    (def! cons
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
    (def! hash->list
      (fn* (hash)
           (map
            (fn* (k)
                 (cons k (get hash k)))
            (keys hash))))
    (def! %dis% (atom ""))
    (def! newline (fn* () (do (println (deref %dis%)) (reset! %dis% ""))))
    (def! putstr
      (fn* (x)
           (swap! %dis% (fn* (s) (str s x)))))
    (def! eqs
      (fn* (x y & xs)
           (if (= x y)
               (if (empty? xs)
                   true
                   (apply eqs (cons y xs)))
               false)))
    ))
