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

(struct macrosym (id sym))

(define midc 0)
(define newmacrosym
  (let ([midc 0])
    (λ (s)
      (set! midc (+ 1 midc))
      (macrosym midc s))))

(define macrons (make-base-namespace))
(namespace-set-variable-value! 'sym newmacrosym #f macrons)

(define-syntax-rule (exp x ...)
  (stream "(" x ... ")"))

(define-syntax-rule (block x ...)
  (stream (exp "function()"
               x ...
               " end") "()"))

(define-syntax-rule (var x)
  (stream "local " (newvarid x) "=nil "))

(define-syntax-rule (set ms x v)
  (stream (id x) "=" (EVAL ms v) " "))

(define (APPLY ms f xs)
  (let ([f (macroexpand ms f)])
    (cond
      [(eq? f 'λ)
       (exp "function(" (mkss (car xs)) ")"
            "return " (BEGIN ms (cdr xs))
            " end")]
      [(eq? f 'letrec) (LETREC ms xs)]
      [(eq? f 'let) (LET ms xs)]
      [(eq? f 'if)
       (block "if " (EVAL ms (car xs))
              " then return " (EVAL ms (second xs))
              " else return " (EVAL ms (third xs))
              " end")]
      [(eq? f 'begin) (BEGIN ms xs)]
      [(eq? f 'set!) (set ms (car xs) (second xs))]
      [(eq? f 'ffi) (FFI (car xs))]
      [(eq? f 'quote) (QUOTE (car xs))]
      [else (stream (EVAL ms f) "(" (%apply ms xs) ")")])))

(define (BEGIN ms xs)
  (if (null? (cdr xs))
      (EVAL ms (car xs))
      (let ([xs (map (λ (x) (macroexpand ms x)) xs)])
        (block
         (map (λ (x)
                (var (second x)))
              (filter (λ (x) (and (pair? x) (eq? (car x) 'def))) xs))
         (let loop ([x (car xs)] [xs (cdr xs)])
           (if (and (pair? x) (or (eq? (car x) 'set!) (eq? (car x) 'def)))
               (stream
                (set ms (second x) (third x))
                (if (null? xs)
                    "return void"
                    (loop (car xs) (cdr xs))))
               (if (null? xs)
                   (stream
                    "return " (EVAL ms x))
                   (stream
                    "ig(" (EVAL ms x) ")"
                    (loop (car xs) (cdr xs))))))))))

(define (%apply ms xs)
  (cond
    [(null? xs) ""]
    [(null? (cdr xs)) (EVAL ms (car xs))]
    [else (stream (EVAL ms (car xs)) "," (%apply ms (cdr xs)))]))

(define (mkss xs)
  (cond
    [(null? xs) ""]
    [(null? (cdr xs)) (id (car xs))]
    [else (stream (id (car xs)) "," (mkss (cdr xs)))]))

(define-syntax-rule (ps [x v] ...)
  (make-hasheq
   (list
    (cons (quote x) (symbol->string (quote v))) ...)))

(define ids (ps
             [+ add]
             [- sub]
             [* mul]
             [/ quo]
             [and2 and2]
             [or2 or2]
             [not notf]
             [luatype type]
             [null? is_null]
             [pair? is_pair]
             [cons cons]
             [car car]
             [cdr cdr]
             [vec vector]
             [vec? is_vector]
             [vec-ref vector_ref]
             [symbol? is_symbol]
             [string->symbol symbol]
             [symbol->syring sym2str]
             [string? is_string]))
(define idc 0)

(define (mknewid x)
  (set! idc (+ 1 idc))
  (string-append
   "zs"
   (number->string idc)
   "_"
   (list->string (map
                  (λ (x) (if (or (char-alphabetic? x) (char-numeric? x)) x #\_))
                  (string->list (symbol->string x))))))

(define (id x)
  (hash-ref! ids x (λ () (mknewid x))))

(define (newvarid x)
  (hash-ref! ids x
             (λ ()
               (if (macrosym? x)
                   (string-append
                    "zsm"
                    (number->string (macrosym-id x))
                    "_"
                    (macrosym-sym x))
                   (mknewid x)))))

(define (LETREC ms xs)
  (let ([ps (car xs)])
    (block
     (map (λ (s)
            (var s))
          (map car ps))
     (map (λ (p)
            (stream (id (car p)) "=" (EVAL ms (second p)) " "))
          ps)
     "return " (BEGIN ms (cdr xs)))))

(define (LET ms xs)
  (let ([ps (car xs)])
    (stream
     (exp "function(" (mkss (map car ps)) ")"
          "return " (BEGIN ms (cdr xs))
          " end") "(" (%apply ms (map second ps)) ")")))

(define (EVAL ms x)
  (let ([x (macroexpand ms x)])
    (cond
      [(symbol? x) (id x)]
      [(pair? x) (APPLY ms (car x) (cdr x))]
      [(null? x) "null"]
      [(number? x) (number->string x)]
      [(eq? x #t) "true"]
      [(eq? x #f) "false"]
      [(string? x) (stream "\"" x "\"")]
      [else (error)])))

(define (e x)
  (cond
    [(or (null? x) (and (stream? x) (stream-empty? x))) ""]
    [(stream? x) (string-append (e (stream-first x)) (e (stream-rest x)))]
    [(list? x) (string-append (e (car x)) (e (cdr x)))]
    [else x]))

(define pre (file->string "prelude.lua"))

(define (c x)
  (endc (e (EVAL (make-hasheq) x))))

(define (endc x)
  (string-append
   pre
   "return " x))

(define (FFI x)
  (stream "l2sv(" (symbol->string x) ")"))

(define (macroexpand ms x)
  (cond
    [(and (pair? x) (eq? (car x) 'defmacro))
     (hash-set! ms (second x) (eval (third x) macrons))]
    [(and (pair? x) (hash-ref ms (car x) #f)) => (λ (mf) (macroexpand ms (mf (cdr x))))]
    [else x]))

(define (QUOTE x)
  (cond
    [(symbol? x) (stream "symbol(\"" (symbol->string x) "\")")]
    [(pair? x) (stream "cons(" (QUOTE (car x)) "," (QUOTE (cdr x)) ")")]
    [(null? x) "null"]
    [(eq? x #t) "true"]
    [(eq? x #f) "false"]
    [(number? x) (number->string x)]
    [(string? x) (stream "\"" x "\"")]
    [else (error)]))
