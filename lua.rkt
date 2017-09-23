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

(define-syntax-rule (exp x ...)
  (stream "(" x ... ")"))

(define-syntax-rule (block x ...)
  (stream (exp "function()"
               x ...
               " end") "()"))

(define (APPLY f xs)
  (cond
    [(eq? f 'λ)
     (exp "function(" (%λ (car xs)) ")"
          "return " (EVAL (second xs))
          " end")]
    [(eq? f 'letrec) (LETREC xs)]
    [(eq? f 'let) (LET xs)]
    [(eq? f 'if)
     (block "if " (EVAL (car xs))
            " then return " (EVAL (second xs))
            " else return " (EVAL (third xs))
            " end")]
    [(eq? f 'ffi) (FFI (car xs))]
    [else (stream (EVAL f) "(" (%apply xs) ")")]))

(define (%apply xs)
  (cond
    [(null? xs) ""]
    [(null? (cdr xs)) (EVAL (car xs))]
    [else (stream (EVAL (car xs)) "," (%apply (cdr xs)))]))

(define (%λ xs)
  (cond
    [(null? xs) ""]
    [(null? (cdr xs)) (id (car xs))]
    [else (stream (id (car xs)) "," (%λ (cdr xs)))]))

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
             [vec-ref vector_ref]))
(define idc 0)

(define (id x)
  (hash-ref! ids x
             (λ ()
               (set! idc (+ 1 idc))
               (string-append
                "zs"
                (number->string idc)
                "_"
                (list->string (map
                               (λ (x) (if (or (char-alphabetic? x) (char-numeric? x)) x #\_))
                               (string->list (symbol->string x))))))))

(define (LETREC xs)
  (let ([ps (car xs)])
    (block
     (map (λ (s)
            (stream "local " (id s) "=nil "))
          (map car ps))
     (map (λ (p)
            (stream (id (car p)) "=" (EVAL (second p)) " "))
          ps)
     "return " (EVAL (second xs)))))

(define (LET xs)
  (let ([ps (car xs)])
    (stream
     (exp "function(" (%λ (map car ps)) ")"
          "return " (EVAL (second xs))
          " end") "(" (%apply (map second ps)) ")")))

(define (EVAL x)
  (cond
    [(symbol? x) (id x)]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(null? x) "null"]
    [(number? x) (number->string x)]
    [(eq? x #t) "true"]
    [(eq? x #f) "false"]
    [(string? x) x]
    [else (error)]))

(define (e x)
  (cond
    [(or (null? x) (and (stream? x) (stream-empty? x))) ""]
    [(stream? x) (string-append (e (stream-first x)) (e (stream-rest x)))]
    [(list? x) (string-append (e (car x)) (e (cdr x)))]
    [else x]))

(define pre (file->string "prelude.lua"))

(define (c x)
  (endc (e (EVAL x))))

(define (endc x)
  (string-append
   pre
   "return " x))

(define (FFI x)
  (stream "l2sv(" (symbol->string x) ")"))
