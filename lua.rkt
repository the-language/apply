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

(define-syntax-rule (f2 o)
  (exp "function(x,y)return x" o "y end"))

(define-syntax-rule (exp x ...)
  (stream "(" x ... ")"))

(define p
  (hash '+ (f2 "+")
        '- (f2 "-")
        '* (f2 "*")
        '/ (f2 "/")
        'and (f2 "and")
        'or (f2 "or")
        'luatype "type"))

(define-syntax-rule (call f x ...)
  (stream f "(" x ... ")"))

(define (APPLY f xs)
  (cond
    [(eq? f 'λ)
     (exp "function(" (%λ (car xs)) ")"
          "return " (EVAL (second xs))
          " end")]
    [(eq? f 'letrec) (LETREC xs)]
    [(eq? f 'if)
     (exp (exp "function()"
               "if " (EVAL (car xs))
               " then return " (EVAL (second xs))
               " else return " (EVAL (third xs))
               " end end") "()")]
    [(eq? f 'ffi) (FFI (car xs))]
    [else (call (EVAL f) (%apply xs))]))

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

(define (id x)
  (symbol->string x)) ;; Fix

(define (LETREC xs)
  (let ([ps (car xs)])
    (exp
     (exp "function()"
          (map (λ (s)
                 (stream "local " (id s) "=nil "))
               (map car ps))
          (map (λ (p)
                 (stream (id (car p)) "=" (EVAL (second p)) " "))
               ps)
          "return " (EVAL (second xs))
          " end") "()")))

(define (EVAL x)
  (cond
    [(symbol? x) (hash-ref p x (id x))]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(null? x) "nil"]
    [(number? x) (number->string x)]
    [(eq? x #t) "true"]
    [(eq? x #f) "false"]
    [else (error)]))

(define (e x)
  (cond
    [(or (null? x) (and (stream? x) (stream-empty? x))) ""]
    [(stream? x) (string-append (e (stream-first x)) (e (stream-rest x)))]
    [(list? x) (string-append (e (car x)) (e (cdr x)))]
    [else x]))

(define (c x) (e (EVAL x)))

(define (FFI x) (error))
