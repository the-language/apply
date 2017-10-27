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
(provide lua)
(require "codegen.rkt")
(define-syntax-rule (includes f)
  (include/reader
   f
   (λ (source-name in)
     (let ([x (read-line in)])
       (if (eof-object? x)
           eof
           (let loop ([s x])
             (let ([x (read-line in)])
               (if (eof-object? x)
                   (datum->syntax #f s)
                   (loop (string-append s "\n" x))))))))))
(define pre (includes "lua.lua"))
(define id c-getid)

(define ++
  (case-lambda
    [() ""]
    [(x . xs) (if (list? x)
                  (apply ++ (cons (apply ++ x) xs))
                  (string-append x (apply ++ xs)))]))
(define-syntax-rule (exp x ...)
  (++ "(" x ... ")"))
(define (function xs . vs)
  (exp "function(" (apply %function xs) ")"
       (apply ++ vs) "end"))
(define (function... xs rest . vs)
  (exp "function(" (apply %function (append xs (list "..."))) ")"
       "local " rest "=list(...)\n"
       (apply ++ vs) "end"))
(define %function
  (case-lambda
    [() ""]
    [(a) a]
    [(a . xs) (++ a "," (apply %function xs))]))
(define-syntax-rule (var x)
  (++ "local " x "\n"))
(define-syntax-rule (newvar+set x v)
  (++ "local " x "=" v "\n"))
(define-syntax-rule (var-set x v)
  (++ x "=" v "\n"))
(define-syntax-rule (return x)
  (++ "return " x "\n"))
(define-syntax-rule (cmd-if b x y)
  (++ "if " b " then\n"
      x
      "else\n"
      y
      "end\n"))
(define (cmd-apply f . xs)
  (++ f "(" (apply %function xs) ")"))
(define (cmd-eval x)
  (++ "ig(" x ")"))
(define (block . xs)
  (cmd-apply (apply function (cons '() xs))))

(define (EVAL x)
  (cond
    [(eq? x 'host-language) "lua"]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x) (primcase
                  x
                  [pair? "is_pair"]
                  [cons "cons"]
                  [car "car"]
                  [cdr "cdr"]
                  [symbol? "is_symbol"]
                  [symbol->string "sym2str"]
                  [raise "raise"]
                  [apply "apply"]
                  [with-exception-handler "weh"]
                  [putstr "io.write"]
                  [newline "print()"]
                  [atom! "atom"]
                  [atom-get "atomget"]
                  [atom-set! "atomset"]
                  [atom-map! "atommap"]
                  [atom? "isatom"]
                  [vector "vec"]
                  [vector? "isvec"]
                  [vector-length "veclen"]
                  [vector-ref "vecref"]
                  [list->vector "lst2vec"]
                  [vector->list "vec2lst"]
                  [number->string "tostring"]
                  (id x))]
    [else (QUOTE x)]))
(define (type s x)
  (cmd-apply (exp "function(x)return type(x)==\"table\"and x[1]==" s) x))
(define (APPLY f xs)
  (match f
    ['if
     (block
      (cmd-if (EVAL (first xs))
              (return (EVAL (second xs)))
              (return (EVAL (third xs)))))]
    ['lambda (LAMBDA (first xs) (%BEGIN (cdr xs)))]
    ['begin (BEGIN xs)]
    ['quote (QUOTE (first xs))]
    ['ffi (if (null? (cdr xs)) (car xs) (error "APPLY: ffi" f xs))]
    [_
     (primcase
      f
      [null? (exp (EVAL (first xs)) "==null")]

      [procedure? (exp "type(" (EVAL (first xs)) ")==\"function\"")]

      [string-append (exp (EVAL (first xs)) ".." (EVAL (second xs)))]
      [string? (exp "type(" (EVAL (first xs)) ")==\"string\"")]
      [str->lst (exp "function(s)
local r={}
for i=1,#s do
r[i]=str:sub(i,i)
end
return lst(r)
end")]

      [string->symbol (exp "{symbolt," (EVAL (first xs)) "}")]

      [boolean? (exp "type(" (EVAL (first xs)) ")==\"boolean\"")]

      [number? (exp "type(" (EVAL (first xs)) ")==\"number\"")]
      [string->number (exp (EVAL (first xs)) "+0")]
      [eq? (exp (EVAL (first xs)) "=" (EVAL (second xs)))]
      [+/2 (exp (EVAL (first xs)) "+" (EVAL (second xs)))]
      [-/2 (exp (EVAL (first xs)) "-" (EVAL (second xs)))]
      [*2 (exp (EVAL (first xs)) "*" (EVAL (second xs)))]
      [/2 (exp (EVAL (first xs)) "/" (EVAL (second xs)))]
      [<2 (exp (EVAL (first xs)) "<" (EVAL (second xs)))]
      [>2 (exp (EVAL (first xs)) ">" (EVAL (second xs)))]
      [<=2 (exp (EVAL (first xs)) "<=" (EVAL (second xs)))]
      [>=2 (exp (EVAL (first xs)) ">=" (EVAL (second xs)))]

      (apply cmd-apply (cons (EVAL f) (map EVAL xs))))]))
(define (LAMBDA xs x)
  (if (list? xs)
      (function (map id xs) x)
      (let-values ([(h t) (ends xs)])
        (function... (map id h) (id t) x))))
(define (ends xs)
  (if (symbol? xs)
      (values '() xs)
      (let-values ([(h t) (ends (cdr xs))])
        (values (cons (car xs) h) t))))
(define (BEGIN xs) (block (%BEGIN xs)))
(define (%BEGIN xs)
  (++
   (map (λ (x) (var (id (second x))))
        (filter (λ (x) (and (pair? x) (eq? (first x) 'define))) xs))
   (let-values  ([(h tl) (split-at-right xs 1)])
     (++ (map
          (λ (x)
            (if (and (pair? x) (eq? (first x) 'define))
                (var-set (id (second x)) (EVAL (third x)))
                (cmd-eval (EVAL x))))
          h)
         (let ([t (car tl)])
           (if (and (pair? t) (eq? (first t) 'define))
               (list
                (var-set (id (second t)) (EVAL (third t)))
                (return "void"))
               (return (EVAL t))))))))
(define (QUOTE x)
  (cond
    [(symbol? x) (++ "symbol(\"" (symbol->string x) "\")")]
    [(pair? x) (++ "cons(" (QUOTE (car x)) "," (QUOTE (cdr x)) ")")]
    [(null? x) "null"]
    [(eq? x #t) "true"]
    [(eq? x #f) "false"]
    [(number? x) (number->string x)]
    [(string? x) (format "~s" x)]
    [(char? x) (++ "char(" (format "~s" (string x)) ")")]
    [else (error 'quote x)]))

(define (feval x)
  (++
   pre
   (%BEGIN (unbegin x))))

(compiler lua [display quote atom [charstr 'nochar] vector [split 10]] feval)
