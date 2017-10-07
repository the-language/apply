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
;(provide c)
(require "corescm.rkt")
(define-syntax %newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons (cons (quote r) (symbol->string (quote s))) (%newns x ...))]
    [(_ r x ...) (cons (cons (quote r) (symbol->string (quote r))) (%newns x ...))]))
(define-syntax-rule (newns x ...)
  (make-hasheq
   (%newns x ...)))
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

(define ns (newns
            list
            list?
            map
            [with-exception-handler withexceptionhandler]
            [error zerror]
            [raise raise]
            [%+ add]
            [%- sub]
            [%* mul]
            [%/ quo]
            cons
            [%car car]
            [%cdr cdr]
            [%pair? is_pair]
            [null? is_null]
            [%< lt]
            [%> gt]
            [%= eq]
            [number? is_number]
            [char? is_char]
            [string? is_string]
            [string->list string2list]
            [symbol? is_symbol]
            [eq? equal]
            [equal? equal]
            [%vector->list vec2list]
            [list->vector list2vec]
            vector
            [%vector? is_vector]
            [%vector-length veclen]
            [%vector-ref vector_ref]
            [atom? is_atom]
            [atom! atom]
            [atom-map! atom_map]
            [atom-set! atom_set]
            [atom-get atom_get]
            [void voidf]
            displayln))

(define (id x) (newid x))
(define (newid x)
  (hash-ref!
   ns
   x
   (λ ()
     (apply ++
            (cons "zs_" (map
             (λ (x) (if (or (char-alphabetic? x) (char-numeric? x))
                        (string x)
                        (list
                         "_"
                         (number->string (char->integer x))
                         "_")))
             (string->list (symbol->string x))))))))

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
    [(eq? x 'host-language) "\"lua\""]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x) (id x)]
    [else (QUOTE x)]))
(define (APPLY f xs)
  (match f
    ['if
     (block
      (cmd-if (EVAL (first xs))
              (return (EVAL (second xs)))
              (return (EVAL (third xs)))))]
    ['lambda (LAMBDA (first xs) (second xs))]
    ['begin (BEGIN xs)]
    ['void "void"]
    ['quote (QUOTE (first xs))]
    [_ (apply cmd-apply (cons (EVAL f) (map EVAL xs)))]))
(define (LAMBDA xs x)
  (if (list? xs)
      (function (map newid xs) (return (EVAL x)))
      (let-values ([(h t) (ends xs)])
        (function... (map newid h) (newid t) (return (EVAL x))))))
(define (ends xs)
  (if (symbol? xs)
      (values '() xs)
      (let-values ([(h t) (ends (cdr xs))])
        (values (cons (car xs) h) t))))
(define (BEGIN xs)
  (block
   (map (λ (x) (var (newid (second x))))
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
    [else (error)]))

(define (feval x)
  (++
   pre
   (return (cmd-apply "scmto" (EVAL x)))))

(compiler c [ffi atom vector list display] feval)

(displayln (c (read)))
