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
(provide scm)
(require "codegen.rkt")
(new-lisp-getid
 id

  null?
pair?
cons
car
cdr

error
raise
with-exception-handler

<lambda>
<begin>
procedure?
apply

string-append
string?

symbol?
symbol->string
string->symbol

boolean?
<if>

number?
number->string
string->number
eq?
+/2
-/2
*2
/2
<2
>2
<=2
>=2

            ))
(define (id x) (newid x))
(define (newid x)
  (hash-ref! ns x (λ () (string->symbol (string-append "zs-" (symbol->string x))))))

(define (EVAL x)
  (cond
    [(eq? x 'host-language) "scheme"]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x) (id x)]
    [else x]))
(define (APPLY f xs)
  (match f
    ['lambda (LAMBDA (first xs) (second xs))]
    ['begin (BEGIN xs)]
    ['quote (QUOTE (car xs))]
    ['ffi (if (null? (cdr xs)) (car xs) (error "APPLY: ffi" f xs))]
    [_ (cons (EVAL f) (map EVAL xs))]))
(define (QUOTE x) (list 'quote x))
(define (BEGIN xs)
  (cond
    [(null? xs) (EVAL '(void))]
    [(null? (cdr xs)) (EVAL (car xs))]
    [else
     (cons 'begin
           (map (λ (x)
                  (if (and (pair? x) (eq? (car x) 'define))
                      (if (null? (cdddr x))
                          `(define ,(newid (cadr x)) ,(EVAL (caddr x)))
                          (error "BEGIN: define" xs))
                      (EVAL x))) xs))]))
(define (LAMBDA args x)
  `(lambda ,(%LAMBDA args)
     ,(EVAL x)))
(define (%LAMBDA x)
  (cond
    [(null? x) '()]
    [(symbol? x) (newid x)]
    [(pair? x) (cons (%LAMBDA (car x)) (%LAMBDA (cdr x)))]
    [else (error "%LAMBDA" x)]))

(compiler c [number display ffi] feval)

(define (feval x)
  (unbegin (EVAL x)))
(define (unbegin x)
  (if (eq? (car x) 'begin)
      (cdr x)
      (error "unbegin")))

(writeln (cons 'begin (c (read))))
