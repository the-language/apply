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
            cons
            [%car car]
            [%cdr cdr]
            [%pair? pair?]
            null?
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
            eq?
            error
            boolean?
            procedure?
            apply
            raise
            with-exception-handler
            displayln
            ))
(define (id x) (newid x))
(define (newid x)
  (hash-ref! ns x (λ () (string->symbol (string-append "zs-" (symbol->string x))))))

(define (EVAL x)
  (cond
    [(eq? x 'host-language) "scheme"]
    [(pair? x) (APPLY (car x) (cdr x))]
    [(eq? x 'void) '(lambda () (if #f #f))]
    [(symbol? x) (id x)]
    [else x]))
(define (APPLY f xs)
  (match f
    ['lambda (LAMBDA (first xs) (second xs))]
    ['begin (BEGIN xs)]
    ['void '(if #f #f)]
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

(compiler c [number display ffi if2] feval)

(define (feval x)
  (cons '(define displayln (lambda (x) (begin (display x) (newline))))
        (unbegin (EVAL x))))
(define (unbegin x)
  (if (eq? (car x) 'begin)
      (cdr x)
      (error "unbegin")))

(writeln (cons 'begin (c (read))))
