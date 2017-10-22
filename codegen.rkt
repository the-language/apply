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
(require "zscm.rkt")
(provide (all-defined-out) (all-from-out "zscm.rkt"))

(define-syntax builtinname
  (syntax-rules ()
    [(_ r) (string->symbol (string-append "__" (symbol->string (quote r))))]))

(define-syntax c%newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons
                      (cons
                       (builtinname r)
                       (symbol->string (quote s)))
                      (c%newns x ...))]
    [(_ r x ...) (cons
                  (cons
                   (builtinname r)
                   (symbol->string (quote r)))
                  (c%newns x ...))]))
(define-syntax-rule (%new-c-ns x ...)
  (make-hasheq
   (c%newns x ...)))
(define (c-getid ns x)
  (hash-ref!
   ns
   x
   (λ ()
     (apply string-append
            (cons "zs" (map
                        (λ (x) (if (or (char-alphabetic? x) (char-numeric? x))
                                   (string x)
                                   (string-append
                                    "_"
                                    (number->string (char->integer x)))))
                        (string->list (symbol->string x))))))))
(define-syntax-rule (new-c-getid geter x ...)
  (begin
    (define ns (%new-c-ns x ...))
    (define (geter v) (c-getid ns v))))

(define-syntax %newns%
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons
                      (cons
                       (builtinname r)
                       (quote s))
                      (%newns% x ...))]
    [(_ r x ...) (cons
                  (cons
                   (builtinname r)
                   (quote r))
                  (%newns% x ...))]))
(define-syntax-rule (lisp-newns x ...)
  (make-hasheq
   (%newns x ...)))
(define (lisp-getid ns x)
  (hash-ref! ns x (λ () (string->symbol (string-append "zs-" (symbol->string x))))))
(define-syntax-rule (new-lisp-getid geter x ...)
  (begin
    (define ns (lisp-newns x ...))
    (define (geter v) (lisp-getid ns v))))

(define (unbegin x)
  (if (and (pair? x) (eq? (car x) 'begin))
      (cdr x)
      (list x)))
