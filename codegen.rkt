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
(define-syntax %primcase
  (syntax-rules ()
    [(_ f v) v]
    [(_ f [p x] r ...) (if (eq? f (builtinname p)) x (%primcase f r ...))]
    [(_ f x r ...) (%primcase f [x (quote x)] r ...)]))
(define-syntax-rule (primcase f x ...)
  (let ([f0 f])
    (%primcase f0 x ...)))
(define (prim? x)
  (and (symbol? x)
       (match (string->list (symbol->string x))
         [`(#\_ #\_ . ,f) (string->symbol (list->string f))]
         [_ #f])))

(define (c-getid x)
  (apply string-append
         (cons "zs" (map
                     (λ (x) (if (or (char-alphabetic? x) (char-numeric? x))
                                (string x)
                                (string-append
                                 "_"
                                 (number->string (char->integer x)))))
                     (string->list (symbol->string x))))))
(define (lisp-getid x)
  (string->symbol (string-append "zs-" (symbol->string x))))

(define (unbegin x)
  (if (and (pair? x) (eq? (car x) 'begin))
      (cdr x)
      (list x)))

(define (FFI lang evaler xs)
  (if (eq? (first (car xs)) 'else)
      (evaler (car xs))
      (if (eq? lang (first (car xs)))
          (second (car xs))
          (FFI lang evaler (cdr xs)))))
