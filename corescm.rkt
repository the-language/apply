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
(require "alexpander.rkt")
(set-null-prog!
 '((define-syntax define-syntax-rule
     (syntax-rules ()
       ((_ (f . args) x) (define-syntax f
                           (syntax-rules ()
                             ((_ . args) x))))))
   (define-syntax-rule (λ . xs) (lambda . xs))
   (let-syntax ((def define))
     (define-syntax define
       (syntax-rules ()
         ((_ var init) (def var init))
         ((_ (var . args) . body) (define var (λ args . body))))))
   )
 '((define (append xs ys)
     (if (null? xs)
         ys
         (cons (car xs) (append (cdr xs) ys))))
   (define (list . xs)
     xs)
   (define (map f xs)
     (if (null? xs)
         '()
         (cons (f (car xs)) (map f (cdr xs)))))
   (define (equal? x y)
     (if (pair? x)
         (and (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y)))
         (eq? x y)))
   (define eqv? equal?))
 '())
(expand-program '())
