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

(provide z z-current)

(define ($$tail-if b xs ys)
  (list ($if b
             (if (null? (cdr xs)) (car xs) `((λ () ,@xs)))
             (if (null? (cdr ys)) (car ys) `((λ () ,@ys))))))
(define ($if b x y) `(if ,b ,x ,y))
(define $void 'VOIDz)
(define ($$apply f xs) (cons f xs))
(define ($$define x v) `(define ,x ,v))
(define ($$lambda vars defines args xs) `(lambda ,args ,@xs))
(define ($$tail-apply f xs) (list ($$apply f xs)))
(define ($$tail-val x) (list x))
(define ($$top defines xs) xs)
(define ($$var x) x)
(define ($$number x) x)
(define ($$char x) x)
(define ($$string x) x)
(define $null ''())
(define $true #t)
(define $false #f)
(define ($list xs) `(list ,@xs))
(define ($list-ref xs k) `(list-ref ,xs ,k))
(define ($$record pred cons fs) `(define-record-type ,pred (,cons ,@fs) ,pred ,@(map (λ (x) `(,x ,x)) fs)))
(define ($host-exp x) x)
(define $host 'scheme)

(include "scm.rkt")
(include "z.scm")
