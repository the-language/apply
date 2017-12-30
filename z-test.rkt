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
(require "z.rkt")
(require rackunit)
(define-syntax-rule (test f [src dist] ...)
  (begin
    (check-equal? (f (quote src)) dist) ...))
(test
 z-current
 [((displayln (not #t)))
  '((define not (lambda (x) (if x #f #t)))
  (define-record-type
   error-object?
   (error-object error-object-message error-object-irritants)
   error-object?
   (error-object-message error-object-message)
   (error-object-irritants error-object-irritants))
  (define raise raise)
  (define CATCHz (lambda (t h) (guard (e (#t (h e))) (t))))
  (define error
    (lambda (message . irritants) (raise (error-object message irritants))))
  (define CARz car)
  (define CDRz cdr)
  (define pair? pair?)
  (define car
    (lambda (p) (if (pair? p) (CARz p) (error "car: isn't a pair:" p))))
  (define cdr
    (lambda (p) (if (pair? p) (CDRz p) (error "cdr: isn't a pair:" p))))
  (define list? list?)
  (define cons cons)
  (displayln (not #t)))])