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
 [[(begin (define x 0) (define y 0)) y (displayln y)]
  '[(define x 0) (define y 0) (displayln y)]]
 [[(MODULEz (exp)
           [(m m1) (a a)]
           (define a 0)
           (DEFMACROz m1
                      (λ () 0)))
  (IMPALLz (exp))
  (displayln (m))]
 '[(define exp@_Mz ((lambda () (define a 0) (list a))))
  (define exp@a@Mz (list-ref exp@_Mz 0))
  (define a exp@a@Mz)
  (displayln 0)]]
 )