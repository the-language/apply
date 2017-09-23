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

(provide pp)

(define (pp s)
  (let-values ([(dms os) (partition (λ (x) (and (pair? x) (eq? (car x) 'defmacro))) s)])
    (let ([ms (map (λ (x) (cons (second x) (eval (third x)))) dms)])
      (EVAL (make-hasheq ms) os))))

(define (EVAL ms s)
  (cond
    [(pair? s) (APPLY ms (car s) (cdr s))]
    [else s]))

(define (APPLY ms f x)
  (cond
    [(hash-ref ms f #f) => (λ (m) (EVAL ms (apply m x)))]
    [else (cons f (map (λ (y) (EVAL ms y)) x))]))
