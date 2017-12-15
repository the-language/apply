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
(define (LOADp dir xs)
  (if (null? xs)
      '()
      (let ([x (car xs)] [xs (cdr xs)])
        (if (and (pair? x) (eq? (car x) 'load))
            ((λ ()
              (define rcd (current-directory))
              (current-directory dir)
              (define f (simplify-path (second x)))
              (let-values ([(d i0 i1) (split-path f)])
                (define file (file->list f))
                (current-directory rcd)
                (append (LOADp d f) (LOADp dir xs)))))
            (cons x (LOADp dir xs))))))
; BUGs
(define (TOP ms xs k)
  (if (null? xs)
      '()
      (let ([x (car xs)] [xs (cdr xs)])
        (if (pair? x)
            (let ([a (car x)])
        (cond
          [(hash-ref ms a #f) => (λ (m) (TOP ms (cons (apply m (cdr x)) xs) k))]
          [(eq? a 'DEFMACROz) (TOP (hash-set ms (second x) (eval (third x))) xs k)]
