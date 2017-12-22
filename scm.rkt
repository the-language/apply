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

(require compatibility/defmacro)
(require srfi/9) ; R7RS
(require racket/sandbox)
(define EVAL (make-evaluator 'racket))
(define null-set (set))
(define null-hash (hash))
;(define (dir-of/file->list dir path k)
;  (define rcd (current-directory))
;  (current-directory dir)
;  (define f (simplify-path path))
;  (define-values (d i0 i1) (split-path f))
;  (define xs (file->list f))
;  (current-directory rcd)
;  (k d xs))
(define (partition/k f xs k)
  (let-values ([(x y) (partition f xs)])
    (k x y)))
(define-macro (load x) `(include ,x))
