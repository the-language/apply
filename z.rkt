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

(require compatibility/defmacro)
;(define %eval%ns (make-base-namespace))
(require srfi/9) ; R7RS
;(define (EVAL x) (eval x %eval%ns))
(require racket/sandbox)
(define EVAL (make-evaluator 'racket))
(provide z z-current)
(define null-set (set))
(define null-hash (hash))
; map/symbol = Hash Symbol _
(define null-map/symbol (hasheq))
(define map/symbol-set hash-set)
(define map/symbol-get hash-ref)
(define map/symbol-has? hash-has-key?)
(define (dir-of/file->list dir path k)
  (define rcd (current-directory))
  (current-directory dir)
  (define f (simplify-path path))
  (define-values (d i0 i1) (split-path f))
  (define xs (file->list f))
  (current-directory rcd)
  (k d xs))
(define (partition/k f xs k)
  (let-values ([(x y) (partition f xs)])
    (k x y)))
(define-macro (load x) `(include ,x))

;(struct $if (b x y) #:transparent)
;(struct $VOID ())
;(define $void ($VOID))
;(struct $$apply (f xs) #:transparent)
;(struct $$define (x v) #:transparent)
;(struct $$lambda (defines args xs x) #:transparent)
;(struct $$top (defines xs) #:transparent)
;(struct $$var (x) #:transparent)
;(struct $$number (x) #:transparent)
;(struct $$char (x) #:transparent)
;(struct $$string (x) #:transparent)
;(struct $NULL ())
;(define $null ($NULL))
;(struct $list (xs) #:transparent)
;(struct $list-ref (xs k) #:transparent)

(define ($if b x y) `(if ,b ,x ,y))
(define $void 'VOIDz)
(define ($$apply f xs) (cons f xs))
(define ($$define x v) `(define ,x ,v))
(define ($$lambda defines args xs x) `(lambda ,args ,@xs ,x))
(define ($$top defines xs) xs)
(define ($$var x) x)
(define ($$number x) x)
(define ($$char x) x)
(define ($$string x) x)
(define $null ''())
(define ($list xs) `(list ,@xs))
(define ($list-ref xs k) `(list-ref ,xs ,k))

(load "z.scm")
