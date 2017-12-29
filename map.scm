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

(load "z.scm")

(define ($$tail-if b xs ys) (list (**if b xs ys)))
(define $if *if)
(define $void *undefined)
(define $$apply **apply*)
(define ($$define x v) (**set! x v))
(define ($$lambda defines args xs)
  (**lambda
   args
   (append
    (map **define-undefined defines)
    xs)))
(define ($$tail-apply f xs) (list (**return ($$apply f xs))))
(define ($$tail-val x) (list (**return x)))
(define ($$top defines xs)
  (**top
   (append
    (map **define-undefined defines)
    xs)))
(define ($$var x) (**var x))
(define $$number **number)
(define ($$char x) (**apply* (**var 'CHAR_) (list (**string (string x)))))
(define $$string **string)
(define $null (*vector* '()))
(define $list *vector*)
(define $list-ref *vector-ref)
(define ($$record pred new fs)
  (**begin*
   (cons
    (**struct pred new fs)
    (map (λ (f)
           (**define f (**lambda '(x) (list (**return (**@ (**var 'x) f)))))) fs))))
(define $true *true)
(define $false *false)
(define prelude
  (*exp
   '(begin
      (define raiseZ (lambda (x) (raise x)))
      (define catchUZ
        (lambda (handler thunk)
          (try
           [(return (thunk))]
           e
           [(return (handler e))])))

      (define procedure?Z (lambda (x) (return (procedure? x))))
      (define applyUZ (lambda (f xs) (return (apply f xs))))

      (define boolean?Z (lambda (x) (return (boolean? x))))
      (define eq?Z (lambda (x y) (return (eq? x y))))

      (struct symbol?Z string->symbolUZ (x))
      (define string?Z (lambda (x) (return (string? x))))
      (define string-appendUZ (lambda (x y) (return (string-append x y))))
      (define symbol->stringUZ (lambda (x) (return (@ x x))))
      (define number->stringUZ (lambda (x) (return (number->string x))))
      (define string->numberUZ (lambda (x) (return (string->number x))))

      (struct char?Z char (x))
      (define list->stringUZ
        (lambda (xs)
          (return
           (list->string
            (vector-map
             (lambda (x) (return (@ x x))) xs)))))
      (define string->listUZ
        (lambda (s)
          (return
           (vector-map char (string->list s)))))
      (define char->integerUZ
        (lambda (c)
          (return (char->integer (@ c x)))))
      (define integer->charUZ
        (lambda (x)
          (return (char (integer->char x)))))

      (define null?Z
        (lambda (x)
          (return (and (vector? x) (eq? (vector-length x) 0)))))
      (struct JPAIR_? JPAIR_ (a d))
      (define pair?Z
        (lambda (x)
          (return (or (JPAIR_? x) (and (vector? x) (not-eq? (vector-length x) 0))))))
      (define consZ
        (lambda (a d)
          (if-boolean/do (vector? d)
                         [(return (vector-append (vector a) d))]
                         [(return (JPAIR_ a d))])))
      (define carUZ
        (lambda (p)
          (return (if-boolean (JPAIR_? p) (@ p a) (vector-head p)))))
      (define cdrUZ
        (lambda (p)
          (return (if-boolean (JPAIR_? p) (@ p d) (vector-tail p)))))
      (define list?Z (lambda (xs) (return (vector? xs))))
      (define appendUZ (lambda (xs ys) (return (vector-append xs ys))))
      (define mapUZ (lambda (f xs) (return (vector-map f xs))))

      (define number?Z (lambda (x) (return (number? x))))
      (define +UZ (lambda (x y) (return (+ x y))))
      (define -UZ (lambda (x y) (return (- x y))))
      (define *UZ (lambda (x y) (return (* x y))))
      (define /UZ (lambda (x y) (return (/ x y))))
      (define +UZ (lambda (x y) (return (+ x y))))
      (define <UZ (lambda (x y) (return (< x y))))
      (define >UZ (lambda (x y) (return (> x y))))
      (define =UZ (lambda (x y) (return (= x y))))
      (define <=UZ (lambda (x y) (return (<= x y))))
      (define >=UZ (lambda (x y) (return (>= x y))))
      )))
(define (+prelude x) (string-append prelude";"x))
