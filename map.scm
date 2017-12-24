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

(define $if *if)
(define $void *undefined)
(define $$apply **apply)
(define $$define **set!)
(define ($$lambda defines args xs x)
  (**lambda
   args
   (append
    (map **define-undefined defines)
    xs
    (**return x))))
(define ($$top defines xs)
  (**top
   (cons
    prelude
    (append
     (map **define-undefined defines)
     xs))))
(define ($$var x) (**var (string->symbol (string-append (symbol->string x) "Z"))))
(define $$number **number)
(define ($$char x) (**apply* (**var 'CHAR_) (list (**string (string x)))))
(define $$string **string)
(define $null (*vector '()))
(define $list *vector*)
(define $list-ref *vector-ref)
(define prelude
  (*exp
   '(begin
      (struct char?Z CHAR_ (x))
      (struct JPAIR_? JPAIR_ (a d))
      (define consZ
        (lambda (a d)
          (if-boolean/do (vector? d)
                         [(return (vector-append (vector a) d))]
                         [(return (JPAIR_ a d))])))
      (define IS_LIST_ (lambda (xs) (vector? xs)))
      (define CAR_
        (lambda (p)
          (return (if-boolean (JPAIR_? p) (@ p a) (vector-head p)))))
      (define CDR_
        (lambda (p)
          (return (if-boolean (JPAIR_? p) (@ p d) (vector-tail p))))))))
