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

(define ($if b x y) (*if b x y))
(define $void *undefined)
(define ($$apply f xs) (**apply f xs))
(define ($$define x v) (**set! x v))
(define ($$lambda defines args xs x)
  (**lambda
   args
   (append
    (map **define-undefined defines)
    xs
    (**return x))))
(define ($$top defines xs)
  (**top
   (append
    (map **define-undefined defines)
    xs)))
(define $$var **var)
(define $$number **number)
(define ($$char x) (**new (**var 'CHAR_) (list (**string (string x)))))
(define $$string **string)
(define $null (**vector '()))
(define ($list xs) `(list ,@xs))
(define ($list-ref xs k) `(list-ref ,xs ,k))
(define prelude
  "
function PAIR_(a,d){this.a=a;this.d=d;}
function IS_PAIR_(x){return x instanceof Array || x instanceof PAIR_;}
function CONS_(a,d){if(d instanceof Array){return [a].concat(d);}else{return new PAIR_(a,d);}}
function IS_LIST_(x){return x instanceof Array;}
function CHAR_(x){this.c=x;}
function IS_CHAR_(x){return x instanceof CHAR_;}
")