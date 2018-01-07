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
(load "JSLISP/js.scm")
(define ($var/k local-state state x k) (k local-state state (**var x)))
(define ($num/k local-state state x k) (k local-state state (**number x)))
(define ($sym/k local-state state x k) (k local-state state (**apply* "SYMz" (list (**string x)))))
(define ($char/k local-state state x k) (k local-state state (**apply* "CHARz" (list (**string (string x))))))
(define ($str/k local-state state x k) (k local-state state (**string x)))
(define ($$val/k local-state state x k) (k local-state state (list x)))
(define ($$tail-val/k local-state state x k) (k local-state state (list (**return x))))
(define ($$define/k local-state state f x k) (k (cons f local-state) state (list (**set! f x))))
(define $void "VOIDz")
(define $null "NULLz")
(define ($$if/k local-state state b xs x ys y k)
  (if (and (null? xs) (null? ys))
      (k local-state state '() (*if b x y))
      (let ([v (gensym)])
        (k local-state state (list (**if b (append xs (list (**set! v x))) (append ys (list (**set! v y))))) (**var v)))))
(define ($$if-tail/k local-state state b xs ys k) (k local-state state (list (**if b xs ys))))
(define ($$apply/k local-state state f args k) (k local-state state '() (**apply* f args)))
(define ($$tail-apply/k local-state state f args k) (k local-state state (list (**return (**apply* f args)))))
(define ($!pre-define-lambda local-state state parm k) (k '() state))
(define ($!pre-lambda local-state state parm k) (k '() state))
(define ($$define-lambda/k local-state local-state1 state name parm xs k)
  (k (cons name local-state) state (list (**define name (**lambda parm xs)))))
(define ($$lambda/k local-state local-state1 state parm xs k)
  (k local-state state '() (**lambda parm (append (map **define-undefined local-state1) xs))))
(define $null-local-state null-hash)
(define $null-env null-env)
(define $null-state null-hash)
(define ($$top local-state state xs) (**top (append (map **define-undefined local-state) xs)))
(define $arch 'js)
(define ($$host-expr/k local-state state x k) (k local-state state '() (*exp x)))
(define ($$host-expr-tail/k local-state state x k) (k local-state state (list (**return (*exp x)))))