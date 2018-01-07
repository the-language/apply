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
(define ($var/k local-state state x k) (k local-state state x))
(define ($num/k local-state state x k) (k local-state state x))
(define ($sym/k local-state state x k) (k local-state state `(quote ,x)))
(define ($char/k local-state state x k) (k local-state state x))
(define ($str/k local-state state x k) (k local-state state x))
(define ($$val/k local-state state x k) (k local-state state `(,x)))
(define ($$tail-val/k local-state state x k) (k local-state state `(,x)))
(define ($$define/k local-state state f x k) (k local-state state `((define ,f ,x))))
(define $void `!void)
(define $null `!null)
(define ($$if/k local-state state b xs x ys y k) (k local-state state '()
                                                    (if (and (null? xs) (null? ys))
                                                             `(if ,b ,x ,y)
                                                    `(if ,b (begin ,@xs ,x) (begin ,@ys y)))))
(define ($$if-tail/k local-state state b xs ys k) (k local-state state
                                                     (if (and (null? (cdr xs)) (null? (cdr ys)))
                                                         `((if ,b ,(car xs) ,(car ys)))
                                                     `((if ,b (begin ,@xs) (begin ,@ys))))))
(define ($$apply/k local-state state f args k) (k local-state state '() (cons f args)))
(define ($$tail-apply/k local-state state f args k) (k local-state state `((,f ,@args))))
(define ($!pre-define-lambda local-state state parm k) (k local-state state))
(define ($!pre-lambda local-state state parm k) (k local-state state))
(define ($$define-lambda/k local-state local-state1 state name parm xs k) (k local-state state `((define ,name (lambda ,parm ,@xs)))))
(define ($$lambda/k local-state local-state1 state parm xs k) (k local-state state '() `(lambda ,parm ,@xs)))
(define $null-local-state null-hash)
(define $null-env null-env)
(define $null-state null-hash)
(define ($$top local-state state xs) xs)
(define $arch ''scheme)