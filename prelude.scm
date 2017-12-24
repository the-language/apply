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
(DEFMACROz define-macro
           (λ (id . body)
             (if (pair? id)
                 `(DEFMACROz ,(car id) (λ ,(cdr id) ,@body))
                 `(DEFMACROz ,id ,@body))))
(define-macro (defmacro id formals . body)
  `(define-macro ,id (λ ,formals ,@body)))
(define-macro (quasiquote x)
  (define (Q n x)
    (cond
      [(pair? x)
       (let ([f (first x)])
         (cond
           [(eq? f 'unquote)
            (if (zero? n)
                (second x)
                (list 'list ''unquote (Q (- n 1) (second x))))]
           [(and (pair? f) (eq? (first f) 'unquote-splicing))
            (if (zero? n)
                (list 'append (second f) (Q 0 (cdr x)))
                (list 'cons (list 'list ''unquote-splicing (Q (- n 1) (second f))) (Q n (cdr x))))]
           [(eq? f 'quasiquote) (list 'list ''quasiquote (Q (+ n 1) (second x)))]
           [else (list 'cons (Q n f) (Q n (cdr x)))]))]
      [else (list 'quote x)]))
  (Q 0 x))

(define (not x) (if x #f #t))
(define-macro (and . xs)
  (cond
    [(null? xs) #t]
    [(null? (cdr xs)) (car xs)]
    [else `(if ,(car xs)
                (and ,@(cdr xs))
                #f)]))
(define-macro (or . xs)
  (cond
    [(null? xs) #f]
    [(null? (cdr xs)) (car xs)]
    [else (let ([s (gensym)])
            `(let ([,s ,(car xs)])
               (if ,s
                   ,s
                   (or ,@(cdr xs)))))]))
(define-macro (let ps . vs)
  `((λ ,(map first ps) ,@vs) ,@(map second ps)))
