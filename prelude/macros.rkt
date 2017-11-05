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
(require "../conf.rkt")

(prelude
 get
 '((defmacro and
     (λ xs
       (if (null? xs)
           #t
           (if (null? (cdr xs))
               (car xs)
               (let ([s (gensym)])
                 `(let ([,s ,(car xs)])
                    (if ,s
                        (and ,@(cdr xs))
                        #f)))))))
   (defmacro or
     (λ xs
       (if (null? xs)
           #f
           (if (null? (cdr xs))
               (car xs)
               (let ([s (gensym)])
                 `(let ([,s ,(car xs)])
                    (if ,s
                        ,s
                        (or ,@(cdr xs)))))))))
   (defmacro let
     (λ (p . xs)
       `((λ ,(map car p)
           ,@xs) ,@(map second p))))
   (defmacro letrec
     (λ (p . xs)
       `(begin
          ,@(map
             (λ (x)
               `(define ,(car x) ,(second x)))
             p)
          ,@xs)))
   (defmacro let*
     (λ (p . xs)
       (if (null? p)
           `(begin ,@xs)
           (let ([x (car p)])
             `(let ([,(car x) ,(second x)])
                (let* ,(cdr p) ,@xs))))))
   (defmacro cond
     (λ xs
       (if (null? xs)
           `(error "cond")
           (let ([c (car xs)])
             (let ([g (car c)] [v (cdr c)])
               (if (eq? g 'else)
                   `(begin ,@v)
                   `(if ,g
                        (begin ,@v)
                        (cond ,@(cdr xs)))))))))
   ))
