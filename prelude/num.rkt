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
 '((define (zero? x) (eq? x 0))
   (define (positive? x) (> x 0))
   (define (negative? x) (< x 0))
   (define (%max x y) (if (> x y) x y))
   (define (max x . xs) (foldl %max x xs))
   (define (%min x y) (if (< x y) x y))
   (define (min x . xs) (foldl %min x xs))
   ))
(prelude
 get
 (if (get '+-*/<>=)
     '((define < __<)
       (define > __>)
       (define = __=)
       (define <= __<=)
       (define >= __>=)
       (define + __+)
       (define - __-)
       (define * __*)
       (define / __/)
       )
     (let loop ([c '((define (_+_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__+/2 x y)
                               (error "+: isn't number" y))
                           (error "+: isn't number" x)))
                     (define (_-_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__-2 x y)
                               (error "-: isn't number" y))
                           (error "-: isn't number" x)))
                     (define (_*_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__*2 x y)
                               (error "*: isn't number" y))
                           (error "*: isn't number" x)))
                     (define (_/_ x y)
                       (if (number? x)
                           (if (number? y)
                               (__/2 x y)
                               (error "/: isn't number" y))
                           (error "/: isn't number" x)))
                     (define (+ . xs) (foldl _+_ 0 xs))
                     (define (* . xs) (foldl _*_ 1 xs))
                     (define (- x . xs)
                       (if (null? xs)
                           (_-_ 0 x)
                           (foldl (λ (n x) (_-_ x n)) x xs)))
                     (define (/ x . xs)
                       (if (null? xs)
                           (_/_ 1 x)
                           (foldl (λ (n x) (_/_ x n)) x xs))))]
                [ops (list
                      (cons '< '__<2)
                      (cons '> '__>2)
                      (cons '= 'eq?)
                      (cons '<= '__<=2)
                      (cons '>= '__>=2))])
       (if (null? ops)
           c
           (let* ([op (car ops)]
                  [f (cdr op)]
                  [fn (gensym f)]
                  [d (car op)]
                  [doop (gensym d)])
             (loop
              (append
               `((define (,fn x y)
                   (if (number? x)
                       (if (number? y)
                           (,f x y)
                           (error "isn't number" y))
                       (error "isn't number" x)))
                 (define (,doop x y xs)
                   (if (null? xs)
                       (,fn x y)
                       (and (,fn x y) (,doop y (car xs) (cdr xs)))))
                 (define (,d x y . xs) (,doop x y xs)))
               c)
              (cdr ops)))))))
