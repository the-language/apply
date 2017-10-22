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
(define-syntax newconf
  (syntax-rules ()
    [(_) (hasheq)]
    [(_ [c v] x ...) (hash-set (newconf x ...) (quote c) v)]
    [(_ c x ...) (hash-set (newconf x ...) (quote c) #t)]))
(define (conf-get c x) (hash-ref c x #f))

(define preludes '())
(define-syntax-rule (prelude get x)
  (set! preludes
        (cons (λ (c)
                (let ([get (λ (v) (conf-get c v))])
                  x))
              preludes)))
(define (runprelude conf)
  (foldl append '() (map (λ (f) (f conf)) preludes)))

(prelude
 get
 (if (get 'vector)
     '((define pair? __pair?)
       (define car __car)
       (define cdr __cdr)
       (define vector __vector)
       (define _vec?_ __vector?)
       (define _vec_len_ __vector-length)
       (define _vec_ref_ __vector-ref)
       (define list->vector __list->vector)
       (define _vec->lst_ __vector->list)
       (define (_vec_len_0?_ v) (zero? (_vec_len_ v))))
     '((define (pair? x) (and (__pair? x) (not (_vec?_ x))))
       (define (car p)
         (if (pair? p)
             (__car p)
             (error "car: isn't pair" p)))
       (define (cdr p)
         (if (pair? p)
             (__cdr p)
             (error "cdr: isn't pair" p)))
       (define (vector . xs) (cons '_%vec%_ xs))
       (define (_vec?_ x) (and (__pair? x) (eq? (__car x) '_%vec%_)))
       (define (_vec_len_ x)
         (if (_vec?_ x)
             (length (__cdr x))
             (error "vector-length: isn't vector" x)))
       (define (_vec_ref_ x i)
         (if (_vec?_ x)
             (list-ref (__cdr x) i)
             (error "vector-ref: isn't vector" x)))
       (define (list->vector xs) (cons '_%vec%_ xs))
       (define (_vec->lst_ x)
         (if (_vec?_ x)
             (__cdr x)
             (error "vector->list: isn't vector" x)))
       (define (_vec_len_0?_ x) (null? (_vec->lst_ x))))))

(runprelude (newconf))
