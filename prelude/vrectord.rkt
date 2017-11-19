#lang racket
;;  vrectord = vector + record
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
 (if (get 'vector)
     '((define (pair? x) (__pair? x))
       (define _vec_ __vector) ; _vec_ 用不到
       (define (_vec?_ x) (__vector? x))
       (define (_vec_len_ x) (__vector-length x))
       (define (_vec_ref_ vector k) (__vector-ref vector k))
       (define (_lst2vec_ x)
         (if (list? x)
             (__list->vector x)
             (error "list->vector: isn't list" x)))
       (define (_vec->lst_ x) (__vector->list x))
       )
     '((define (pair? x) (and (__pair? x) (not (_vec?_ x))))
       (define (_vec_ . xs) (cons '_%vec%_ xs))  ; _vec_ 用不到
       (define (_vec?_ x) (and (__pair? x) (eq? (__car x) '_%vec%_)))
       (define (_vec_len_ x) (length (__cdr x)))
       (define (_vec_ref_ x i) (list-ref (__cdr x) i))
       (define (_lst2vec_ xs) (cons '_%vec%_ xs))
       (define (_vec->lst_ x) (__cdr x))
       )))

(prelude
 get
 '((defmacro define-record-type
     (λ (name constructor pred . fs)
       (define (mkc fs)
         (if (null? fs)
             '()
             (cons
              (match (car fs)
                [`(,x ,a) x]
                [`(,x ,a ,set) `(atom! ,x)])
              (mkc (cdr fs)))))
       (define (deffs name pred c fs)
         (if (null? fs)
             '()
             (append
              (match (car fs)
                [`(,f ,a)
                 `((define (,a x)
                     (if (,pred x)
                         (_vec_ref_ x ,c)
                         (error ,(symbol->string name) ,(symbol->string a) x))))]
                [`(,f ,a ,set)
                 `((define (,a x)
                     (if (,pred x)
                         (atom-get (_vec_ref_ x ,c))
                         (error ,(symbol->string name) ,(symbol->string a) x)))
                   (define (,set x v)
                     (if (,pred x)
                         (atom-set! (_vec_ref_ x ,c) v)
                         (error ,(symbol->string name) ,(symbol->string a) x))))])
              (deffs name pred (+ 1 c) (cdr fs)))))
       `(begin
          (define ,constructor
            (_vec_
             #t
             (quote ,name)
             ,@(mkc fs)))
          (define (,pred x)
            (and
             (_vec?_ x)
             (_vec_ref_ x 0)
             (eq? (_vec_ref_ x 1) (quote ,name))))
          ,@(deffs name pred 2 fs))))
   (define (struct? x)
     (and (_vec?_ x)
          (_vec_ref_ x 0)))
   (define (vector? x) (and (_vec?_ x) (not (_vec_ref_ x 0))))
   (define (list->vector xs) (_lst2vec_ (cons #f xs)))
   (define (vector . xs) (list->vector xs))
   (define (_struct_type_ x)
     (if (struct? x)
         (_vec_ref_ x 1)
         (error "isn't struct" x)))
   (define (struct->list x) ;BUG 可变的struct 结果不正确
     (if (struct? x)
         (cddr (_vec->lst_ x))
         (error "struct->list: isn't struct" x)))
   (define (struct->vector x)
     (if (struct? x)
         (list->vector (cons (string->symbol
                              (string-append "struct:" (symbol->string (_struct_type_ x))))
                             (struct->list x)))
         (error "struct->vector: isn't struct" x)))
   (define (vector-length v)
     (if (vector? v)
         (- (_vec_len_ v) 1)
         (error "vector-length: isn't vector" v)))
   (define (vector-ref v i)
     (if (vector? v)
         (_vec_ref_ v (+ i 1))
         (error "vector-ref: isn't vector" v)))
   (define (vector->list v)
     (if (vector? v)
         (cdr (_vec->lst_ v))
         (error "vector->list: isn't vector" v)))
   ))
