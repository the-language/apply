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
 '((define hasheq hash)
   (define hasheqv hash)
   (define make-immutable-hasheqv make-immutable-hash)
   (define make-immutable-hasheq make-immutable-hash)
   (define (hash-update hash key updater . f)
     (hash-set
      hash
      key
      (updater
       (if (null? f)
           (hash-ref hash key)
           (hash-ref hash key (car f))))))
   ))
(prelude
 get
 (if (get 'hash)
     '((define (make-immutable-hash x) (__make-immutable-hash x))
       (define hash __hash)
       (define (hash->list h)
         (if (hash? h)
             (__hash->list h)
             (error "hash->list: isn't hash" h)))
       (define (hash-set h k v)
         (if (hash? h)
             (__hash-set h k v)
             (error "hash-set: isn't hash" h)))
       (define (hash-ref hash key . f)
         (if (hash-hash-key? hash key)
             (__hash-ref hash key)
             (if (procedure? (car f))
                 ((car f))
                 (car f))))
       (define (hash-hash-key? h k)
         (if (hash? h)
             (__hash-hash-key? h k)
             (error "hash-hash-key?: isn't hash" h))))
     '((define-record-type hash
         (%make-immutable-hash xs)
         hash?
         (xs hash->list))
       (define (make-immutable-hash xs)
         (if (null? xs)
             (%make-immutable-hash '())
             (let ([x (car xs)])
               (hash-set (make-immutable-hash (cdr xs)) (car x) (cdr x)))))
       (define (hash-set hash key v)
         (let ([h (%hash-set hash key v)])
           (if h
               h
               (%make-immutable-hash (cons (cons key v) (hash->list hash))))))
       (define (%hash-set hash key v)
         (%%hash-set (hash->list hash) key v
                     %make-immutable-hash
                     (λ () #f)))
       (define (%%hash-set hash key v s u)
         (if (null? hash)
             (u)
             (let ([x (car hash)])
               (if (equal? (car x) key)
                   (s (cons (cons (car x) v) (cdr hash)))
                   (%%hash-set (cdr hash) key v (λ (r) (s (cons x r))) u)))))
       (define (hash-ref hash key . f)
         (let ([r (assoc key (hash->list hash))])
           (if r
               (cdr r)
               (if (null? f)
                   (error "hash-ref" hash key)
                   (let ([x (car f)])
                     (if (procedure? x)
                         (x)
                         x))))))
       (define (hash-has-key? hash key)
         (if (hash-ref hash key #f) ; 非boolean,所以要if
             #t
             #f))

       (define (%hash xs)
         (if (null? xs)
             '()
             (cons (cons (car xs) (cadr xs)) (%hash (cddr xs)))))
       (define (hash . xs) (make-immutable-hash (%hash xs))))))
