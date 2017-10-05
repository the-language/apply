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
(define-syntax %newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons (cons (quote r) (quote s)) (%newns x ...))]
    [(_ r x ...) (cons (cons (quote r) (quote r)) (%newns x ...))]))
(define-syntax-rule (newns x ...)
  (make-hasheq
   (%newns x ...)))

(define ns (newns
            cons
            [%car car]
            [%cdr cdr]
            [%pair? pair?]
            null?
            +
            -
            *
            /
            number?
            char?
            string?
            string->list
            if
            quote
            symbol?
            eq?
            error
            boolean?
            procedure?
            apply))
;(define (id x) (hash-ref ns x))
(define (id x) (newid x))
(define (newid x)
  (hash-ref! ns x (λ () (gensym x))))

(define (EVAL x)
  (cond
    [(pair? x) (APPLY (car x) (cdr x))]
    [(symbol? x) (id x)]
    [else x]))
(define (APPLY f xs)
  (cond
    [(eq? f 'lambda) (if (null? (cddr xs))
                         (LAMBDA (car xs) (cadr xs))
                         (error "APPLY: lambda" f xs))]
    [(eq? f 'begin) (BEGIN xs)]
    [(eq? f 'define) (error "APPLY: define" f xs)]
    [(eq? f 'void) '(if #f #f)]
    [(eq? f 'quote) (if (null? (cdr xs)) (QUOTE (car xs)) (error "APPLY: quote" f xs))]
    [else (cons (EVAL f) (map EVAL xs))]))
(define (QUOTE x) x)
(define (BEGIN xs)
  (cond
    [(null? xs) (EVAL '(void))]
    [(null? (cdr xs)) (EVAL (car xs))]
    [else
     (cons 'begin
           (map (λ (x)
                  (if (and (pair? x) (eq? (car x) 'define))
                      (if (null? (cdddr x))
                          `(define ,(newid (cadr x)) ,(EVAL (caddr x)))
                          (error "BEGIN: define" xs))
                      (EVAL x))) xs))]))
(define (LAMBDA args x)
  `(lambda ,(%LAMBDA args)
     ,(EVAL x)))
(define (%LAMBDA x)
  (cond
    [(null? x) '()]
    [(symbol? x) (newid x)]
    [(pair? x) (cons (%LAMBDA (car x)) (%LAMBDA (cdr x)))]
    [else (error "%LAMBDA" x)]))

(EVAL
 '(begin
    (define vector (lambda _xs_18 (cons '_vector_ _xs_18)))
    (define %vector? (lambda (_x_18) (if (%pair? _x_18) (eq? (%car _x_18) '_vector_) #f)))
    (define list->vector (lambda (_xs_18) (cons '_vector_ _xs_18)))
    (define vector->list (lambda (_x_18) (if (vector? _x_18) (%cdr _x_18) (error "vector->list: isn't vector?" _x_18))))
    (define %vector-ref
      (lambda (_v_18 _k_19) (if (< _k_19 0) (error "vector-ref: isn't exact-nonnegative-integer?" _v_18 _k_19) (if (%vector? _v_18) (%%vector-ref _v_18 _k_19 (%cdr _v_18) _k_19) (error "vector-ref: isn't vector?" _v_18 _k_19)))))
    (define %%vector-ref
      (lambda (_v_18 _k_19 _xs_20 _x_21)
        (if (null? _xs_20)
            (error "vector-ref: index is out of range" _v_18 _k_19)
            (if (zero? _x_21) (%car _xs_20) (if (< _k_19 0) (error "vector-ref: isn't exact-nonnegative-integer?" _v_18 _k_19) (%vector-ref _v_18 _k_19 (%cdr _xs_20) (- _x_21 1)))))))
    (define pair? (lambda (_x_18) (if (%pair? _x_18) (not (eq? (%car _x_18) '_vector_)) #f)))
    (define %vector-length (lambda (_x_18) (if (%vector? _x_18) (length (%cdr _x_18)) (error "vector-length: isn't vector?" _x_18))))
    (define %vector-length-0? (lambda (_x_18) (if (%vector? _x_18) (null? (%cdr _x_18)) (error "vector-length: isn't vector?" _x_18))))
    (define car (lambda (_p_18) (if (pair? _p_18) (%car _p_18) (error "car: isn't pair?" _p_18))))
    (define cdr (lambda (_p_18) (if (pair? _p_18) (%cdr _p_18) (error "cdr: isn't pair?" _p_18))))
    (define not (lambda (_x_18) (if _x_18 #f #t)))
    (define zero? (lambda (_x_18) (eq? _x_18 0)))
    (define append (lambda (_xs_18 _ys_19) (if (null? _xs_18) _ys_19 (cons (car _xs_18) (append (cdr _xs_18) _ys_19)))))
    (define list (lambda _xs_18 _xs_18))
    (define map (lambda (_f_18 _xs_19) (if (null? _xs_19) '() (cons (_f_18 (car _xs_19)) (map _f_18 (cdr _xs_19))))))
    (define filter (lambda (_f_18 _xs_19) (if (null? _xs_19) '() (if (_f_18 (car _xs_19)) (cons (car _xs_19) (filter _f_18 (cdr _xs_19))) (filter _f_18 (cdr _xs_19))))))
    (define foldl (lambda (_f_18 _x_19 _xs_20) (if (null? _xs_20) _x_19 (foldl _f_18 (_f_18 (car _xs_20) _x_19) (cdr _xs_20)))))
    (define length (lambda (_xs_18) (if (null? _xs_18) 0 (+ 1 (length (cdr _xs_18))))))
    (define equal? (lambda (_x_18 _y_19) (if (pair? _x_18) (if (pair? _y_19) (if (equal? (car _x_18) (car _y_19)) (equal? (cdr _x_18) (cdr _y_19)) #f) #f) (eq? _x_18 _y_19))))
    (define eqv? equal?)
    (define list-ref
      (lambda (_xs_18 _k_19)
        (if (< _k_19 0) (error "list-ref: isn't exact-nonnegative-integer?" _xs_18 _k_19) (if (null? _xs_18) (error "list-ref: index is out of range" _xs_18 _k_19) (if (zero? _k_19) (car _xs_18) (list-ref (cdr _xs_18) _k_19))))))
    (define vector? (lambda (_x_44) (if (%vector? _x_44) ((lambda (_x_45) (if _x_45 _x_45 (not ((lambda (_x_46) (if (pair? _x_46) (eq? (car _x_46) '_struct:_) #f)) (%vector-ref _x_44 0))))) (%vector-length-0? _x_44)) #f)))
    (define vector-length (lambda (_x_44) (if (vector? _x_44) (%vector-length _x_44) (error "vector-length: isn't vector?" _x_44))))
    (define vector-ref (lambda (_v_44 _k_45) (if (vector? _v_44) (%vector-ref _v_44 _k_45) (error "vector-ref: isn't vector?" x))))
    (define kons (lambda (_x_44 _y_45) (vector (cons '_struct:_ '<pare>) _x_44 _y_45)))
    (define pare? (lambda (_x_44) (if (%vector? _x_44) (equal? (%vector-ref _x_44 0) (cons '_struct:_ '<pare>)) #f)))
    (define kar (lambda (_x_44) (if (pare? _x_44) (%vector-ref _x_44 1) (error "type error" 'x _x_44))))
    (define kdr (lambda (_x_44) (if (pare? _x_44) (%vector-ref _x_44 (+ 1 1)) (error "type error" 'y _x_44))))))
