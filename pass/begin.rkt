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
(require "../pass.rkt")
(require "../common.rkt")
(define (EVAL x)
  (match x
    [`(,(or 'λ 'lambda) ,a ,b) `(lambda ,a ,(EVAL b))]
    [`(begin ,@b) (mk-begin (map EVAL (BEGIN b)))]
    [(list _ ...) (map EVAL x)]
    [_ x]))
(define (GCfind x)
  (match x
    [`(lambda ,_ ,@v) (GCfind v)]
    [(? symbol?) (set x)]
    [`(define ,_ ,v) (GCfind v)]
    [`(quote ,_) (set)]
    [(? pair?) (set-union (GCfind (car x)) (GCfind (cdr x)))]
    [_ (set)]))
(define ((find? s) x)
  (set-member? s (car x)))
(define (BEGIN xs)
  (let-values ([(defs notdefs) (partition define? xs)])
    (let ([ndefs (map (match-lambda [`(define ,(and s (? symbol?)) ,x) (cons s x)])
                      defs)]
          [nf (GCfind notdefs)])
      (let-values ([(fdefs defs) (partition (find? nf) ndefs)])
        (let loop ([marked '()] [fdefs fdefs] [defs defs])
          (if (null? fdefs)
              (filter
               (match-lambda
                 [`(define ,s ,_) (set-member? marked s)]
                 [_ #t])
               xs)
              (let-values ([(fdefs1 defs1) (partition (find? (GCfind (map cdr fdefs))) defs)])
                (loop (append (map car fdefs) marked)
                      fdefs1
                      defs1))))))))
(defpass (λ (conf x) (EVAL x)))
