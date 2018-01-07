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

(require compatibility/defmacro)
(require srfi/9) ; R7RS
(require racket/sandbox)
(define EVAL (make-evaluator 'racket))
(define null-set (set))
(define null-hash (hash))
;---------------------------
(define ($var/k state x k) (k state `(!var ,x)))
(define ($num/k state x k) (k state `(!num ,x)))
(define ($char/k state x k) (k state `(!char ,x)))
(define ($str/k state x k) (k state `(!str ,x)))
(define ($$val/k state x k) (k state (list x)))
(define ($$define/k state f x k) (k state `((!def ,f ,x))))
(define $void `!void)
(define ($$if/k state b xs x ys y k) (k state '() `(!if ,b (begin ,@xs ,x) (begin ,@ys y))))
(define ($$apply/k state f args k) (k state '() `(!app ,f ,@args)))
;-----------------------------
(define-record-type macro
  (macro src proc)
  macro?
  (src macro-src)
  (proc macro-proc))
(define-record-type just
  (just x)
  just?
  (x run-just))
(define (macroexpand local-state env state x k) ; (k env state x)
  (if (pair? x)
      (let ([f (car x)] [args (cdr x)])
        (let ([m (hash-ref env f #f)])
          (if (and m (macro? m))
              ((macro-proc m)
               local-state env state args
               (λ () (k env state x))
               (λ (env state x)
                 (macroexpand local-state env state x k)))
              (k env state x))))
      (k env state x)))
(define null-env
  (hash
   'DEFMACROz (macro
               '(λ (local-state env state args default k)
                  (k (hash-set env (first args) (macro (second args) (EVAL (second args)))) state 'VOIDz))
               (λ (local-state env state args default k)
                 (k (hash-set env (first args) (macro (second args) (EVAL (second args)))) state 'VOIDz)))
   'define (macro
            '(λ (local-state env state args default k)
               (let ([f (car args)] [r (cdr args)])
                 (if (pair? f)
                     (k env state `(define ,(car f) (λ ,(cdr f) ,@r)))
                     (let ([v (car r)])
                       (if (symbol? v)
                           ($var/k
                            state v
                            (λ (state r)
                              (k (hash-set env f r) state 'VOIDz)))
                           (default))))))
            (λ (local-state env state args default k)
              (let ([f (car args)] [r (cdr args)])
                (if (pair? f)
                    (k env state `(define ,(car f) (λ ,(cdr f) ,@r)))
                    (let ([v (car r)])
                      (if (symbol? v)
                          ($var/k
                           state v
                           (λ (state r)
                             (k (hash-set env f r) state 'VOIDz)))
                          (default)))))))))
(define (COMPILE/k local-state env state x k) ; (k state xs x)
  (macroexpand
   local-state env state x
   (λ (env state x)
     (cond
       [(pair? x)
        (let ([f (car x)] [args (cdr x)])
          (cond
            [(eq? f 'quote) (QUOTE/k local-state env state (first args) k)]
            [(eq? f 'define)
             ($$define/k
              state (first args) (second args)
              (λ (state xs)
                (k state xs $void)))]
            [(eq? f 'begin) (BEGIN/k local-state env state args k)]
            [(eq? f 'if)
             (COMPILE/k
              local-state env state (first args)
              (λ (state bs b)
                (COMPILE/k
                 local-state env state (second args)
                 (λ (state xs x)
                   (COMPILE/k
                    local-state env state (third args)
                    (λ (state ys y)
                      ($$if/k
                       state b xs x ys y
                       (λ (state rs r)
                         (k state (append bs rs) r)))))))))]
            [else
             (COMPILE/k
              local-state env state f
              (λ (state fs f)
                (COMPILE/k*
                 local-state env state args
                 (λ (state argss args)
                   ($$apply/k
                    state f args
                    (λ (state rs r)
                      (k state (append fs argss rs) r)))))))]))]
       [(eq? x 'VOIDz) (k state '() $void)]
       [else
        ((cond
           [(symbol? x) $var/k]
           [(number? x) $num/k]
           [(char? x) $char/k]
           [(string? x) $str/k]
           [else (error 'compile "invalid syntax" x)])
         state x
         (λ (state x)
           (k state '() x)))]))))
(define (QUOTE/k) (cond))
(define (BEGIN/k local-state env state xs k) ; (k state xs x)
  (if (null? (cdr xs))
      (COMPILE/k local-state env state (car xs) k)
      (COMPILE/k
       local-state env state (car xs)
       (λ (state as a)
         (BEGIN/k
          local-state env state (cdr xs)
          (λ (state ds d)
            (if (eq? $void a)
                (k state (append as ds) d)
                ($$val/k
                 state a
                 (λ (state xs)
                   (k state (append as xs ds) d))))))))))
(define (COMPILE/k* local-state env state xs k) ; (k state xs rs)
  (if (null? (cdr xs))
      (COMPILE/k
       local-state env state (car xs)
       (λ (state xs x)
         (k state xs (list x))))
      (COMPILE/k
       local-state env state (car xs)
       (λ (state as a)
         (COMPILE/k*
          local-state env state (cdr xs)
          (λ (state xs rs)
            (k state (append as xs) (cons a rs))))))))