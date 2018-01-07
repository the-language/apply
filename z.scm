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

(define-record-type macro
  (macro src proc)
  macro?
  (src macro-src)
  (proc macro-proc))
(define-record-type just
  (just x)
  just?
  (x run-just))
(define (macroexpand local-state env state x k) ; (k local-state env state x)
  (if (pair? x)
      (let ([f (car x)] [args (cdr x)])
        (let ([m (hash-ref env f #f)])
          (if (and m (macro? m))
              ((macro-proc m)
               local-state env state args
               (λ () (k local-state env state x))
               (λ (local-state env state x)
                 (macroexpand local-state env state x k)))
              (k local-state env state x))))
      (k local-state env state x)))
(define null-env
  (hash
   'DEFMACROz (macro
               '(λ (local-state env state args default k)
                  (k local-state (hash-set env (first args) (macro (second args) (EVAL (second args)))) state 'VOIDz))
               (λ (local-state env state args default k)
                 (k local-state (hash-set env (first args) (macro (second args) (EVAL (second args)))) state 'VOIDz)))
   'define (macro
            '(λ (local-state env state args default k)
               (let ([f (car args)] [r (cdr args)])
                 (if (pair? f)
                     (k local-state env state `(define ,(car f) (λ ,(cdr f) ,@r)))
                     (let ([v (car r)])
                       (if (symbol? v)
                           ($var/k
                            local-state state v
                            (λ (local-state state r)
                              (k local-state (hash-set env f r) state 'VOIDz)))
                           (default))))))
            (λ (local-state env state args default k)
              (let ([f (car args)] [r (cdr args)])
                (if (pair? f)
                    (k local-state env state `(define ,(car f) (λ ,(cdr f) ,@r)))
                    (let ([v (car r)])
                      (if (symbol? v)
                          ($var/k
                           local-state state v
                           (λ (local-state state r)
                             (k local-state (hash-set env f r) state 'VOIDz)))
                          (default)))))))))
(define (COMPILE/k local-state env state x k) ; (k local-state env state xs x)
  (macroexpand
   local-state env state x
   (λ (local-state env state x)
     (cond
       [(pair? x)
        (let ([f (car x)] [args (cdr x)])
          (cond
            [(eq? f 'QUOTEz) ; (QUOTEz <symbol>)
             ($sym/k
              local-state state (first args)
              (λ (local-state env state x)
                (k local-state env state '() x)))]
            [(eq? f 'define)
             (let ([n (first args)] [v (second args)])
               (if (or (eq? (car v) 'lambda) (eq? (car v) 'λ))
                   (let ([parm (cadr v)] [body (cddr v)])
                     ($!pre-define-lambda
                      local-state state parm
                      (λ (local-state1 state)
                        (BEGINtail/k
                         local-state1 env state body
                         (λ (local-state1 env state xs)
                           ($$define-lambda/k
                            local-state local-state1 state n parm xs
                            (λ (local-state state xs)
                              (k local-state env state xs $void))))))))
                   ($$define/k
                    local-state state (first args) (second args)
                    (λ (local-state state xs)
                      (k local-state env state xs $void)))))]
            [(eq? f 'begin) (BEGIN/k local-state env state args k)]
            [(or (eq? f 'lambda) (eq? f 'λ))
             (let ([parm (car args)] [body (cdr args)])
               ($!pre-lambda
                local-state state parm
                (λ (local-state1 state)
                  (BEGINtail/k
                   local-state1 env state body
                   (λ (local-state1 env state xs)
                     ($$lambda/k
                      local-state local-state1 state parm xs k))))))]
            [(eq? f 'if)
             (COMPILE/k
              local-state env state (first args)
              (λ (local-state env state bs b)
                (COMPILE/k
                 local-state env state (second args)
                 (λ (local-state env state xs x)
                   (COMPILE/k
                    local-state env state (third args)
                    (λ (local-state env state ys y)
                      ($$if/k
                       local-state state b xs x ys y
                       (λ (local-state state rs r)
                         (k local-state env state (append bs rs) r)))))))))]
            [else
             (COMPILE/k
              local-state env state f
              (λ (local-state env state fs f)
                (COMPILE/k*
                 local-state env state args
                 (λ (local-state env state argss args)
                   ($$apply/k
                    local-state state f args
                    (λ (local-state state rs r)
                      (k local-state env state (append fs argss rs) r)))))))]))]
       [(eq? x 'VOIDz) (k local-state env state '() $void)]
       [(eq? x 'ARCHz) (k local-state env state '() $arch)]
       [else
        ((cond
           [(symbol? x) $var/k]
           [(number? x) $num/k]
           [(char? x) $char/k]
           [(string? x) $str/k]
           [(null? x) $null]
           [else (error 'compile "invalid syntax" x)])
         local-state state x
         (λ (local-state state x)
           (k local-state env state '() x)))]))))
(define ($$val/k+ local-state state x k)
  (if (eq? x $void)
      (k local-state state '())
      ($$val/k local-state state x k)))
(define (BEGIN/k local-state env state xs k) ; (k local-state env state xs x)
  (if (null? (cdr xs))
      (COMPILE/k local-state env state (car xs) k)
      (COMPILE/k
       local-state env state (car xs)
       (λ (local-state env state as a)
         (BEGIN/k
          local-state env state (cdr xs)
          (λ (local-state env state ds d)
            ($$val/k+
             local-state state a
             (λ (local-state state xs)
               (k local-state env state (append as xs ds) d)))))))))
(define (COMPILE/k* local-state env state xs k) ; (k local-state env state xs rs)
  (if (null? xs)
      (k local-state env state '() '())
      (COMPILE/k
       local-state env state (car xs)
       (λ (local-state env state as a)
         (COMPILE/k*
          local-state env state (cdr xs)
          (λ (local-state env state xs rs)
            (k local-state env state (append as xs) (cons a rs))))))))

(define (COMPILEtail/k local-state env state x k) ; (k local-state env state xs)
  (macroexpand
   local-state env state x
   (λ (local-state env state x)
     (define (X)
       (COMPILE/k
        local-state env state x
        (λ (local-state env state xs x)
          ($$tail-val/k
           local-state state x
           (λ (local-state state es)
             (k local-state env state (append xs es)))))))
     (define (E) (error 'compile "invalid syntax" x))
     (cond
       [(pair? x)
        (let ([f (car x)] [args (cdr x)])
          (cond
            [(or (eq? f 'QUOTEz) (eq? f 'lambda) (eq? f 'λ)) (X)]
            [(eq? f 'define) (E)]
            [(eq? f 'begin) (BEGINtail/k local-state env state args k)]
            [(eq? f 'if)
             (COMPILE/k
              local-state env state (first args)
              (λ (local-state env state bs b)
                (COMPILEtail/k
                 local-state env state (second args)
                 (λ (local-state env state xs)
                   (COMPILEtail/k
                    local-state env state (third args)
                    (λ (local-state env state ys)
                      ($$if-tail/k
                       local-state state b xs ys
                       (λ (local-state state rs)
                         (k local-state env state (append bs rs))))))))))]
            [else
             (COMPILE/k
              local-state env state f
              (λ (local-state env state fs f)
                (COMPILE/k*
                 local-state env state args
                 (λ (local-state env state argss args)
                   ($$tail-apply/k
                    local-state state f args
                    (λ (local-state state rs)
                      (k local-state env state (append fs argss rs))))))))]))]
       [else (X)]))))
(define (BEGINtail/k local-state env state xs k) ; (k local-state env state xs)
  (if (null? (cdr xs))
      (COMPILEtail/k local-state env state (car xs) k)
      (COMPILE/k
       local-state env state (car xs)
       (λ (local-state env state as a)
         (BEGINtail/k
          local-state env state (cdr xs)
          (λ (local-state env state ds)
            ($$val/k+
             local-state state a
             (λ (local-state state xs)
               (k local-state env state (append as xs ds))))))))))
(define (z dir xs)
  (BEGIN/k
   (hash-set $null-local-state 'dir dir) $null-env $null-state (append xs '(VOIDz))
   (λ (local-state env state xs x)
     ($$top local-state state xs))))
(define prelude (INCLUDE-LISTz "prelude.scm"))
(define (z+prelude dir xs)
  (z dir (append prelude xs)))
