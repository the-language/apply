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

(define (hash-append h xs)
  (foldl (λ (p h) (hash-set h (car p) (cdr p))) h xs))
(define (set-append s xs)
  (if (null? xs)
      s
      (set-append (set-add s (car xs)) (cdr xs))))

(define-record-type module
  (module export-macros export-values)
  module?
  (export-macros module-export-macros)
  (export-values module-export-values))
(define (MODULE/k state name modules macros defines dir exports body k) ; (k vars state defines modules xs)
  (COMPILE-TOP/k
   state modules macros defines dir body
   (λ (state modules macros defines xs)
     (partition/k
      (λ (export) (hash-has-key? macros (second export))) exports
      (λ (exportmacros exports)
        (let ([export-macros
               (map
                (λ (e)
                  (cons (first e) (hash-ref macros (second e)))) exportmacros)]
              [export-values (map first exports)])
          (MODULEdo state modules macros dir name export-macros export-values exports xs defines k)))))))
(define (MODULEmk1/k name m c export-values defines k)
  (if (null? export-values)
      (k defines '())
      (MODULEmk1/k name m (+ 1 c) (cdr export-values) defines
                   (λ (defines xs)
                     (let ([n (MODULEvalue-name name (car export-values))])
                       (k (set-add defines n)
                          (cons ($$define n ($list-ref ($$var m) ($$var c)))
                                xs)))))))
(define (MODULEvalue-name m v)
  (string->symbol
   (string-append
    (foldr string-append ""
           (map (λ (s) (string-append (symbol->string s) "@")) (append m (list v))))
    "Mz")))
(define (MODULEname m)
  (string->symbol
   (string-append
    (foldr string-append ""
           (map (λ (s) (string-append (symbol->string s) "@")) m))
    "_Mz")))
(define (MODULEdo state modules macros dir name export-macros export-values exports xs defines k)
  (let ([n (MODULEname name)])
    (MODULEmk1/k
     name n 0 export-values defines
     (λ (defines cs1)
       (LAMBDA/k state modules macros dir '() (append xs `((LISTz ,@(map second exports))))
                 (λ (vars state modules lam)
                   (k vars state defines (hash-set modules name (module export-macros export-values))
                      (cons
                       ($$define n ($$apply lam '())) cs1))))))))

(define null-state null-hash)
(define (COMPILE-TOP/k state modules macros defines dir xs k)
  (BEGIN
   null-set state modules macros defines dir #f (append xs (list 'VOIDz))
   (λ (vars state modules macros defines xs v)
     (k state modules macros defines xs))))
(define (COMPILE/k vars state modules macros defines dir exp? x k) ; (k vars state modules macros defines xs v)
  (cond
    [(pair? x)
     (let ([f (car x)] [args (cdr x)])
       (cond
         [(hash-ref macros f #f) => (λ (m) (COMPILE/k vars state modules macros defines dir exp? (apply m args) k))]
         [(eq? f 'DEFMACROz) (k vars state modules (hash-set macros (first args) (EVAL (second args))) defines '() $void)]
         [(eq? f 'HOSTz)
          (HOST args
                (λ (x)
                  (k vars state modules macros defines '() ($host-exp x)))
                (λ (x)
                  (COMPILE/k vars state modules macros defines dir exp? x k)))]
         [(eq? f 'HOSTCASEz)
          (HOST args
                (λ (x)
                  (COMPILE/k vars state modules macros defines dir exp? x k))
                (λ (x)
                  (COMPILE/k vars state modules macros defines dir exp? x k)))]
         [(eq? f 'define)
          (DEF/k
           (car args) (cdr args)
           (λ (f v)
             (COMPILE/k
              vars state modules macros defines dir exp? v
              (λ (vars state modules macros defines xs v)
                (k vars state modules macros (set-add defines f) (append xs (list ($$define f v))) $void)))))]
         [(eq? f 'begin)
          (if exp?
              (COMPILE/k vars state modules macros defines dir exp? `((lambda () ,@args)) k)
              (BEGIN vars state modules macros defines dir exp? args k))]
         [(eq? f 'IMPALLz)
          (let ([name (first args)])
            (IMPALL/k state macros defines name (hash-ref modules name)
                      (λ (state modules macros defines xs)
                        (k vars state modules macros defines xs 'VOIDz))))]
         [(or (eq? f 'lambda) (eq? f 'λ))
          (LAMBDA/k state modules macros dir (car args) (cdr args)
                    (λ (vars1 state modules lam)
                      (k (set-union vars1 vars) state modules macros defines '() lam)))]
         [(eq? f 'LISTz)
          (COMPILE/k*
           vars state modules macros defines dir exp? args
           (λ (vars state modules macros defines ys args)
             (k vars state modules macros defines ys ($list args))))]
         [(eq? f 'MODULEz)
          (let ([name (car args)] [exports+body (cdr args)])
            (MODULE/k
             state name modules macros defines dir (car exports+body) (cdr exports+body)
             (λ (vars1 state defines modules cs)
               (k (set-union vars1 vars) state modules macros defines cs $void))))]
         [(eq? f 'RECORDz)
          (k vars state modules macros defines (list ($$record (car args) (cadr args) (cddr args))) $void)]
         [(eq? f 'if)
          (COMPILE/k
           vars state modules macros defines dir exp? (first args)
           (λ (vars state modules macros defines cs0 b)
             (COMPILE/k
              vars state modules macros defines dir #t (second args)
              (λ (vars state modules macros defines cs1 x)
                (COMPILE/k
                 vars state modules macros defines dir #t (third args)
                 (λ (vars state modules macros defines cs2 y)
                   (k vars state modules macros defines (append cs0 cs1 cs2) ($if b x y))))))))]
         [else
          (COMPILE/k
           vars state modules macros defines dir exp? f
           (λ (vars state modules macros defines xs f)
             (COMPILE/k*
              vars state modules macros defines dir exp? args
              (λ (vars state modules macros defines ys args)
                (k vars state modules macros defines (append xs ys) ($$apply f args))))))]))]
    [(and (symbol? x) (not (eq? x 'VOIDz))) (k (set-add vars x) state modules macros defines '() ($$var x))]
    [else (k vars state modules macros defines '()
             (cond
               [(eq? x 'VOIDz) $void]
               [(number? x) ($$number x)]
               [(char? x) ($$char x)]
               [(string? x) ($$string x)]
               [(null? x) $null]
               [(eq? x #t) $true]
               [(eq? x #f) $false]
               [else (error 'compile "invalid syntax" x)]))]))
(define (COMPILE/tail vars state modules macros defines dir x k) ; (k vars state modules macros defines xs)
  (cond
    [(pair? x)
     (let ([f (car x)] [args (cdr x)])
       (cond
         [(hash-ref macros f #f) => (λ (m) (COMPILE/tail vars state modules macros defines dir (apply m args) k))]
         [(eq? f 'DEFMACROz) (error 'compile "invalid syntax" x)]
         [(eq? f 'HOSTz)
          (HOST args
                (λ (x)
                  (k vars state modules macros defines (list ($$tail-val ($host-exp x)))))
                (λ (x)
                  (COMPILE/tail vars state modules macros defines dir x k)))]
         [(eq? f 'HOSTCASEz)
          (HOST args
                (λ (x)
                  (COMPILE/tail vars state modules macros defines dir x k))
                (λ (x)
                  (COMPILE/tail vars state modules macros defines dir x k)))]
         [(eq? f 'define) (error 'compile "invalid syntax" x)]
         [(eq? f 'begin) (BEGIN/tail vars state modules macros defines dir args k)]
         [(eq? f 'IMPALLz) (error 'compile "invalid syntax" x)]
         [(or (eq? f 'lambda) (eq? f 'λ))
          (LAMBDA/k state modules macros dir (car args) (cdr args)
                    (λ (vars1 state modules lam)
                      (k (set-union vars1 vars) state modules macros defines ($$tail-val lam))))]
         [(eq? f 'LISTz)
          (COMPILE/k*
           vars state modules macros defines dir #f args
           (λ (vars state modules macros defines ys args)
             (k vars state modules macros defines ($$tail-val ($list args)))))]
         [(eq? f 'MODULEz)
          (let ([name (car args)] [exports+body (cdr args)])
            (MODULE/k
             state name modules macros defines dir (car exports+body) (cdr exports+body)
             (λ (state defines modules cs)
               (k vars state modules macros defines (append cs ($$tail-val $void))))))]
         [(eq? f 'RECORDz) (error 'compile "invalid syntax" x)]
         [(eq? f 'if)
          (COMPILE/k
           vars state modules macros defines dir #f (first args)
           (λ (vars state modules macros defines cs0 b)
             (COMPILE/tail
              vars state modules macros defines dir (second args)
              (λ (vars state modules macros defines xs)
                (COMPILE/tail
                 vars state modules macros defines dir (third args)
                 (λ (vars state modules macros defines ys)
                   (k vars state modules macros defines (append cs0 ($$tail-if b xs ys)))))))))]
         [else
          (COMPILE/k
           vars state modules macros defines dir #f f
           (λ (vars state modules macros defines xs f)
             (COMPILE/k*
              vars state modules macros defines dir #f args
              (λ (vars state modules macros defines ys args)
                (k vars state modules macros defines (append xs ys ($$tail-apply f args)))))))]))]
    [(and (symbol? x) (not (eq? x 'VOIDz))) (k (set-add vars x) state modules macros defines ($$tail-val ($$var x)))]
    [else (k vars state modules macros defines
             ($$tail-val
              (cond
                [(eq? x 'VOIDz) $void]
                [(number? x) ($$number x)]
                [(char? x) ($$char x)]
                [(string? x) ($$string x)]
                [(null? x) $null]
                [(eq? x #t) $true]
                [(eq? x #f) $false]
                [else (error 'compile "invalid syntax" x)])))]))
(define (IMPALL/k state macros defines name module k)
  (let ([module-macros (module-export-macros module)]
        [values (module-export-values module)])
    (k state macros (map/symbol-append macros module-macros) (set-append defines values)
       (map
        (λ (x)
          ($$define x ($$var (MODULEvalue-name name x))))
        values))))
(define (DEF/k a d k)
  (if (symbol? a)
      (k a (car d))
      (DEF/k (car a) (list (cons 'λ (cons (cdr a) d))) k)))
(define (BEGIN vars state modules macros defines dir exp? xs k)
  (cond
    [(null? (cdr xs)) (COMPILE/k vars state modules macros defines dir exp? (car xs) k)]
    [(pair? (car xs))
     (COMPILE/k
      vars state modules macros defines dir exp? (car xs)
      (λ (vars state modules macros defines cs1 v)
        (BEGIN
         vars state modules macros defines dir exp? (cdr xs)
         (λ (vars state modules macros defines cs2 r)
           (if (eq? v $void)
               (k vars state modules macros defines (append cs1 cs2) r)
               (k vars state modules macros defines (append cs1 (cons v cs2)) r))))))]
    [else (BEGIN vars state modules macros defines dir exp? (cdr xs) k)]))
(define (BEGIN/tail vars state modules macros defines dir xs k)
  (cond
    [(null? (cdr xs)) (COMPILE/tail vars state modules macros defines dir (car xs) k)]
    [(pair? (car xs))
     (COMPILE/k
      vars state modules macros defines dir #f (car xs)
      (λ (vars state modules macros defines cs1 v)
        (BEGIN/tail
         vars state modules macros defines dir (cdr xs)
         (λ (vars state modules macros defines cs2)
           (if (eq? v $void)
               (k vars state modules macros defines (append cs1 cs2))
               (k vars state modules macros defines (append cs1 (cons v cs2))))))))]
    [else (BEGIN/tail vars state modules macros defines dir (cdr xs) k)]))
(define (COMPILE/k* vars state modules macros defines dir exp? xs k)
  (if (null? xs)
      (k vars state modules macros defines '() '())
      (COMPILE/k
       vars state modules macros defines dir exp? (car xs)
       (λ (vars state modules macros defines cs1 a)
         (COMPILE/k*
          vars state modules macros defines dir exp? (cdr xs)
          (λ (vars state modules macros defines cs2 d)
            (k vars state modules macros defines (append cs1 cs2) (cons a d))))))))
(define (args->set args)
  (cond
    [(symbol? args) (set args)]
    [(null? args) null-set]
    [else (set-add (args->set (cdr args)) (car args))]))
(define (LAMBDA/k state modules macros dir args body k) ; (k vars state modules lambda)
  (BEGIN/tail
   null-set state modules macros null-set dir body
   (λ (vars state modules macros defines1 cs)
     (let ([vars (set-subtract vars (args->set args) defines1)])
       (k vars state modules ($$lambda (set->list vars) (set->list defines1) args cs))))))
(define (HOST xs k1 k2)
  (let ([x (car xs)] [xs (cdr xs)])
    (let ([arch (first x)] [code (second x)])
      (cond
        [(or (eq? arch $host) (and (pair? arch) (member $host arch))) (k1 code)]
        [(eq? (first x) '_) (k2 code)]
        [else (HOST xs k1 k2)]))))

(define prelude (file->list "prelude.scm"))
(define preludeC
  (COMPILE-TOP/k null-hash null-hash null-hash null-set (current-directory) prelude
                 (λ (state modules macros defines xs)
                   (list state modules macros defines xs))))
(define prelude-state (apply (λ (state modules macros defines xs) state) preludeC))
(define prelude-modules (apply (λ (state modules macros defines xs) modules) preludeC))
(define prelude-macros (apply (λ (state modules macros defines xs) macros) preludeC))
(define prelude-defines (apply (λ (state modules macros defines xs) defines) preludeC))
(define prelude-xs (apply (λ (state modules macros defines xs) xs) preludeC))
(define (z dir xs)
  (COMPILE-TOP/k prelude-state prelude-modules prelude-macros prelude-defines dir xs
                 (λ (state modules macros defines xs)
                   ($$top (set->list defines) (append prelude-xs xs)))))
(define (z-current xs) (z (current-directory) xs))
;-----------------------------------rewrite
(define (hash-append h xs)
  (foldl (λ (p h) (hash-set h (car p) (cdr p))) h xs))
(define-record-type module
  (module exports vars defines define-la xs)
  module?
  _!_
  )
(define-record-type macro
  (macro src proc)
  macro?
  (src macro-src)
  (proc macro-proc))
(define-record-type just
  (just x)
  just?
  (x run-just))
; (MODULEz <name> ([<export-name> <id>] ...) <body> ...)
(define (MODULE/k dir env vars state modules xs k)
  (let ([name (car xs)] [exports (cadr xs)] [body (cddr xs)])
    (let ([exports (map
     (λ (e)
       (cons (first e) (hash-ref env (second e)))) exports)])
      _!_)))
(define-record-type define-lambda
  (define-lambda vars defines define-lambdas xs)
  define-lambda?
  (vars define-lambda-vars)
  (defines define-lambda-defines)
  (define-lambdas define-lambda-define-lambdas)
  (xs define-lambda-xs))
(define (macroexpand dir env state modules x)
  (if (pair? x)
      (let ([f (car x)] [args (cdr x)])
        (let ([m (hash-ref env f #f)])
          (if (macro? m)
              (macroexpand dir env state modules (apply (macro-proc m) args))
              x)))
      x))
(define (DEF/k a d k)
  (if (symbol? a)
      (k a (car d))
      (DEF/k (car a) (list (cons 'λ (cons (cdr a) d))) k)))
(define (TOP/k compile-define-name compile-define dir env vars state modules xs k) ; (k vars state modules env defines define-lambdas xs)
  (preTOP/k
   dir env state modules xs
   (λ (env state modules defines xs)
     (postTOP/k
      compile-define
      dir
      (hash-append env (map (λ (d) (cons d (compile-define-name d))) defines))
      vars
      state
      modules
      xs
      (λ (vars state modules env define-lambdas xs)
        (k vars state modules env defines define-lambdas xs))))))
(define (preTOP/k dir env state modules xs k) ; (k env state modules defines xs)
  (if (null? xs)
      (k env state modules defines '())
      (let ([x (macroexpand dir env state modules (car xs))] [xs (cdr xs)])
        (if (pair? x)
            (let ([f (car x)] [args (cdr x)])
              (cond
                [(eq? f 'begin)
                 (preTOP/k dir env state modules (append args xs) k)]
                [(eq? f 'DEFMACROz)
                 (preTOP/k dir (hash-set env (first args) (EVAL (second args))) state modules xs k)]
                [(eq? f 'define)
                 (DEF/k
                  (car args) (cdr args)
                  (λ (s x)
                    (preTOP/k
                     dir env state modules xs
                     (λ (env state modules defines rs)
                       (k env state modules (cons s defines) (cons `(define ,s ,x) rs))))))]
                [(eq? f 'MODULEz) _!_]
                [(eq? f 'RECORDz) _!_]
                [(eq? f 'IMPALLz) _!_]
                [else
                 (preTOP/k
                  dir env state modules xs
                  (λ (env state modules defines rs)
                    (k env state modules defines (cons x rs))))]))
            (preTOP/k
             dir env state modules xs
             (λ (env state modules defines rs)
               (k env state modules defines (cons x rs))))))))
(define (postTOP/k compile-define dir env vars state modules xs k) ; (k vars state modules env define-lambdas xs)
  (if (null? (cdr xs))
      (COMPILE-tail/k
       dir env vars state modules (car xs)
       (λ (vars state modules xs)
         (k vars state modules env '() xs)))
      (let ([x (car xs)] [xs (cdr xs)])
        (if (and (pair? x) (eq? (first x) 'define))
            (let ([i (second x)] [v (third x)])
              (if (and (pair? v) (or (eq? (car v) 'lambda) (eq? (car v) 'λ)))
                  _!_
                  (COMPILE/k
                   dir env vars state modules v
                   (λ (vars state modules vs v)
                     (postTOP/k
                      compile-define dir env vars state modules xs
                      (λ (vars state modules env define-lambdas xs)
                        (k vars state modules env define-lambdas (append (compile-define i v) xs))))))))
            (COMPILE/k
             dir env vars state modules x
             (λ (vars state modules vs v)
               (postTOP/k
                compile-define dir env vars state modules xs
                (λ (vars state modules env define-lambdas xs)
                  (k vars state modules env define-lambdas (append vs ($$val v) xs))))))))))
(define (COMPILE/k dir env vars state modules x k) ; (k vars state modules xs x)
  (let ([x (macroexpand dir env state modules x)])
    (cond
      [(pair? x)
       (let ([f (car x)] [args (cdr x)])
         (cond
           [(or (eq? f 'DEFMACROz) (eq? f 'define) (eq? f 'IMPALLz) (eq? f 'MODULEz) (eq? f 'RECORDz))
            (error 'compile "invalid syntax" x)]
           [(eq? f 'begin)
            (BEGIN/k dir env vars state modules args k)]
           [(eq? f 'HOSTz)
            (HOST args
                  (λ (x)
                    (k vars state modules '() ($host-exp x)))
                  (λ (x)
                    (COMPILE/k dir env vars state modules x k)))]
           [(eq? f 'HOSTCASEz)
            (HOST args
                  (λ (x)
                    (COMPILE/k dir env vars state modules x k))
                  (λ (x)
                    (COMPILE/k dir env vars state modules x k)))]
           [(eq? f 'if)
            (COMPILE/k
             dir env vars state modules (first args)
             (λ (vars state modules bs b)
               (COMPILE/k
                dir env vars state modules (second args)
                (λ (vars state modules xs x)
                  (COMPILE/k
                   dir env vars state modules (third args)
                   (λ (vars state modules ys y)
                     ($if/k b xs x ys y
                            (λ (rs r)
                              (k vars state modules (append bs rs) r)))))))))]
           [(or (eq? f 'lambda) (eq? f 'λ)) _!_]
           [(eq? f 'quote) _!_]
           [else
            (COMPILE/k
             dir env vars state modules f
             (λ (vars state modules fs f)
               (COMPILE/k*
                dir env vars state modules args
                (λ (vars state modules argss args)
                  ($apply/k f args
                            (λ (rs r)
                              (k vars state modules (append fs argss rs) r)))))))]))
       [(symbol? x) (k (set-add vars x) state modules '() (run-just (hash-ref env x)))]
       [else
        (k vars state modules '()
           (cond
             [(number? x) ($number x)]
             [(string? x) ($string x)]
             [(char? x) ($char x)]
             [(null? x) $null]
             [else (error 'compile "invalid syntax" x)]))]])))
(define (BEGIN/k dir env vars state modules xs k)
  (if (null? (cdr xs))
      (COMPILE/k dir env vars state modules (car xs) k)
      (let ([x (car xs)] [xs (cdr xs)])
        (COMPILE/k
         dir env vars state modules x
         (λ (vars state modules as a)
           (BEGIN/k
            dir env vars state modules xs
            (λ (vars state modules ds d)
              (k vars state modules (append as ($$val a) ds) d))))))))
(define (COMPILE-tail/k dir env vars state modules x k) ; (k vars state modules xs)
  (let ([x (macroexpand dir env state modules x)])
    (define (X) (COMPILE/k dir env vars state modules x
                           (λ (vars state modules xs x)
                             (k vars state modules (append xs ($$tail-val x))))))
    (cond
      [(pair? x)
       (let ([f (car x)] [args (cdr x)])
         (cond
           [(or (eq? f 'DEFMACROz) (eq? f 'define) (eq? f 'IMPALLz) (eq? f 'MODULEz) (eq? f 'RECORDz))
            (error 'compile "invalid syntax" x)]
           [(eq? f 'begin)
            (BEGIN-tail/k dir env vars state modules args k)]
           [(eq? f 'HOSTz)
            (HOST args
                  (λ (x)
                    (k vars state modules ($$tail-val ($host-exp x))))
                  (λ (x)
                    (COMPILE-tail/k dir env vars state modules x k)))]
           [(eq? f 'HOSTCASEz)
            (HOST args
                  (λ (x)
                    (COMPILE-tail/k dir env vars state modules x k))
                  (λ (x)
                    (COMPILE-tail/k dir env vars state modules x k)))]
           [(eq? f 'if)
            (COMPILE/k
             dir env vars state modules (first args)
             (λ (vars state modules bs b)
               (COMPILE-tail/k
                dir env vars state modules (second args)
                (λ (vars state modules xs)
                  (COMPILE-tail/k
                   dir env vars state modules (third args)
                   (λ (vars state modules ys)
                     (k vars state modules (append bs ($$tail-if b xs ys)))))))))]
           [(or (eq? f 'lambda) (eq? f 'λ)) (X)]
           [(eq? f 'quote) (X)]
           [else
            (COMPILE/k
             dir env vars state modules f
             (λ (vars state modules fs f)
               (COMPILE/k*
                dir env vars state modules args
                (λ (vars state modules argss args)
                  (k vars state modules (append fs argss ($$apply-tail f args)))))))]))]
      [else (X)])))
(define (BEGIN-tail/k dir env vars state modules xs k)
  (if (null? (cdr xs))
      (COMPILE-tail/k dir env vars state modules (car xs) k)
      (let ([x (car xs)] [xs (cdr xs)])
        (COMPILE-tail/k
         dir env vars state modules x
         (λ (vars state modules as)
           (BEGIN-tail/k
            dir env vars state modules xs
            (λ (vars state modules ds)
              (k vars state modules (append as ds)))))))))
(define (COMPILE/k* dir env vars state modules xs k)
  (if (null? (cdr xs))
      (COMPILE/k dir env vars state modules (car xs) k)
      (COMPILE/k
       dir env vars state modules (car xs)
       (λ (vars state modules as a)
         (COMPILE/k*
          dir env vars state modules (cdr xs)
          (λ (vars state modules dss ds)
            (k vars state modules (append as dss) (cons a ds))))))))
    