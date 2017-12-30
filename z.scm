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

(define (map/symbol-append map xs)
  (foldl (λ (p map) (hash-set map (car p) (cdr p))) map xs))
(define (set-append s xs)
  (if (null? xs)
      s
      (set-append (set-add s (car xs)) (cdr xs))))

(define (z dir xs)
  (COMPILE-TOP/k null-hash null-hash null-hash null-set dir xs
                 (λ (state modules macros defines xs)
                   ($$top (set->list defines) xs))))

(define-record-type module
  (module export-macros export-values)
  module?
  (export-macros module-export-macros)
  (export-values module-export-values))
(define (MODULE/k state name modules macros defines dir exports body k) ; (k state defines modules xs)
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
                 (λ (state modules lam)
                   (k state defines (hash-set modules name (module export-macros export-values))
                      (cons
                       ($$define n ($$apply lam '())) cs1))))))))

(define null-state null-hash)
(define (COMPILE1/k modules macros defines dir x k)
  (COMPILE/k null-state modules macros defines dir #f x k))
(define (COMPILE-TOP/k state modules macros defines dir xs k)
  (BEGIN
   state modules macros defines dir #f (append xs (list 'VOIDz))
   (λ (state modules macros defines xs v)
     (k state modules macros defines xs))))
(define (COMPILE/k state modules macros defines dir exp? x k) ; (k state modules macros defines xs v)
  (cond
    [(pair? x)
     (let ([f (car x)] [args (cdr x)])
       (cond
         [(hash-ref macros f #f) => (λ (m) (COMPILE/k state modules macros defines dir exp? (apply m args) k))]
         [(eq? f 'DEFMACROz) (k state modules (hash-set macros (first args) (EVAL (second args))) defines '() $void)]
         [(eq? f 'HOSTz)
          (HOST args
                (λ (x)
                  (k state modules macros defines '() ($host-exp x)))
                (λ (x)
                  (COMPILE/k state modules macros defines dir exp? x k)))]
         [(eq? f 'define)
          (DEF/k
           (car args) (cdr args)
           (λ (f v)
             (COMPILE/k state
                        modules macros defines dir exp? v
                        (λ (state modules macros defines xs v)
                          (k state modules macros (set-add defines f) (append xs (list ($$define f v))) $void)))))]
         [(eq? f 'begin)
          (if exp?
              (COMPILE/k state modules macros defines dir exp? `((lambda () ,@args)) k)
              (BEGIN state modules macros defines dir exp? args k))]
         [(eq? f 'IMPALLz)
          (let ([name (first args)])
            (IMPALL/k state macros defines name (hash-ref modules name)
                      (λ (state modules macros defines xs)
                        (k state modules macros defines xs 'VOIDz))))]
         [(or (eq? f 'lambda) (eq? f 'λ))
          (LAMBDA/k state modules macros dir (car args) (cdr args)
                    (λ (state modules lam)
                      (k state modules macros defines '() lam)))]
         [(eq? f 'LISTz)
          (COMPILE/k* state
                      modules macros defines dir exp? args
                      (λ (state modules macros defines ys args)
                        (k state modules macros defines ys ($list args))))]
         [(eq? f 'MODULEz)
          (let ([name (car args)] [exports+body (cdr args)])
            (MODULE/k
             state name modules macros defines dir (car exports+body) (cdr exports+body)
             (λ (state defines modules cs)
               (k state modules macros defines cs $void))))]
         [(eq? f 'RECORDz) (k state modules macros defines (list ($$record (car args) (cadr args) (cddr args))) $void)]
         [(eq? f 'if)
          (COMPILE/k
           state modules macros defines dir exp? (first args)
           (λ (state modules macros defines cs0 b)
             (COMPILE/k
              state modules macros defines dir #t (second args)
              (λ (state modules macros defines cs1 x)
                (COMPILE/k
                 state modules macros defines dir #t (third args)
                 (λ (state modules macros defines cs2 y)
                   (k state modules macros defines (append cs0 cs1 cs2) ($if b x y))))))))]
         [else
          (COMPILE/k state
                     modules macros defines dir exp? f
                     (λ (state modules macros defines xs f)
                       (COMPILE/k* state
                                   modules macros defines dir exp? args
                                   (λ (state modules macros defines ys args)
                                     (k state modules macros defines (append xs ys) ($$apply f args))))))]))]
    [else (k state modules macros defines '()
             (cond
               [(eq? x 'VOIDz) $void]
               [(symbol? x) ($$var x)]
               [(number? x) ($$number x)]
               [(char? x) ($$char x)]
               [(string? x) ($$string x)]
               [(null? x) $null]
               [(eq? x #t) $true]
               [(eq? x #f) $false]
               [else (error 'compile "invalid syntax" x)]))]))
(define (COMPILE/tail state modules macros defines dir x k) ; (k state modules macros defines xs)
  (cond
    [(pair? x)
     (let ([f (car x)] [args (cdr x)])
       (cond
         [(hash-ref macros f #f) => (λ (m) (COMPILE/tail state modules macros defines dir (apply m args) k))]
         [(eq? f 'DEFMACROz) (error 'compile "invalid syntax" x)]
         [(eq? f 'HOSTz)
          (HOST args
                (λ (x)
                  (k state modules macros defines (list ($$tail-val ($host-exp x)))))
                (λ (x)
                  (COMPILE/tail state modules macros defines dir x k)))]
         [(eq? f 'define) (error 'compile "invalid syntax" x)]
         [(eq? f 'begin) (BEGIN/tail state modules macros defines dir args k)]
         [(eq? f 'IMPALLz) (error 'compile "invalid syntax" x)]
         [(or (eq? f 'lambda) (eq? f 'λ))
          (LAMBDA/k state modules macros dir (car args) (cdr args)
                    (λ (state modules lam)
                      (k state modules macros defines ($$tail-val lam))))]
         [(eq? f 'LISTz)
          (COMPILE/k* state
                      modules macros defines dir #f args
                      (λ (state modules macros defines ys args)
                        (k state modules macros defines ($$tail-val ($list args)))))]
         [(eq? f 'MODULEz)
          (let ([name (car args)] [exports+body (cdr args)])
            (MODULE/k
             state name modules macros defines dir (car exports+body) (cdr exports+body)
             (λ (state defines modules cs)
               (k state modules macros defines (append cs ($$tail-val $void))))))]
         [(eq? f 'RECORDz) (error 'compile "invalid syntax" x)]
         [(eq? f 'if)
          (COMPILE/k
           state modules macros defines dir #f (first args)
           (λ (state modules macros defines cs0 b)
             (COMPILE/tail
              state modules macros defines dir (second args)
              (λ (state modules macros defines xs)
                (COMPILE/tail
                 state modules macros defines dir (third args)
                 (λ (state modules macros defines ys)
                   (k state modules macros defines (append cs0 ($$tail-if b xs ys)))))))))]
         [else
          (COMPILE/k state
                     modules macros defines dir #f f
                     (λ (state modules macros defines xs f)
                       (COMPILE/k* state
                                   modules macros defines dir #f args
                                   (λ (state modules macros defines ys args)
                                     (k state modules macros defines (append xs ys ($$tail-apply f args)))))))]))]
    [else (k state modules macros defines
             ($$tail-val
              (cond
                [(eq? x 'VOIDz) $void]
                [(symbol? x) ($$var x)]
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
(define (BEGIN state modules macros defines dir exp? xs k)
  (cond
    [(null? (cdr xs)) (COMPILE/k state modules macros defines dir exp? (car xs) k)]
    [(pair? (car xs))
     (COMPILE/k state
                modules macros defines dir exp? (car xs)
                (λ (state modules macros defines cs1 v)
                  (BEGIN
                   state modules macros defines dir exp? (cdr xs)
                   (λ (state modules macros defines cs2 r)
                     (if (eq? v $void)
                         (k state modules macros defines (append cs1 cs2) r)
                         (k state modules macros defines (append cs1 (cons v cs2)) r))))))]
    [else (BEGIN state modules macros defines dir exp? (cdr xs) k)]))
(define (BEGIN/tail state modules macros defines dir xs k)
  (cond
    [(null? (cdr xs)) (COMPILE/tail state modules macros defines dir (car xs) k)]
    [(pair? (car xs))
     (COMPILE/k state
                modules macros defines dir #f (car xs)
                (λ (state modules macros defines cs1 v)
                  (BEGIN/tail
                   state modules macros defines dir (cdr xs)
                   (λ (state modules macros defines cs2)
                     (if (eq? v $void)
                         (k state modules macros defines (append cs1 cs2))
                         (k state modules macros defines (append cs1 (cons v cs2))))))))]
    [else (BEGIN/tail state modules macros defines dir (cdr xs) k)]))
(define (COMPILE/k* state modules macros defines dir exp? xs k)
  (if (null? xs)
      (k state modules macros defines '() '())
      (COMPILE/k state
                 modules macros defines dir exp? (car xs)
                 (λ (state modules macros defines cs1 a)
                   (COMPILE/k* state
                               modules macros defines dir exp? (cdr xs)
                               (λ (state modules macros defines cs2 d)
                                 (k state modules macros defines (append cs1 cs2) (cons a d))))))))
(define (LAMBDA/k state modules macros dir args body k) ; (k state modules lambda)
  (BEGIN/tail
   state modules macros null-set dir body
   (λ (state modules macros defines1 cs)
     (k state modules ($$lambda (set->list defines1) args cs)))))
(define (HOST xs k1 k2)
  (let ([x (car xs)] [xs (cdr xs)])
    (cond
      [(eq? (first x) $host) (k1 (second x))]
      [(eq? (first x) '_) (k2 (second x))])))

(define (z-current xs) (z (current-directory) xs))
