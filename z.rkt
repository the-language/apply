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
(define %eval%ns (make-base-namespace))
;(define (EVAL x) (eval x %eval%ns))
(require racket/sandbox)
(define EVAL (make-evaluator 'racket))
(provide z z-current)
(define null-set (set))
(define null-hash (hash))
; map/symbol = Hash Symbol _
(define null-map/symbol (hasheq))
(define map/symbol-set hash-set)
(define map/symbol-get hash-ref)
(define map/symbol-has? hash-has-key?)
(define (dir-of/file->list dir path k)
  (define rcd (current-directory))
  (current-directory dir)
  (define f (simplify-path path))
  (define-values (d i0 i1) (split-path f))
  (define xs (file->list f))
  (current-directory rcd)
  (k d xs))
(define (partition/k f xs k)
  (let-values ([(x y) (partition f xs)])
    (k x y)))

(define (map/symbol-append map xs)
  (foldl (λ (p map) (map/symbol-set map (car p) (cdr p))) map xs))
(define (set-append s xs)
  (if (null? xs)
      s
      (set-append (set-add s (car xs)) (cdr xs))))

(define (z dir xs)
  (TOP/k null-hash null-map/symbol null-set dir xs
         (λ (modules macros defines xs) xs)))
(define (z-current xs) (z (current-directory) xs))

(struct module (export-macros export-values))
(define (TOP/k modules macros defines dir xs k) ; (k modules macros defines xs)
  (if (null? xs)
      (k modules macros defines '())
      (let ([x (car xs)] [xs (cdr xs)])
        (if (pair? x)
            (let ([f (car x)] [args (cdr x)])
              (cond
                [(eq? f 'MODULEz)
                 (let ([name (car args)] [exports+body (cdr args)])
                   (MODULE/k
                    name modules macros defines dir (car exports+body) (cdr exports+body)
                    (λ (defines module cs)
                      (TOP/k (hash-set modules name module) macros defines dir xs
                             (λ (modules macros defines cs2)
                               (k modules macros defines (append cs cs2)))))))]
                [else
                 (COMPILE1/k
                  modules macros defines dir x
                  (λ (macros defines cs v)
                    (TOP/k modules macros defines dir xs
                           (λ (modules macros defines xs)
                             (if (pair? v)
                                 (k modules macros defines (append cs (cons v xs)))
                                 (k modules macros defines (append cs xs)))))))]))
            (TOP/k modules macros defines dir xs k)))))
(define (MODULE/k name modules macros defines dir exports body k) ; (k defines module xs)
  (COMPILE-TOP/k
   modules macros defines dir body
   (λ (macros defines xs)
     (partition/k
      (λ (export) (map/symbol-has? macros (second export))) exports
      (λ (exportmacros exports)
        (let ([export-macros
               (map
                (λ (e)
                  (cons (first e) (map/symbol-get macros (second e)))) exportmacros)]
              [export-values (map first exports)])
          (MODULEdo name export-macros export-values exports xs defines k)))))))
(define (MODULEmk1/k name m c export-values defines k)
  (if (null? export-values)
      (k defines '())
      (MODULEmk1/k name m (+ 1 c) (cdr export-values) defines
                   (λ (defines xs)
                     (let ([n (MODULEvalue-name name (car export-values))])
                       (k (set-add defines n) (cons `(define ,n (list-ref ,m ,c)) xs)))))))
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
(define (MODULEdo name export-macros export-values exports xs defines k)
  (let ([n (MODULEname name)])
    (MODULEmk1/k
     name n 0 export-values defines
     (λ (defines cs1)
       (k defines (module export-macros export-values)
          (cons
           `(define ,n ((lambda () ,@xs (list ,@(map second exports)))))
           cs1))))))

(define (COMPILE1/k modules macros defines dir x k)
  (COMPILE/k modules macros defines dir #f x k))
(define (COMPILE-TOP/k modules macros defines dir xs k)
  (BEGIN
   modules macros defines dir (append xs (list 'VOIDz))
   (λ (macros defines xs v)
     (k macros defines xs))))
(define (COMPILE/k modules macros defines dir exp x k) ; (k macros defines xs v)
  (cond
    [(pair? x)
     (let ([f (car x)] [args (cdr x)])
       (cond
         [(map/symbol-get macros f #f) => (λ (m) (COMPILE/k modules macros defines dir exp (apply m args) k))]
         [(eq? f 'DEFMACROz) (k (map/symbol-set macros (first args) (EVAL (second args))) defines '() 'VOIDz)]
         [(eq? f 'define)
          (DEF/k
           (car args) (cdr args)
           (λ (f v)
             (COMPILE/k
              modules macros defines dir exp v
              (λ (macros defines xs v)
                (k macros (set-add defines f) (append xs (list `(define ,f ,v))) 'VOIDz)))))]
         [(eq? f 'begin)
          (if exp
              (COMPILE/k modules macros defines dir exp `((lambda () ,@args)) k)
              (BEGIN modules macros defines dir args k))]
         [(eq? f 'IMPALLz)
          (let ([name (first args)])
            (IMPALL/k macros defines name (hash-ref modules name)
                      (λ (macros defines xs)
                        (k macros defines xs 'VOIDz))))]
         [else
          (COMPILE/k
           modules macros defines dir exp f
           (λ (macros defines xs f)
             (COMPILE/k*
              modules macros defines dir exp args
              (λ (macros defines ys args)
                (k macros defines (append xs ys) (cons f args))))))]))]
    [else (k macros defines '() x)]))
(define (IMPALL/k macros defines name module k)
  (let ([module-macros (module-export-macros module)]
        [values (module-export-values module)])
    (k (map/symbol-append macros module-macros) (set-append defines values)
       (map
        (λ (x)
          `(define ,x ,(MODULEvalue-name name x)))
        values))))
(define (DEF/k a d k)
  (if (symbol? a)
      (k a (car d))
      (DEF/k (car a) (cons 'λ (cons (cdr a) d)) k)))
(define (BEGIN modules macros defines dir xs k)
  (if (null? (cdr xs))
      (COMPILE/k modules macros defines dir exp (car xs) k)
      (COMPILE/k
       modules macros defines dir exp (car xs)
       (λ (macros defines cs1 v)
         (BEGIN
          modules macros defines dir (cdr xs)
          (λ (macros defines cs2 r)
            (if (pair? v)
                (k macros defines (append cs1 (cons v cs2)) r)
                (k macros defines (append cs1 cs2) r))))))))
(define (COMPILE/k* modules macros defines dir exp xs k)
  (if (null? xs)
      (k macros defines '() '())
      (COMPILE/k
       modules macros defines dir exp (car xs)
       (λ (macros defines cs1 a)
         (COMPILE/k*
          modules macros defines dir exp (cdr xs)
          (λ (macros defines cs2 d)
            (k macros defines (append cs1 cs2) (cons a d))))))))
