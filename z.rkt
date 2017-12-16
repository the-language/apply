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
(define null-hash (hasheq))
(define list->hash make-immutable-hasheq)
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
(define (TOP ms dir xs k) ; ms=macros
  (if (null? xs)
      (k ms '())
      (let ([x (car xs)] [xs (cdr xs)])
        (if (pair? x)
            (let ([a (car x)])
              (cond
                [(hash-ref ms a #f) => (λ (m) (TOP ms dir (cons (apply m (cdr x)) xs) k))]
                [(eq? a 'DEFMACROz) (TOP (hash-set ms (second x) (eval (third x))) dir xs k)]
                [(eq? a 'begin) (TOP ms dir (append (cdr x) xs) k)]
                [(eq? a 'load) (dir-of/file->list dir (second x)
                                                  (λ (ndir file)
                                                    (TOP ms ndir file
                                                         (λ (ms ys)
                                                           (TOP ms dir xs
                                                                (λ (ms xs)
                                                                  (k ms (append ys xs))))))))]
                [else (TOP ms dir xs (λ (nms nxs) (k nms (cons x nxs))))]))
            (TOP ms dir xs k)))))
; exports : (list <导出标识> <值>)
(struct module (name exports body))
(define (TOPp dir xs)
  (TOP
   null-hash dir xs
   (λ (ms xs)
     (partition/k
      (λ (s) (eq? (car s) 'MODULEz)) xs
      (λ (modules xs)
        (let ([mods (map (λ (m)
                           (let ([m (cdr m)])
                             (let ([name (car m)] [exports (cadr m)] [body (cddr m)])
                               (module name exports body)))))])
        (let ([module-infos (list->hash (map
                                         (λ (m)
                                               (cons (module-name m) (map first (module-exports m)))) mods))])
          (MODULEp module-infos ms modules xs))))))))
(define (IDin-mod m s)
  (string->symbol
   (string-append
    (symbol->string m)
    (string-append
    "@"
    (string-append
     (symbol->string s)
     "Mz")))))
(define (IDmod m)
  (string->symbol
   (string-append
    (symbol->string m)
    "@Mz")))
(define (EXPAND-MODULE module-infos ms m)
  (TOP ; bug: ms未使用
   null-hash ""
   (λ (ms xs)
     
     
(define (MODULEp module-infos ms modules xs)
  (MODULE-IMPORTp
  (map (λ (m) (EXPAND-MODULE module-infos ms null-hash m)) modules)
             

(define (COMPILEp ms x k)
  (cond
    [(pair? x)
     (let ([a (car x)] [xs (cdr x)])
       (cond
         [(hash-ref ms a #f) => (λ (m) (COMPILEp ms (apply m xs) k))]
         [(eq? a 'DEFMACROz) (k (hash-set ms (first xs) (eval (second xs))) 'VOIDz)]
         [(eq? a 'define) (DEFINE ms xs k)]
         [(eq? a 'begin) (BEGIN ms xs k)]
         [(eq? a 'quote) (QUOTE (first xs))]
         [(eq? a 'if)
          (COMPILEp ms (first xs)
                    (λ (ms b)
                      (COMPILEp ms (second xs) ; BUG 当if中有define,begin等
                                (λ (ms x)
                                  (COMPILEp ms (third xs)
                                            (λ (ms y)
                                              (k ms (list 'if b x y))))))))]
         [(or (eq? a 'λ) (eq? a 'lambda))
          (k ms (cons 'lambda (cons (car xs) (BEGIN ms (cdr xs) (λ (ms v) (list v))))))]
         [else
          (COMPILEp ms a
                    (λ (ms f)
                      (COMPILE* ms xs
                                (λ (ms xs)
                                  (k ms (cons f xs))))))]))]
    [else 0]))
(define (BEGIN ms xs k)
  (if (null? (cdr xs))
      (COMPILEp ms (car xs) k)
      (COMPILEp ms (car xs)
                (λ (nms v)
                  (if (pair? v)
                      (cons v (COMPILEp nms (BEGIN (cdr xs)) k))
                      (COMPILEp nms (BEGIN (cdr xs)) k))))))
(define (DEFINE ms xs k)
  (let ([f (first xs)])
    (if (symbol? f)
        (COMPILEp ms (second xs)
                  (λ (ms v)
                    (cons (list 'define (first xs) v)
                          (k ms 'VOIDz))))
        (DEFINE ms (list (car f) (cons 'λ (cons (cdr f) (cdr xs)))) k))))
(define (QUOTE x)
  (cond
    [(list? x) (cons 'list (map QUOTE x))]
    [(pair? x) (list 'cons (QUOTE (car x)) (QUOTE (cdr x)))]
    [(symbol? x) (list 'quote x)]
    [else x]))
(define (COMPILE* ms xs k)
  (if (null? (cdr xs))
      (COMPILEp ms (car xs) (λ (ms v) (k ms (list v))))
      (COMPILEp ms (car xs)
                (λ (ms a)
                  (COMPILE* ms (cdr xs)
                            (λ (ms d)
                              (k ms (cons a d))))))))
