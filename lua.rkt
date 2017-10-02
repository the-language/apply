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

(provide c)
(require racket/sandbox)

(define-syntax-rule (includes f)
  (include/reader
   f
   (λ (source-name in)
     (let ([x (read-line in)])
       (if (eof-object? x)
           eof
           (let loop ([s x])
             (let ([x (read-line in)])
               (if (eof-object? x)
                   (datum->syntax #f s)
                   (loop (string-append s "\n" x))))))))))

(define-syntax-rule (ps [x v] ...)
  (make-hasheq
   (list
    (cons (quote x) (symbol->string (quote v))) ...)))

(define ids (ps
             [number? is_number]
             [boolean? is_boolean]
             [+ add]
             [- sub]
             [* mul]
             [/ quo]
             [> gt]
             [< lt]
             [= eq]
             [equal? equal]
             [>= gteq]
             [<= lteq]
             [and2 and2]
             [or2 or2]
             [not notf]
             [luatype type]
             [null? is_null]
             [pair? is_pair]
             [cons cons]
             [car car]
             [cdr cdr]
             [list list]
             [vec vector]
             [vec? is_vector]
             [vec-ref vector_ref]
             [symbol? is_symbol]
             [string->symbol symbol]
             [symbol->syring sym2str]
             [string? is_string]
             [void voidf]
             [atom? is_atom]
             [atom! atom]
             [atom-map! atom_map]
             [atom-set! atom_set]
             [atom-get atom_get]
             [assert assert]
             [promise? is_promise]
             [force force]
             [procedure? is_procedure]
             [wrtie write]
             [writeln writeln]
             [raise raise]))

(define-syntax-rule (exp x ...)
  (stream "(" x ... ")"))

(define-syntax-rule (block x ...)
  (stream (exp "function()"
               x ...
               "\nend") "()"))

(define-syntax-rule (var nme xc)
  (let ([x xc])
    (begin
      (set! nme (upme nme x))
      (stream "local " (newvarid x) "=nil\n"))))

(define-syntax-rule (setc me ms x v)
  (stream (id me x) "=" (EVAL me ms v) "\n"))

(define (APPLY me ms f xs)
  (let ([f (macroexpand ms f)])
    (cond
      [(eq? f 'λ)
       (let loop ([as (car xs)] [ss '()] [me me])
         (cond
           [(null? as)
            (exp "function(" ss ")"
              "return " (BEGIN me ms (cdr xs))
              "\nend")]
           [(symbol? as)
            (exp "function(" (if (null? ss) "..." (list ss ",...")) ")"
                 "local " (newvarid as) "=list(...)\n"
              "return " (BEGIN (upme me as) ms (cdr xs))
              "\nend")]
           [else (let ([s (car as)])
                   (loop (cdr as)
                         (if (null? ss)
                             (newvarid s)
                             (list ss "," (newvarid s)))
                         (upme me s)))]))]
      [(eq? f 'letrec) (LETREC me ms xs)]
      [(eq? f 'if)
       (block "if " (EVAL me ms (car xs))
              " then\nreturn " (EVAL me ms (second xs))
              "\nelse\nreturn " (EVAL me ms (third xs))
              "\nend")]
      [(eq? f 'cond)
       (let ([a (car xs)] [d (cdr xs)])
         (if (eq? (car a) 'else)
             (BEGIN me ms (cdr a))
             (block
              "if " (EVAL me ms (car a)) " then\n"
              "return " (BEGIN me ms (cdr a)) "\n"
              (let loop ([xs d])
                (if (null? xs)
                    (error 'cond-else)
                    (let ([a (car xs)])
                      (if (eq? (car a) 'else)
                          (stream "else\nreturn " (BEGIN me ms (cdr a)) "\nend")
                          (stream "elseif " (EVAL me ms (car a)) " then\n"
                                  "return " (BEGIN me ms (cdr a)) "\n"
                                  (loop (cdr xs))))))))))]
      [(eq? f 'begin) (BEGIN me ms xs)]
      [(eq? f 'set!) (setc me ms (car xs) (second xs))]
      [(eq? f 'ffi) (FFI (car xs))]
      [(eq? f 'quote) (QUOTE (car xs))]
      [(eq? f 'delay) (stream "{promiset,function()return " (EVAL me ms (car xs)) " end}")]
      [(eq? f 'catch*) (stream "spcall(function()return " (EVAL me ms (car xs)) " end," (EVAL me ms (second xs)) ")")]
      [else (stream (EVAL me ms f) "(" (%apply me ms xs) ")")])))

(define (%apply me ms xs)
  (cond
    [(null? xs) ""]
    [(null? (cdr xs)) (EVAL me ms (car xs))]
    [else (stream (EVAL me ms (car xs)) "," (%apply me ms (cdr xs)))]))

(define (bh ms xs)
  (if (null? xs)
      '()
      (let ([x (macroexpand ms (car xs))])
        (if (and (pair? x) (eq? (car x) 'begin))
            (append (bh ms (cdr x)) (bh ms (cdr xs)))
            (cons x (bh ms (cdr xs)))))))

(define (BEGIN me ms xs)
  (let ([xs (bh ms xs)])
    (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)]
          [b (equal? (last xs) '(void))])
      (if (null? (cdr xs))
          (EVAL me ms (car xs))
          (let ([me me])
            (block
             (map (λ (x)
                    (var me (second x)))
                  (filter (λ (x) (and (pair? x) (eq? (car x) 'def))) xs))
             (let loop ([x (car xs)] [xs (cdr xs)])
               (if (and (pair? x) (or (eq? (car x) 'set!) (eq? (car x) 'def)))
                   (stream
                    (setc me ms (second x) (third x))
                    (if (null? xs)
                        "return void"
                        (loop (car xs) (cdr xs))))
                   (if (null? xs)
                       (if b
                           "return void"
                           (stream
                            "return " (EVAL me ms x)))
                       (stream
                        "ig(" (EVAL me ms x) ")\n"
                        (loop (car xs) (cdr xs))))))))))))

(define (upme me x)
  (if (macrosym? x)
      (set-add me x)
      me))

(define idc 0)
(define (mknewid x)
  (set! idc (+ 1 idc))
  (string-append
   "zs"
   (number->string idc)
   "_"
   (list->string (map
                  (λ (x) (if (or (char-alphabetic? x) (char-numeric? x)) x #\_))
                  (string->list (symbol->string x))))))

(define (id me x)
  (cond
    [(set-member? me x) (newvarid x)]
    [(symbol? x) (hash-ref! ids x (λ () (mknewid x)))]
    [else (id me (macrosym-sym x))]))

(define evalr
  (make-module-evaluator
   '(module m racket
      (struct macrosym (id sym))
      (define mcsym
        (let ([midc 0])
          (λ (s)
            (set! midc (+ 1 midc))
            (macrosym midc s)))))))

(define (macrosym? x) (and (struct? x) (eq? (vector-ref (struct->vector x) 0) 'struct:macrosym)))
(define (macrosym-id x)
  (if (macrosym? x)
      (vector-ref (struct->vector x) 1)
      (error 'i)))
(define (macrosym-sym x)
  (if (macrosym? x)
      (vector-ref (struct->vector x) 2)
      (error 'i)))

(define (newvarid x)
  (if (symbol? x)
      (id (set) x)
      (string-append
       "zsm"
       (number->string (macrosym-id x))
       "_"
       (symbol->string (macrosym-sym x)))))

(define (LETREC me ms xs)
  (BEGIN me ms (append (map (λ (p) (list 'def (car p) (cdr p))) (car xs)) (cdr xs))))

(define (EVAL me ms x)
  (let ([x (macroexpand ms x)])
    (cond
      [(symbol? x) (id me x)]
      [(macrosym? x) (id me x)]
      [(pair? x) (APPLY me ms (car x) (cdr x))]
      [(null? x) "null"]
      [(number? x) (number->string x)]
      [(eq? x #t) "true"]
      [(eq? x #f) "false"]
      [(string? x) (stream "\"" x "\"")]
      [else (error)])))

(define (e x)
  (cond
    [(or (null? x) (and (stream? x) (stream-empty? x))) ""]
    [(stream? x) (string-append (e (stream-first x)) (e (stream-rest x)))]
    [(list? x) (string-append (e (car x)) (e (cdr x)))]
    [else x]))

(define pre (includes "prelude.lua"))
(define (rs s)
  (read (open-input-string (string-append "(" s ")"))))
(define prescm (rs (includes "prelude.scm")))

(define (c x)
  (endc
   (e
    (EVAL (set) (make-hasheq)
          (append (list 'begin)
                  prescm
                  x)))))

(define (endc x)
  (string-append
   pre
   "return " x))

(define (FFI x)
  (stream "l2sv(" (symbol->string x) ")"))

(define (macroexpand ms x)
  (cond
    [(and (pair? x) (eq? (car x) 'defmacro))
     (hash-set! ms (second x) (evalr (third x)))
     '(void)]
    [(and (pair? x) (hash-ref ms (car x) #f)) => (λ (mf) (macroexpand ms (apply mf (cdr x))))]
    [else x]))

(define (QUOTE x)
  (cond
    [(symbol? x) (stream "symbol(\"" (symbol->string x) "\")")]
    [(pair? x) (stream "cons(" (QUOTE (car x)) "," (QUOTE (cdr x)) ")")]
    [(null? x) "null"]
    [(eq? x #t) "true"]
    [(eq? x #f) "false"]
    [(number? x) (number->string x)]
    [(string? x) (stream "\"" x "\"")]
    [(macrosym? x) (QUOTE (macrosym-sym x))]
    [else (error)]))

(define (read* port)
  (let ([x (read-line port)])
    (if (eof-object? x)
        ""
        (string-append x "\n" (read* port)))))

(displayln (c (rs (read* (current-input-port)))))
