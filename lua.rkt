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
;(provide c)
(require "corescm.rkt")
(define-syntax %newns
  (syntax-rules ()
    [(_) '()]
    [(_ [r s] x ...) (cons (cons (quote r) (symbol->string (quote s))) (%newns x ...))]
    [(_ r x ...) (cons (cons (quote r) (symbol->string (quote r))) (%newns x ...))]))
(define-syntax-rule (newns x ...)
  (make-hasheq
   (%newns x ...)))
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
(define pre (includes "lua.lua"))

(define ns (newns