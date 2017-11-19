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
(require "../conf.rkt")
(define (EVAL x)
  (match x
    [(? char?) `(%char ,(string x))]
    [(cons a d) (cons (EVAL a) (EVAL d))]
    [_ x]))
(defpass (λ (conf x)
           (match (conf-get conf 'charstr)
             ['nochar (EVAL x)]
             [_ x])))
