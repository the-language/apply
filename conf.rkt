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
(provide newconf conf-get %prelude prelude runprelude)
(define-syntax newconf
  (syntax-rules ()
    [(_) (hasheq)]
    [(_ [c v] x ...) (hash-set (newconf x ...) (quote c) v)]
    [(_ c x ...) (hash-set (newconf x ...) (quote c) #t)]))
(define (conf-get c x) (hash-ref c x #f))

(define preludes '())
(define (%prelude f)
  (set! preludes
        (cons (λ (c)
                (f (λ (v) (conf-get c v))))
              preludes)))
(define-syntax-rule (prelude get x)
  (%prelude (λ (get) x)))
(define (runprelude conf)
  (foldl append '() (map (λ (f) (f conf)) preludes)))
(prelude
 get
 `((macrobegin
    (define _FILE_ "")
    (define (_DIR_)
      (if (eq? _FILE_ "")
          #f
          (let-values ([(d f md) (split-path _FILE_)])
            (if (path-for-some-system? d)
                d
                #f))))
    '(void))))
