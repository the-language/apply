#lang racket
#|
    應用語
    Copyright (C) 2018  Zaoqi

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#
(provide (except-out (all-defined-out) %->首尾) (all-from-out racket srfi/9 compatibility/defmacro))
(require compatibility/defmacro)
(require srfi/9)

(define-macro (復名詞法 名 原) `(define-macro (,名 . 參) (cons ',原 參)))
(復名詞法 定 define)
(復名詞法 定表 define-record-type)
(復名詞法 名 let)
(復名詞法 名眾 letrec)
(復名詞法 始 begin)
(定 錯 error)
(定 算 eval)
(復名詞法 嵌 include)
(define-macro (嵌式 址)
  `(include/reader
    ,址
    (λ (source-name in)
      (let ([E (read-syntax source-name in)])
        (if (eof-object? E)
            eof
            (with-syntax ([E E])
              #'(quote E)))))))

(復名詞法 入 λ)
(定 用 apply)
(定 入？ procedure?)

(復名詞法 或 or)
(復名詞法 皆 and)
(復名詞法 若 if)
(復名詞法 若. cond)
(定 否則 #t)
(define-macro (若等. x . xs)
  (cons 'case (cons x (map (λ (x) (if (list? (car x)) x (cons 'else (cdr x)))) xs))))
(定 陰陽？ boolean?)
(定 陰 #f)
(定 陽 #t)
(定 等？ equal?)
(定 (全等？ 甲 乙 . 集)
   (皆 (或 (等？ 甲 乙) (皆 (數？ 甲) (數？ 乙) (= 甲 乙)))
      (或 (空？ 集) (用 全等？ 乙 集))))

(定 (%->首尾 甲)
   (若.
    [(string? 甲) (string->list 甲)]
    [(vector? 甲) (vector->list 甲)]
    [否則 甲]))
(定 (首 甲) (car (%->首尾 甲)))
(定 (尾 甲) (cdr (%->首尾 甲)))
(定 (首尾 之首 之尾) (cons 之首 (%->首尾 之尾)))
(定 (首尾？ 甲) (或 (pair? 甲) (string? 甲) (vector? 甲)))
(定 空 '())
(定 (空？ 甲) (null? (%->首尾 甲)))
(定 (列？ 甲) (或 (list? 甲) (string? 甲) (vector? 甲)))
(定 列 list)
(定 (連 甲 乙) (append (%->首尾 甲) (%->首尾 乙)))
(定 (簡 留？ 甲) (filter 留？ (%->首尾 甲)))
(定 (遍 換 . 列列) (apply map 換 (map %->首尾 列列)))
(定 (第一 甲) (first (%->首尾 甲)))
(定 (第二 甲) (second (%->首尾 甲)))
(定 (第三 甲) (third (%->首尾 甲)))

(定 數？ number?)
(定 加 +)
(定 減 -)
(定 乘 *)
(定 除 /)
(定 小？ <)
(定 大？ >)
(定 小等？ <=)
(定 大等？ >=)
(定 (數→字列 甲) (string->list (number->string 甲)))
(定 (字列→數 甲) (string->number (list->string 甲)))

(定 字？ char?)
(定 字→數 char->integer)
(定 數→字 integer->char)
(定 連串 string-append)

(定 符？ symbol?)
(定 (字列→符 甲) (string->symbol (list->string 甲)))
(定 (符→字列 甲) (string->list (symbol->string 甲)))

(定 空映 (make-immutable-hash))
(定 映 hash)
(定 映？ hash?)
(定 映-取 hash-ref)
(定 映-改 hash-set)
(定 映-有？ hash-has-key?)
(定 映→列 hash->list)
(定 列→映 make-immutable-hash)
