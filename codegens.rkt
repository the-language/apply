#lang racket
(provide lua mal scm html)
(require "lua.rkt")
(require "mal.rkt")
(require "scm.rkt")

(define (html x)
  (let ([o (open-output-string)])
    (writeln (scm x) o)
    (let ([s (get-output-string o)])
      (string-append
       "<script src=\"biwascheme.js\">\n"
       s
       "</script>"))))
