#lang racket
(require racket/cmdline)
(require "codegens.rkt")
(require "conf.rkt")
(define to (make-parameter scm))
(command-line
 #:program "zscheme"
 #:once-any
 [("--html") "Compile to HTML" (to html)]
 [("-s" "--scheme") "Compile to scheme" (to scm)]
 [("-m" "--mal") "Compile to mal" (to mal)]
 [("-l" "--lua") "Compile to lua" (to lua)]
 #:args fs
 (define-values (s f)
   (match fs
     ['() (values (port->list) "")]
     [`(,f) (values (file->list f) f)]))
 (prelude
  get
  `((macrobegin
    (set! _FILE_ ,f)
    '(void))))
 (define x ((to) s))
 (if (set-member? (seteq html lua) (to))
     (displayln x)
     (writeln x)))
