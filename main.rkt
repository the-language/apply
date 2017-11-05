#lang racket
(require racket/cmdline)
(require "codegens.rkt")
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
 (define s2 (cons
             `(macrobegin
               (define _FILE_ ,f)
               '(void))
             s))
 (define x ((to) s2))
 (if (set-member? (seteq html lua) (to))
       (displayln x)
       (writeln x)))
