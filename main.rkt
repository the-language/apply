#lang racket
(require racket/cmdline)
(require "codegens.rkt")
(define to (make-parameter scm))
(command-line
 #:program "zscheme"
 #:once-any
 [("-s" "--scheme") "Compile to scheme" (to scm)]
 [("-m" "--mal") "Compile to mal" (to mal)]
 [("-l" "--lua") "Compile to lua" (to lua)]
 #:args fs
 (define s (if (null? fs)
               (port->list)
               (foldr append '() (map file->list fs))))
 (define x ((to) s))
 (if (string? x)
       (displayln x)
       (writeln x)))
