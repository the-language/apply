#lang racket
(provide FILE DIR)
(define odir (current-directory))
(define FILE (make-parameter ""))
(define (DIR)
  (let ([f (FILE)])
  (if (equal? f "")
      odir
      (let-values ([(d i0 i1) (split-path f)])
            (if (path-for-some-system? d)
               d
              odir)))))
