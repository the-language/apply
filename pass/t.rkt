#lang racket

; Symbol -> Exp -> Bool
(define (GCfind? s x)
  (match x
    [`(lambda ,_ ,@v) (GCfind? s v)]
    [(? symbol? x) (eq? s x)]
    [`(define ,_ ,v) (GCfind? s v)]
    [`(quote ,_) #f]
    [(? pair? x) (or (GCfind? s (car x)) (GCfind? s (cdr x)))]
    [_ #f]))
(define (BEGINgc xs)
  (let* ([lastv (last xs)] [lastvoid (or (equal? lastv '(void)) (define? lastv))])
    (let ([xs (filter-not (λ (x) (equal? x '(void))) xs)])
      (let-values ([(rdefs rnotdefs) (partition define? xs)])
        (let ([defs (map (λ (x) (cons (second x) (third x))) rdefs)])
          (let-values ([(marked rest)
                        (partition (λ (x)
                                     (or (and lastvoid (eq? (car x) 'void))
                                         (GCfind? (car x) rnotdefs))) defs)])
            (let loop ([marked marked] [rest rest])
              (if (null? rest)
                  xs
                  (let-values ([(new newrest) (partition (λ (x) (GCfind? (car x) marked)) rest)])
                    (if (null? new)
                        (let ([marked (map car marked)])
                          (let ([xs (filter
                                     (λ (x)
                                       (if (define? x)
                                           (set-member? marked (second x))
                                           #t)) xs)])
                            (if lastvoid
                                (append xs (list '(void)))
                                xs)))
                        (loop (append new marked) newrest)))))))))))
;(set! BEGINgc (λ (x) x))
(define (BEGINappend macros cs)
  (if (null? cs)
      '()
      (let ([c (macroexpand macros (car cs))])
        (if (begin? c)
            (append (BEGINappend macros (cdr c)) (BEGINappend macros (cdr cs)))
            (cons c (BEGINappend macros (cdr cs)))))))
(define (BEGIN conf macros xs)
  (BEGINgc
   (BEGINappend
    macros
    (map
     (λ (x)
       (if (define? x)
           (DEFINE conf macros (cadr x) (cddr x))
           (EVAL conf macros x))) ;这里会GC
     (BEGINappend
      macros
      xs))))) ;在GC前append
(define (QUOTE1 conf x)
  (cond
    [(pair? x) (list 'cons (QUOTE1 conf (car x)) (QUOTE1 conf (cdr x)))]
    [(symbol? x) `(quote ,x)]
    [(null? x) '(quote ())]
    [(and (char? x)
          (eq? (conf-get conf 'charstr) 'nochar)) `(%char ,(string x))]
    [else x]))
(define (%QUOTE max-count conf x count f)
  (if (pair? x)
      (if (> count max-count)
          (let ([v (gensym)])
            (%QUOTE max-count conf (car x) 0
                    (λ (a c2)
                      (%QUOTE max-count conf (cdr x) c2
                              (λ (d c3)
                                `(begin
                                   (define ,v (cons ,a ,d))
                                   ,(f v (+ c3 1))))))))
          (%QUOTE max-count conf (car x) count
                  (λ (a c2)
                    (%QUOTE max-count conf (cdr x) c2
                            (λ (d c3)
                              (f `(cons ,a ,d) (+ c3 1)))))))
      (f (QUOTE1 conf x) (+ count 1))))
(define (QUOTE macros conf x)
  (if (conf-get conf 'split)
      (if (pair? x)
          (EVAL macros conf (%QUOTE (conf-get conf 'split) conf x 0 (λ (x c) x)))
          (QUOTE1 conf x))
      (QUOTE1 conf x)))
