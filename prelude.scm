(defmacro define-macro
  (λ (p . v)
    `(defmacro ,(car p)
       (λ ,(cdr p)
         ,@v))))

(define-macro (let p . v)
    `((λ ,(map car p)
        ,@v)
      ,@(map second p)))

(defmacro define
  (λ (f . v)
    (if (pair? f)
        `(def ,(car f)
           (λ ,(cdr f)
             ,@v))
        `(def ,f
           ,@v))))

(define first car)
(define (second x) (car (cdr x)))
(define cadr second)
(define (third x) (car (cdr (cdr x))))
(define caddr third)
(define (cadar x) (car (cdr (car x))))

(defmacro and
  (λ xs
    (cond
      [(null? xs) #t]
      [(null? (cdr xs)) (car xs)]
      [else (let ([a (mcsym 'a)])
              `(let ([,a ,(car xs)])
                 (if ,a
                     ,a
                     (and ,@(cdr xs)))))])))

(defmacro vector
  (λ xs
    `(vec '_v ,@xs)))
(define (vector? x)
  (and (vec? x) (equal? (vec-ref x 0) '_v)))
(define (vector-ref x n)
  (assert (vector? x))
  (assert (>= x 0))
  (vec-ref x (+ 1 n)))

(define-macro (struct n fs)
  (let ([is (string->symbol (string-append (symbol->string n) "?"))])
  `(begin
     (define (,is x) (and (vec? x) (equal? (vec-ref x 0) (quote ,n))))
     (define (,n ,@fs) (vec (quote ,n) ,@fs))
     ,@(let loop ([c 1] [fs fs])
         (if (null? fs)
             '()
             (cons `(define (,(string->symbol (string-append (symbol->string n) "-" (symbol->string (car fs)))) x)
                      (assert (,is x))
                      (vec-ref x ,c))
                   (loop (+ c 1) (cdr fs))))))))
