(defmacro let
  (λ (p . v)
    `((λ ,(map car p)
       ,@v)
      ,@(map second p))))

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

(defmacro and
  (λ xs
    (if (null? xs)
        #t
        (let ([a (mcsym 'a)])
          `(let ([,a ,(car xs)])
             (if ,a
                 ,a
                 (and ,@(cdr xs))))))))

(defmacro vector
  (λ xs
    `(vec '_v ,@xs)))
