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

(define (zero? x) (= x 0))

(define first car)
(define (second x) (car (cdr x)))
(define cadr second)
(define (caar x) (car (car x)))
(define (cddr x) (cdr (cdr x)))
(define (third x) (car (cdr (cdr x))))
(define caddr third)
(define (cadar x) (car (cdr (car x))))

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))
(define (filter f xs)
  (if (null? xs)
      '()
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))
(define (foldl f x xs)
  (if (null? xs)
      x
      (foldl f (f (car xs) x) (cdr xs))))

(defmacro and
  (λ xs
    (cond
      [(null? xs) #t]
      [(null? (cdr xs)) (car xs)]
      [else (let ([a (mcsym 'a)])
              `(let ([,a ,(car xs)])
                 (if ,a
                     (and ,@(cdr xs))
                     #f)))])))
(defmacro or
  (λ xs
    (cond
      [(null? xs) #f]
      [(null? (cdr xs)) (car xs)]
      [else (let ([a (mcsym 'a)])
              `(let ([,a ,(car xs)])
                 (if ,a
                     ,a
                     (or ,@(cdr xs)))))])))

(defmacro vector
  (λ xs
    `(vec '_v ,@xs)))
(define (vector? x)
  (and (vec? x) (equal? (vec-ref x 0) '_v)))
(define (vector-ref x n)
  (assert (vector? x))
  (assert (>= x 0))
  (vec-ref x (+ 1 n)))

(define-macro (struct n fs . conf)
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

(struct hashv (v))
(define hash? hashv?)
(define hash->list hashv-v)
(define make-immutable-hash hashv)
(define (hash . xs) (hashv (%hash xs)))
(define (%hash xs)
  (if (null? xs)
      '()
      (cons (cons (car xs) (cadr xs)) (%hash (cddr xs)))))
(define (assoc v xs)
  (if (null? xs)
      #f
      (if (equal? (caar xs) v)
          (car xs)
          (assoc v (cdr xs)))))
(define (hash-ref h k . rs)
  (let ([r (assoc k (hashv-v h))])
    (if r
        (cdr r)
        (if (procedure? (car xs))
            ((car xs))
            (car xs)))))
(define (hash-set h k v)
  (hashv (cons (cons k v) (hashv-v h))))
(define (hash-has-key? h k) (assoc k (hashv-v h)))
(define hasheq hash)
(define make-immutable-hasheq make-immutable-hash)

(define-macro (with-handlers hs x)
  `(catch* ,x
           (λ (e)
             ,(let loop ([hs hs])
                (if (null? hs)
                    `(raise e)
                    (let ([h (car hs)])
                      `(if (,(car h) e)
                           (,(second h) e)
                           ,(loop (cdr hs)))))))))

(define (curry) (raise 'curry))
(define true #t)
(define false #f)
(define eq? equal?)
