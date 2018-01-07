(DEFMACROz define-macro
           (λ (local-state env state args default k)
             (k
              local-state env state
              (let ([f (car args)])
                (if (pair? f)
                    `(DEFMACROz ,(car f)
                                (λ (local-state env state args default k)
                                  (k local-state env state (apply (λ ,(cdr f) ,@(cdr args)) args))))
                    `(DEFMACROz ,f
                                (λ (local-state env state args default k)
                                  (k local-state env state (apply ,(second args) args)))))))))
(define-macro (defmacro id formals . body)
  `(define-macro (,id ,@formals)
     ,@body))