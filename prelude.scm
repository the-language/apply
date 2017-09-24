(defmacro let
  (λ (p . v)
    (displayln sym)
    `((λ ,(map car p)
       ,@v)
      ,@(map second p))))
