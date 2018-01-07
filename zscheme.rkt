(require compatibility/defmacro)
(require srfi/9) ; R7RS
(require racket/sandbox)
(define EVAL (make-evaluator 'racket))
(define null-set (set))
(define null-hash (hash))