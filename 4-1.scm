(define (list-of-values-> exps env)
  (if (no-operands? exps)
      '()
      (let ((exp (eval (first-operand exps) env)))
        (cons exp
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-< exps env)
  (if (no-operands? exps)
      '()
      (let ((exp (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              exp))))


(define (list-of-values<- exps env)
  (if (no-operands? exps)
      '()
      (let ((exp (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              exp))))
