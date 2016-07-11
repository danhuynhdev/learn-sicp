(define fibs
   (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;;; fib(n) addition
