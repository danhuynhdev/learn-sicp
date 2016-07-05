;;; Let assume that memoize grows as O(1)
;;; Fib don't need to recompute fib of numbers that it already computed. So it
;;; grows as O(n) because fib(n) will compute fib of [n -> 0]

;;; No (memoize fib) wouldn't work because fib itself don't know that it need
;;; run memoize on recursive calls.
