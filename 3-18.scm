(define (cycle-iter? x init)
  (let ((pairs init))
    (define (rec? p)
      (if (list-index pairs p)
          #t
          (begin
            (set! pairs (append pairs (list p)))
            #f)))
    (cond ((null? x) #f)
          ((rec? (cdr x)) #t)
          ((pair? (car x)) (or (cycle-iter? (car x) (copy-tree pairs))
                               (cycle-iter? (cdr x) pairs)))
          (else (cycle-iter? (cdr x) pairs)))))

(define (cycle? x) (cycle-iter? x '()))

;;; Test
(define p '(3)) ;; #f
(define p3 (cons p p)) ;; #f
(define p4 (cons (cons 2 p) p)) ;; #f
(define p7 (cons p3 p3)) ;; #f
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define ptoinfinityandbeyond (make-cycle '(1 2 3))) ;; #t
