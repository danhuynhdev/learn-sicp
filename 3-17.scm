(define (count-pairs x)
  (let ((pairs '()))
    (define (rec? p)
      (if (list-index pairs p)
          #t
          (begin
            (set! pairs (append pairs (list p)))
            #f)))
    (define (iter x)
      (if (or (rec? x) (not (pair? x)))
          0
          (+ (iter (car x))
             (iter (cdr x))
             1)))
    (iter x)))

;;; Test
(define p '(3)) ;; 1
(define p3 (cons p p)) ;; 2
(define p4 (cons (cons 2 p) p)) ;; 3
(define p7 (cons p3 p3)) ;; 3
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define ptoinfinityandbeyond (make-cycle '(1 2 3))) ;; 3
