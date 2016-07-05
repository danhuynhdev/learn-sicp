;;; We gonna make a two way structure like this diagram
;;;                 +-------------------+
;;; +---------------|------+            |
;;; +->[[a|/] | -]--+->[[b|'] | -]->[[c|'] |/]
;;;
;;; Which will display this in the repl of guile scheme
;;; ((a) (b . #-2#) (c . #-2#))

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (dispatch m)
      (cond ((equal? m 'empty-deque?)
             (null? front-ptr))
            ((equal? m 'rear-ptr)
             rear-ptr)
            ((equal? m 'front-ptr)
             front-ptr)
            ((equal? m 'set-front-ptr!)
             (lambda (item) (set! front-ptr item)))
            ((equal? m 'set-rear-ptr!)
             (lambda (item) (set! rear-ptr item)))))
    dispatch))

(define (empty-deque? deque)
  (deque 'empty-deque?))
(define (front-ptr deque)
  (deque 'front-ptr))
(define (rear-ptr deque)
  (deque 'rear-ptr))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (caar (rear-ptr deque))))
(define (set-front-ptr! deque item)
  ((deque 'set-front-ptr!) item))
(define (set-rear-ptr! deque item)
  ((deque 'set-rear-ptr!) item))

(define (front-insert-deque! deque item)
  (let ((newlist (cons (cons item '()) '())))
    (if (empty-deque? deque)
        (begin
          (set-front-ptr! deque newlist)
          (set-rear-ptr! deque newlist)
          deque)
        (begin
          (set-cdr! (car (front-ptr deque)) newlist)
          (set-cdr! newlist (front-ptr deque))
          (set-front-ptr! deque newlist)
          deque))))

(define (rear-insert-deque! deque item)
  (let ((newlist (cons (cons item '()) '())))
    (if (empty-deque? deque)
        (begin
          (set-front-ptr! deque newlist)
          (set-rear-ptr! deque newlist)
          deque)
        (begin
          (set-cdr! (car newlist) (rear-ptr deque))
          (set-cdr! (rear-ptr deque) newlist)
          (set-rear-ptr! deque newlist)
          deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((null? (cdar (rear-ptr deque)))
         (set-front-ptr! deque '()))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         (set-cdr! (car (front-ptr deque)) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((null? (cdar (rear-ptr deque)))
         (set-front-ptr! deque '()))
        (else
         (set-rear-ptr! deque (cdar (rear-ptr deque)))
         (set-cdr! (rear-ptr deque) '()))))

(define (print-deque deque)
  (display (map car
                (front-ptr deque))) (newline))
