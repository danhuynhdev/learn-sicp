(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys table)
      (let ((record
             (assoc (car keys) (cdr table))))
        (if record
            (if (= (length keys) 1)
                (cond ((pair? (cdr record)) #f)
                      (else (cdr record)))
                (cond ((not (pair? (cdr record))) #f)
                      (else (lookup (cdr keys) record))))
            #f)))
    (define (insert! keys value table)
      (let ((record
             (assoc (car keys) (cdr table))))
        (if record
            (if (= (length keys) 1)
                (set-cdr! record value)
                (insert! (cdr keys) value record))
            (if (= (length keys) 1)
                (set-cdr! table
                          (cons (cons (car keys) value)
                                (cdr table)))
                (let ((new-table (list (car keys))))
                  (set-cdr! table
                            (cons new-table (cdr table)))
                  (insert! (cdr keys) value new-table)))))
        'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys)
                                    (lookup keys local-table)))
            ((eq? m 'insert-proc!) (lambda (keys value)
                                     (insert! keys value local-table)))
            ((eq? m 'print) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (lookup table keys)
  ((table 'lookup-proc) keys))
(define (insert! table keys value)
  ((table 'insert-proc!) keys value))
(define (print table)
  (display (table 'print)) (newline))

;;; Test

(define t1 (make-table))
(insert! t1 '(math +) 43)
(insert! t1 '(math -) 45)
(insert! t1 '(math *) 42)
(insert! t1 '(letters a) 97)
(insert! t1 '(letters b) 98)
(insert! t1 '(a b c) 10)

(lookup t1 '(a b c)) ;; 10
(lookup t1 '(math +)) ;; 43
(lookup t1 '(letters a)) ;; 97
