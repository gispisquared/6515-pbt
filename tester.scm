;;; Core tester:
(define generator-state)
(define reproduce-state)
(define shrinking)
(define shrunk)
(define verbose #f)

(define (test f property generator times)
  (let lp ((n times))
    (if (eq? n 0) #t
      (let* ((input (sample-from generator)) ; populates generator-state
             (output (f input)))
        (if (property input output)
          (lp (- n 1))
          (test-shrinks f property generator generator-state))))))

(define (flatten l)
  (cond ((pair? l) (append-map flatten l))
        ((null? l) '())
        (else (list l))))

(define (test-shrinks f property generator original-state)
  (let lp ((n 0))
    (if (eq? n (max 100 (length (flatten original-state))))
      (reproduce generator original-state)
      (let* ((input (shrink generator original-state)) ; populates generator-state
             (output (f input)))
        (cond
          ((or shrinking (property input output))
           (lp (+ n 1)))
          (else
            (cond
              (verbose
               (display "failed ")
               (pp input)))
            (test-shrinks f property generator generator-state)))))))

(define (sample-from generator)
  (set! generator-state '())
  (set! reproduce-state '())
  (set! shrinking #f)
  (generator))

(define (reproduce generator original-state)
  (set! generator-state '())
  (set! reproduce-state original-state)
  (set! shrinking #f)
  (generator))

(define (shrink generator original-state)
  (set! generator-state '())
  (set! reproduce-state original-state)
  (set! shrinking #t)
  (generator))
