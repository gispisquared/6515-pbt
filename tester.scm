;;; Core tester:
(define generator-state)
(define reproduce-state)
(define shrinking)
(define choices '()) ; will throw an error if we try to divide incorrectly
(define shrunk)

(define (test f property generator times shrink-times)
  (let lp ((n times))
    (if (eq? n 0) #t
      (let* ((input (sample-from generator)) ; populates generator-state
            (output (f input)))
        (if (property input output)
          (lp (- n 1))
          (test-shrinks f property generator generator-state shrink-times))))))

(define (test-shrinks f property generator original-state times)
  (let lp ((n times))
    (if (eq? n 0) (reproduce generator original-state)
      (let* ((input (shrink generator original-state)) ; populates generator-state
            (output (f input)))
        (if (or shrinking (property input output))
          (lp (- n 1))
          (test-shrinks f property generator generator-state times))))))

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

(define (flatten l)
  (cond ((pair? l) (append-map flatten l))
        ((null? l) '())
        (else (list l))))

(define (shrink generator original-state)
  (set! generator-state '())
  (set! reproduce-state original-state)
  (set! shrinking #t)
  ; + 1 to give the option of not shrinking at all
  (set! choices
      (+ 1 (length (flatten original-state))))
  (generator))
