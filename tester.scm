;;; Core tester:
(define generator-state '())
(define reproduce-state '())

(define (test f property generator times shrink-times)
  (let lp ((n times))
    (if (eq? n 0) #t
      (let* ((input (generator)) ; populates generator-state
            (output (f input)))
        (if (property input output)
          (lp (- n 1))
          (test-shrinks f property generator generator-state shrink-times))))))

(define (test-shrinks f property generator original-state times)
  (let lp ((n times))
    (if (eq? n 0) original-state
      (let* ((input (shrink generator original-state)) ; populates generator-state
            (output (f input)))
        (if (property input output)
          (lp (- n 1))
          (test-shrinks f property generator generator-state))))))

(define (shrink generator original-state)
  (set! reproduce-state original-state)
  (generator))
