;;; Core tester:
(define generator-state)
(define reproduce-state)
(define shrinking)
(define verbose #f)

(define (test-property property f input timeout)
  ; TODO add timeouts
  (define done #f)
  (register-timer-event 100 (lambda () (if (not done) (error "timed out"))))
  (define result
    (guard
      (condition (else #f))
      (property input (f input))))
  (set! done #t)
  result)



(define (test f property generator #!optional times timeout)
  (if (default-object? times)
    (set! times 100))
  (if (default-object? timeout)
    (set! timeout 100))
  (let lp ((n times))
    (if (eq? n 0) #t
      (if (test-property property f
                         (sample-from generator) ; populates generator-state
                         timeout)
        (lp (- n 1))
        (test-shrinks f property generator generator-state timeout)))))

(define (num-leaves l)
  (cond ((pair? l) (+ (num-leaves (car l)) (num-leaves (cdr l))))
        ((null? l) 0)
        (else 1)))

(define (test-shrinks f property generator original-state timeout)
  (let lp ((n (max 100 (num-leaves original-state))))
    (if (eq? n 0)
      (reproduce generator original-state)
      (let ((input (shrink generator original-state))) ; populates generator-state
        (cond
          ((or shrinking (test-property property f input timeout))
           (lp (- n 1)))
          (else
            (cond
              (verbose
                (display "failed ")
                (pp input)))
            (test-shrinks f property generator generator-state timeout)))))))

(define (sample-from generator)
  (set! continuations '())
  (set! generator-state '())
  (set! reproduce-state '())
  (set! shrinking #f)
  (generator))

(define (reproduce generator original-state)
  (set! continuations '())
  (set! generator-state '())
  (set! reproduce-state original-state)
  (set! shrinking #f)
  (generator))

(define (shrink generator original-state)
  (set! continuations '())
  (set! generator-state '())
  (set! reproduce-state original-state)
  (set! shrinking #t)
  (generator))
