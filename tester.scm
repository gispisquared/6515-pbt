;;; Core tester:
(define generator-state)
(define reproduce-state)
(define shrinking)
(define verbose #f)

(define (test-property property f input timeout)
  (define done #f)
  (register-timer-event timeout (lambda () (if (not done) (error "timed out"))))
  (define result
    (guard
      (condition (else (if (equal? (error-object-message condition) "timed out")
                         'time-out 
                         'internal-error)))
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
      (let ((result (test-property property f (sample-from generator) timeout)))
        (if (eq? result #t) 
          (lp (- n 1))
          (test-shrinks f property generator generator-state timeout))))))

(define (num-leaves l)
  (cond ((pair? l) (+ (num-leaves (car l)) (num-leaves (cdr l))))
        ((null? l) 0)
        (else 1)))

(define (test-shrinks f property generator original-state timeout)
  (let lp ((n (max 100 (num-leaves original-state))))
    (if (eq? n 0)
      (reproduce generator original-state)
      (let* ((input (shrink generator original-state)) ; populates gen state
             (result (test-property property f input timeout)))
        (cond
          ((or shrinking (eq? result #t))
           (lp (- n 1)))
          (else
            (cond
              (verbose
                (cond ((eq? result 'time-out) 
                       (display "failed (timeout): "))
                      ((eq? result 'internal-error)
                       (display "failed (internal error): "))
                      (else (display "failed ")))
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
