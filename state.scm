; Save state, for backtracking
(define continuations '())

(define (save-global-state)
  (call/cc
    (lambda (k)
      (set! continuations
        (cons
          (list k 0 generator-state reproduce-state shrinking)
          continuations)))))

(define (read-global-state)
  (if (null? continuations) (error "No more backtracking possible!"))
  (let ((state-to-read (car continuations)))
    (set-car! (cdar continuations) (+ 1 (cadar continuations)))
    ; backtrack further if necessary:
    (cond ((> (cadar continuations) 100)
           (set! continuations (cdr continuations))
           (read-global-state)))
    ; restore the global state present at that time:
    (set! generator-state (third state-to-read))
    (set! reproduce-state (fourth state-to-read))
    (set! shrinking (fifth state-to-read))
    ((first state-to-read) #f))) ; call the saved continuations
