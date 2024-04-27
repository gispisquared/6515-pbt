(define (restrict predicate generator)
  (define (try)
    (define original-state generator-state)
    (set! generator-state '())
    (let ((val (generator)))
      (if (predicate val)
        (begin
          (set! generator-state (cons generator-state original-state))
          val)
        (try))))
  try)
