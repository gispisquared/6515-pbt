(define (restrict predicate generator)
  (define (try)
    (define original-state generator-state)
    (set! generator-state '())
    (let ((val (generator)))
      (if (predicate val)
        (begin
          (set! generator-state (append original-state generator-state))
          val)
        (begin
          (set! generator-state '())
            (try)))))
  try)
