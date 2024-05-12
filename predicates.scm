(define (assert condition)
  (if (not condition)
    (read-global-state)))

(define ((restrict predicate generator))
  (define val (generator))
  (assert (predicate val))
  val)
