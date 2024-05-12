(define (g:assert condition)
  (if (not condition)
    (read-global-state)))

(define ((g:restrict predicate generator))
  (define val (generator))
  (g:assert (predicate val))
  val)
