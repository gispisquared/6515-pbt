(define ((restrict predicate generator))
  (let ((val (generator)))
    (if (predicate val)
      val
      (read-global-state)))) ; start backtracking
