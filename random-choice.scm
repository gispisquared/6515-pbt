(define (random-choice choices)
  (let ((i (if (null? reproduce-state)
             (random (length choices))
             (car reproduce-state))))
    (set! generator-state (cons i generator-state))
    (list-ref choices i)))

(define (probabilities-list len)
  (if (= len 0)
    '()
    (append (list (random-real)) (probabilities (- len 1)))))

(define (choose choices size probabilities)
  (fold 
    (lambda (x acc)
      (let ((num-chosen (length acc))
            (choice (list-ref choices x))
            (prob (list-ref probabilities x)))
        (if 
          (<= (/  (- size num-chosen) (- (length choices) x)) prob)
          acc
          (append acc (list choice)))))
    '()
    (iota (length choices))))

(define (random-choices choices size)
  (let ((probabilities (if (null? reproduce-state) 
                         (probabilities-list (length choices))
                         (car reproduce-state))))
    (set! generator-state (cons probabilities generator-state))
    (choose choices size probabilites)))
