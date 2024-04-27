(define (((make-atomic-generator rand-gen transform) . params))
  (let ((value
          (if (null? reproduce-state)
            (apply rand-gen params)
            (begin
              (let ((answer (car reproduce-state)))
                (set! reproduce-state (cdr reproduce-state))
                answer)))))
    (set! generator-state (append generator-state (list value)))
    (transform value params)))

(define integer
  (make-atomic-generator
    (lambda (mn mx) (random (- mx mn)))
    (lambda (value params) (+ value (car params)))))

(define ((string-gen charset len))
  (string-append*
    ((list-of (random-choice charset) len))))

(define boolean
  (make-atomic-generator
    (lambda params (random-real))
    (lambda (value params) (< value (car params)))))

(define float
  (make-atomic-generator
    (lambda params (random-real))
    (lambda (value params)
      (+ (* value (- (cdr params) (car params))) (car params)))))

(define ((symbol charset len))
  (intern ((string-gen charset len))))

(define ((random-choice choices))
  (let ((i (if (null? reproduce-state)
             (random (length choices))
             (car reproduce-state))))
    (set! generator-state (cons i generator-state))
    (list-ref choices i)))

(define (probabilities-list len)
  (if (= len 0)
    '()
    (append (list (random-real)) (probabilities (- len 1)))))

(define ((choose choices size probabilities))
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

(define ((random-choices choices size))
  (let ((probabilities (if (null? reproduce-state)
                         (probabilities-list (length choices))
                         (car reproduce-state))))
    (set! generator-state (cons probabilities generator-state))
    (choose choices size probabilites)))
