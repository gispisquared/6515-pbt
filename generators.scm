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
      (+ (* value (- (cadr params) (car params))) (car params)))))

(define ((symbol charset len))
  (intern ((string-gen (map symbol->string charset) len))))

(define ((random-choice choices))
  (let ((i (if (null? reproduce-state)
             (random (length choices))
             (car reproduce-state))))
    (set! generator-state (cons i generator-state))
    (list-ref choices i)))

(define (reals-list len)
  (map (lambda (x) (random-real)) (iota len)))

(define (choose choices size reals)
  (fold
    (lambda (x acc)
      (let ((num-chosen (length acc))
            (choice (list-ref choices x))
            (prob (list-ref reals x)))
        (if
          (<= (/  (- size num-chosen) (- (length choices) x)) prob)
          acc
          (cons choice acc))))
    '()
    (iota (length choices))))

(define ((random-choices choices size))
  (let ((reals (if (null? reproduce-state)
                         (reals-list (length choices))
                         (car reproduce-state))))
    (set! generator-state (cons reals generator-state))
    (choose choices size reals)))
