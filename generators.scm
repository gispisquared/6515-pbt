; atomic: boolean, integer, string, float, symbol
; combination: cons-of, list-of, amb
; other: random-choice, random-choices

(define (((make-atomic-generator rand-gen transform) . params))
  (let ((value
          (if (null? reproduce-state)
            (apply rand-gen params)
            (car reproduce-state))))
    (set! generator-state (cons value generator-state))
    (transform value params)))

(define integer
  (make-atomic-generator
    (lambda (mn mx) (random (- mx mn)))
    (lambda (value params) (+ value (car params)))))

(define ((string-gen charset len))
  (string-append
    ((list-of (random-choice charset) len))))

(define boolean
  (make-atomic-generator
    (lambda params (random-real))
    (lambda (value params) (< value (car params)))))

(define float
  (make-atomic-generator
    (lambda params (random-real)
      (lambda (value params)
        (+ (* value (- (cdr params) (car params))) (car params))))))

(define ((symbol charset len))
  (intern ((string-gen charset len))))
