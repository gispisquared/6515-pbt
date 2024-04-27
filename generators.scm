; atomic: boolean, integer, string, float, symbol?
; combination: cons-of, list-of, amb
(define (cons-of gen1 gen2)
  (if (null? reproduce-state)
    (begin
      (define original-state generator-state)
      (define cons-state (cons '() '()))
      (set! generator-state '())
      (define first (gen1))
      (set-car! cons-state generator-state)
      (set! generator-state '())
      (define second (gen2))
      (set-cdr! cons-state generator-state)
      (set! generator-state (cons cons-state generator-state))
      (cons first second))
    (begin
      (define original-state generator-state)
      (define original-repdoduce reproduce-state)
      (define cons-state (cons '() '()))
      (set! generator-state '())
      (set! reproduce-state (car original-reproduce))
      (define first (gen1))
      (set-car! cons-state generator-state)
      (set! generator-state '())
      (set! reproduce-state (cdr original-reproduce))
      (define second (gen2))
      (set-cdr! cons-state generator-state)
      (set! generator-state (cons cons-state generator-state))
      (cons first second))))

(define (constant t) (lambda () t))

(define ((make-atomic-generator rand-gen transform) . params)
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
