(define s:random random)
(define s:random-real random-real)

; Shrink things in (loose) order from first to last
; Rationale: we want to shrink the lengths of lists before shrinking their
; elements
; We do some RNG because sometimes it's not obvious that something can't be
; shrunk further without breaking a predicate
(define ((make-random rand f) . params)
  (if (null? reproduce-state)
    (apply rand params)
    (let ((old (car reproduce-state)))
      (set! reproduce-state (cdr reproduce-state))
      (set! old-val old)
      (if shrinking
        (begin
          (cond
            ((and
               (> old 0)
               (< (s:random-real) 0.5))
             (set! shrinking #f)
             (set! shrunk #t)
             (f old))
            (else old)))
        old))))

(define random (make-random s:random s:random))
(define random-real (make-random s:random-real (lambda x 0)))

(define (((make-atomic-generator rand-gen transform) . params))
  (let ((value (apply rand-gen params)))
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
  (list-ref choices ((integer 0 (length choices)))))

(define ((random-choices choices size))
  (let lp ((chosen '()) (size size) (choices choices))
    (if (null? choices) chosen
      (if ((boolean (/ size (length choices))))
        (lp (cons (car choices) chosen) (- size 1) (cdr choices))
        (lp chosen size (cdr choices))))))
