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
      (if shrinking
        (begin
          (cond
            ((and
               (> old 0)
               (< (random-real) 0.5))
             (set! shrinking #f)
             (set! shrunk #t)
             (f old))
            (else old)))
        old))))

(define g:random (make-random random random))
(define g:random-real (make-random random-real (lambda x 0)))

(define (((make-atomic-generator rand-gen transform) . params))
  (let ((value (apply rand-gen params)))
    (set! generator-state (append generator-state (list value)))
    (transform value params)))

(define g:integer
  (make-atomic-generator
    (lambda (mn mx) (g:random (- mx mn)))
    (lambda (value params) (+ value (car params)))))

(define ((g:string charset len))
  (string-append*
    ((g:list (g:random-choice charset) len))))

(define g:boolean
  (make-atomic-generator
    (lambda params (g:random-real))
    (lambda (value params) (< value (car params)))))

(define g:float
  (make-atomic-generator
    (lambda params (g:random-real))
    (lambda (value params)
      (+ (* value (- (cadr params) (car params))) (car params)))))

(define ((g:symbol charset len))
  (intern ((g:string (map symbol->string charset) len))))

(define ((g:random-choice choices))
  (list-ref choices ((g:integer 0 (length choices)))))

(define ((g:random-subset choices size))
  (let lp ((chosen '()) (size size) (choices choices))
    (if (null? choices) chosen
      (if ((g:boolean (/ size (length choices))))
        (lp (cons (car choices) chosen) (- size 1) (cdr choices))
        (lp chosen size (cdr choices))))))
