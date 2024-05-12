; Shrink things in (loose) order from first to last
; Rationale: we want to shrink the lengths of lists before shrinking their
; elements
; We do some RNG because sometimes it's not obvious that something can't be
; shrunk further without breaking a predicate
(define ((make-random rand shrink name) . params)
  (call/cc save-global-state) ; backtrack to here
  (define reproduce
    (and
      (pair? reproduce-state)
      (equal? (caar reproduce-state) (cons name params))))
  (cons (cons name params)
    (if (not reproduce)
      (begin
        (if (pair? reproduce-state)
          (set! reproduce-state (cdr reproduce-state)))
        (apply rand params))
      (let ((old (cdar reproduce-state)))
        (set! reproduce-state (cdr reproduce-state))
        (if shrinking
          (cond
            ((and
               (> old 0)
               (< (random-real) 0.5))
             (set! shrinking #f)
             (shrink old))
            (else
              (set! reproduce #f)
              ; when we backtrack, reproduce will now be #f
              ; so if the old value doesn't work we generate new random values
              old))
          old)))))

(define g:random (make-random random random 'random))
(define g:random-real (make-random random-real (lambda x 0) 'random-real))

(define (((make-atomic-generator rand-gen transform) . params))
  (let* ((name-and-value (apply rand-gen params))
         (name (car name-and-value))
         (value (cdr name-and-value)))
    (set! generator-state (append generator-state (list name-and-value)))
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
