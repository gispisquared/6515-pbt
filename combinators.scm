(define ((cons-of gen1 gen2))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (define cons-state (cons '() '()))
  (set! generator-state '())
  (if reproduce (set! reproduce-state (caar original-reproduce)))
  (define first (gen1))
  (set-car! cons-state generator-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (cdar original-reproduce)))
  (define second (gen2))
  (set-cdr! cons-state generator-state)
  (set! generator-state (append original-state (list cons-state)))
  (if reproduce (set! reproduce-state (cdr original-reproduce)))
  (cons first second))

(define (list-of gen len)
  (if (= len 0) (constant '())
    (cons-of gen (list-of gen (- len 1)))))

(define ((amb gen1 gen2 prob))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (define choice
    (if reproduce
      (caar reproduce-state)
      (random-real)))
  (if reproduce
    (set! reproduce-state (cdar reproduce-state)))
  (set! generator-state '())
  (define generated
    (if (< choice prob) (gen1) (gen2)))
  (set! generator-state
    (append original-state (list (cons choice generator-state))))
  (if reproduce (set! reproduce-state (cdr original-reproduce)))
  generated)

(define ((constant t)) t)
