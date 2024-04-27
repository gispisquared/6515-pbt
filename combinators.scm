(define ((cons-of gen1 gen2))
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
      (define original-reproduce reproduce-state)
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

(define ((list-of gen len))
  (if (null? reproduce-state)
    (begin
      (define original-state generator-state)
      (define all-gen
        (let lp ((len len) (generated '()) (list-state '()))
          (if (= len 0)
            (cons generated list-state)
            (begin
              (set! generator-state '())
              (define first (gen))
              (lp (- len 1)
                  (cons first generated)
                  (cons generator-state list-state))))))
      (set! generator-state (cons (cdr all-gen) original-state))
      (car all-gen))
    (begin
      (define original-state generator-state)
      (set! generator-state '())
      (set! reproduce-state ((random-choices reproduce-state len)))
      (set! generator-state '())
      (define all-gen
        (let lp ((len len)
                 (generated '())
                 (list-state '())
                 (original-reproduce reproduce-state))
          (if (= len 0)
            (cons generated list-state)
            (begin
              (define original-reproduce reproduce-state)
              (set! generator-state '())
              (set! reproduce-state (car original-reproduce))
              (define first (gen))
              (lp (- len 1)
                  (cons first generated)
                  (cons generator-state list-state)
                  (cdr original-reproduce))))))
      (set! generator-state (cons (cdr all-gen) original-state))
      (car all-gen))))

(define ((amb gen1 gen2 prob))
  (if (null? reproduce-state)
    (begin
      (define original-state generator-state)
      (define choice (random-real))
      (set! generator-state '())
      (define generated
        (if (< choice prob) (gen1) (gen2)))
      (set! generator-state
        (cons (cons choice generator-state) original-state))
      generated)
    (begin
      (define original-state generator-state)
      (define choice (caar reproduce-state))
      (set! reproduce-state (cdar reproduce-state))
      (set! generator-state '())
      (define generated
        (if (< choice prob) (gen1) (gen2)))
      (set! generator-state
        (cons (cons choice generator-state) original-state))
      generated)))

(define ((constant t)) t)
