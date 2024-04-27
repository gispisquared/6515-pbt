(define ((cons-of gen1 gen2))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (define cons-state (cons '() '()))
  (set! generator-state '())
  (if reproduce (set! reproduce-state (car original-reproduce)))
  (define first (gen1))
  (set-car! cons-state generator-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (cdr original-reproduce)))
  (define second (gen2))
  (set-cdr! cons-state generator-state)
  (set! generator-state (cons cons-state original-state))
  (cons first second))))

(define ((list-of gen len))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (set! generator-state '())
  (if reproduce
    (set! reproduce-state ((random-choices reproduce-state len))))
  (set! generator-state '())
  (define all-gen
    (let lp ((len len)
             (generated '())
             (list-state '())
             (original-reproduce reproduce-state))
      (if (= len 0)
        (cons generated list-state)
        (let ((original-reproduce reproduce-state))
          (set! generator-state '())
          (if reproduce
            (set! reproduce-state (car original-reproduce)))
          (let ((first (gen)))
            (lp (- len 1)
                (cons first generated)
                (cons generator-state list-state)
                (if reproduce
                  (cdr original-reproduce)
                  '())))))))
  (set! generator-state (cons (cdr all-gen) original-state))
  (car all-gen))

(define ((amb gen1 gen2 prob))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
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
    (cons (cons choice generator-state) original-state))
  generated)

(define ((constant t)) t)
