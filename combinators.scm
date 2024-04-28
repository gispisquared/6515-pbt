(define ((cons-of-naive gen1 gen2))
  (cons (gen1) (gen2)))

(define (list-of-naive gen len)
  (if (= len 0) (constant '())
    (cons-of gen (list-of gen (- len 1)))))

(define ((amb-naive gen1 gen2 prob))
  (if ((boolean prob)) (gen1) (gen2)))

; The above implementations are nice for testing, but they're not
; very useful for shrinking.
; For nice shrinking, we want our generator state to have additional structure
; (i.e. to be a decision tree instead of a decision list).

(define ((cons-of gen1 gen2))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (car original-reproduce)))
  (define first (gen1))
  (define first-state generator-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (cadr original-reproduce)))
  (define second (gen2))
  (define second-state generator-state)
  (set! generator-state (append original-state (list first-state second-state)))
  (if reproduce (set! reproduce-state (cddr original-reproduce)))
  (cons first second))

(define ((list-of gen len))
  (define reproduce (not (null? reproduce-state)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (if reproduce
    (begin
      ; Here I'm using the random-choices function to choose which of the list
      ; elements I keep. Therefore I need to reset reproduce-state so it'll
      ; generate new booleans.
      (set! reproduce-state '())
      (set! reproduce-state ((random-choices (car original-reproduce) len)))))
  (define all-gen
    (let lp ((len len)
             (generated '())
             (list-state '())
             (original-reproduce reproduce-state))
      (if (= len 0)
        (cons generated list-state)
        (begin
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
  (set! generator-state (append original-state (list (cdr all-gen))))
  (if reproduce
    (set! reproduce-state (cdr original-reproduce)))
  (car all-gen))

(define ((amb gen1 gen2 prob))
  (set! shrunk #f)
  (define (get-value gen)
    (if shrunk (set-car! reproduce-state '()))
    (car ((cons-of gen (constant 0)))))
  (if ((boolean prob)) (get-value gen1) (get-value gen2)))

(define ((constant t)) t)
