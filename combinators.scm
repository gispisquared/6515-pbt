(define ((g:cons-naive gen1 gen2))
  (cons (gen1) (gen2)))

(define (g:list-naive gen len)
  (if (= len 0) (g:constant '())
    (g:cons gen (g:list gen (- len 1)))))

; The above implementations are nice for testing, but they're not
; very useful for shrinking.
; For nice shrinking, we want our generator state to have additional structure
; (i.e. to be a decision tree instead of a decision list).

(define ((g:cons gen1 gen2))
  (define reproduce
    (and
      (pair? reproduce-state)
      (equal? (caar reproduce-state) 'cons)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (cadar original-reproduce)))
  (define first (gen1))
  (define first-state generator-state)
  (set! generator-state '())
  (if reproduce (set! reproduce-state (cddar original-reproduce)))
  (define second (gen2))
  (define second-state generator-state)
  (set! generator-state (append original-state
                                (list (cons 'cons
                                      (cons first-state second-state)))))
  (if (pair? reproduce-state)
    (set! reproduce-state (cdr original-reproduce)))
  (cons first second))

(define ((g:list gen len))
  (define reproduce
    (and
      (pair? reproduce-state)
      (equal? (caaar reproduce-state) 'list)
      (>= (cdaar reproduce-state) len)))
  (define original-state generator-state)
  (define original-reproduce reproduce-state)
  ; backtrack to generating a different subset if necessary
  (save-global-state)
  (if reproduce
    (begin
      ; Here I'm using the g:random-subset function to choose which of the list
      ; elements I keep. Therefore I need to reset reproduce-state so it'll
      ; generate new g:booleans.
      (set! reproduce-state '())
      (set! reproduce-state ((g:random-subset
                               (cdar original-reproduce) len)))))
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
  (set! generator-state (append original-state
                                (list (cons (cons 'list len)
                                            (cdr all-gen)))))
  (if reproduce
    (set! reproduce-state (cdr original-reproduce)))
  (car all-gen))

(define ((g:amb gen1 gen2 #!optional prob))
  (if (default-object? prob) (set! prob 0.5))
  (if ((g:boolean prob)) (gen1) (gen2)))

(define ((g:one-of . gens))
  (((g:random-choice gens))))

(define ((g:constant t)) t)
