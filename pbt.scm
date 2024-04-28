; How to specify the types of generated elements?
; Create a *generator language* in Scheme.
(define number-tree-gen
  ; A generator of a fixed prime from 1 to 100
  (define value (constant ((restrict prime? (number 1 100)))))
  ; A generator of numbers from 1 to 10
  (define len (number 1 10))
  (define number-tree
    ; Either
    (amb
      ; A generator of empty lists
      (constant '())
      ; A generator of conses, whose first element is value and whose second
      ; element is a list of number trees with a length between 1 and 10
      (cons-of value (list-of number-tree (len)))))
  number-tree)

; Design of generators:
; Look at reproduce-state. If it is empty, generate a new random object
; according to the specification given. Otherwise, use reproduce-state to
; generate an object previously generated. Either way, populate generator-state
; with information such that (gen-reproduce generator generator-state) produces
; the same value as the last time generator was called.

; This is a somewhat ugly design (and you see the repercussions especially
; in combinators.scm). The reason this design was chosen is that we want to
; have access to the generator state inside the shrinker (so that we can
; perturb it), but we want to be able to define generators in our generator
; language without refrerence to generator-state and reproduce-state.

(define (gen-new generator)
  (set! reproduce-state '())
  (generator))

(define (gen-reproduce generator generator-state)
  (set! reproduce-state generator-state)
  (generator))

; Design of the shrinker:
(shrink generator original-state)
; -> a new thing generated according to generator which is a shrunk version of
; (gen-reproduce generator original-state)