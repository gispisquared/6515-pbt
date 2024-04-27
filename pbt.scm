;;; General API / toy test case:
(define (my-sort l)
  (if (null? l) '()
    (insert (car l) (sort (cdr l)))))

(define (insert val sorted-lst)
  (cond
    ((null? sorted-lst) (list val))
    ((< val (car sorted-lst)) (cons val sorted-lst))
    (else (cons (car sorted-lst) (insert val (cdr sorted-lst))))))

(define (sorted-version? orig l)
  (eq-vals? l (sort orig)))

(define (eq-vals? l1 l2)
  (or (and (null? l1) (null? l2))
      (and (not (null? l1))
           (not (null? l2))
           (eq? (car l1) (car l2))
           (eq-vals? (cdr l1) (cdr l2)))))

(define integer-list
  '(list-of integer 10 100))

(test my-sort sorted-version? (make-generator integer-list))


;;; Core tester:
(define (test f property generator times)
  (let lp ((n times))
    (if (eq? n 0) #t
      (and (test-once f property generator times)
           (lp (- n 1))))))

(define (test-once f property input-spec)
  (define generator-state)
  (let* ((input (eval input-spec (the-environment)))
         (output (apply f input)))
    (or (property input output)
        (shrink f property input input-spec generator-state))))


; How to specify the predicate?
; Answer: create some language for specifying types. Example:
; Atomic types: boolean, integer, string, float, null, symbol
; Means of combination:
; (cons-of)
; (list-of) takes arguments min-length, max-length (default 1, 10)
; (amb-gen) takes an argument probability (default 0.5)
; Means of abstraction: eg
; Actually we want to write this language in Scheme itself, so that we can call
; it.
(define number-tree-gen
  (begin
    (define value (restrict prime? number))
    (define len (constant (number 1 10)))
    (define number-tree
      (amb-gen null
               (cons-of value (list-of tree len len))))
    (restrict restriction? number-tree)))

(define (restrict predicate generator)
  (define (try)
    (let ((val (generator)))
      (if (predicate val) val
        (try))))
  try)

(define (constant val) (lambda () val))

(define (cons-of gen1 gen2)
  (lambda () (cons (gen1) (gen2))))

; How to design shrinker?
; Keep a variable generator-state which contains a list of real numbers in
; (0,1) corresponding to decisions
; -> Might want to add additional structure
(define global-state '(1 2 3))
(define (add-state val)
  (set! global-state (cons val global-state)))
(add-state 5)
global-state

(define (add-states l)
  (define current global-state)
  (set! global-state '())
  (map add-state l)
  (define new global-state)
  (set! global-state current)
  (add-state new))

(add-states '(1 2 3))
global-state

; The point is we want reproducibility:
(define (gen-new generator)
  (define reproduce-state '())
  (generator))

(define (gen-reproduce generator generator-state)
  (define reproduce-state generator-state)
  (generator))

(shrink generator generator-state)
; -> a new generator which only produces shrinks of generator-state
