(load "load.scm")

;; tests list-of, cons-of, integer, string, test
(define (my-sort l)
  (if (null? l) '()
    (insert (car l) (my-sort (cdr l)))))

(define (insert val sorted-lst)
  (cond
    ((null? sorted-lst) (list val))
    ((string<? val (car sorted-lst)) (cons val sorted-lst))
    (else (cons (car sorted-lst) (insert val (cdr sorted-lst))))))

(define (sorted-version? orig l)
  (eq-vals? l (sort orig string<?)))

(define (eq-vals? l1 l2)
  (or (and (null? l1) (null? l2))
      (and (not (null? l1))
           (not (null? l2))
           (string=? (car l1) (car l2))
           (eq-vals? (cdr l1) (cdr l2)))))

(define (string-list-gen)
  (define list-len ((restrict (lambda (x) (> x 5)) (integer 0 10))))
  (define (my-string-gen charset)
    (define string-len ((integer 0 10)))
    (string-gen charset string-len))
  ((list-of (my-string-gen '("a" "b" "c")) list-len)))

(pp (test my-sort sorted-version? string-list-gen 100))

; tests amb, restrict, cons-of, list-of, integer
(define (prime? n)
  (and (exact-positive-integer? n)
       (>= n 2)
       (let loop ((k 2))
         (or (> (square k) n)
             (and (not (= (remainder n k) 0))
                  (loop (+ k 1)))))))

; amb will shrink from second to first, so make sure the recursive case is
; second
(define (prime-tree)
  ((cons-of (restrict prime? (integer 0 100))
            (list-of (amb (constant '()) prime-tree 0.5) 2))))

(pp (sample-from prime-tree))
(pp (reproduce prime-tree generator-state))

; tests other generators

(define symbol-gen (symbol '(a b c) 5))
(pp (sample-from symbol-gen))
(pp (reproduce symbol-gen generator-state))

(pp (sample-from (boolean 0.5)))
(pp (reproduce (boolean 0.5) generator-state))

(pp (sample-from (float 5 10)))
(pp (reproduce (float 5 10) generator-state))

(pp (sample-from (random-choices (iota 10) 3)))
(pp (reproduce (random-choices (iota 10) 3) generator-state))

(pp (sample-from (random-choice (iota 10))))
(pp (reproduce (random-choice (iota 10)) generator-state))

; tests shrinking over predicate
(define (my-sort l)
  (if (null? l) '()
    (insert (car l) (cdr l))))

(pp (test my-sort sorted-version? string-list-gen 100))

; tests shrinking over amb
(define (small-sum i o) (< (fold + 0 (flatten i)) 10))
(pp (test values small-sum
          (list-of (amb prime-tree (float 10 15) 0.1) 5) 100))
; -> list of five trees

(pp (test values small-sum
          (list-of (amb (float 10 15) prime-tree 0.1) 5) 100))
; -> list of five 10s
