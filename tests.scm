(load "load.scm")

;; tests g:list, g:cons, g:integer, string, test
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
  (define list-len ((g:restrict (lambda (x) (> x 5)) (g:integer 0 10))))
  (define (my-string-gen charset)
    (define string-len ((g:integer 0 10)))
    (g:string charset string-len))
  ((g:list (my-string-gen '("a" "b" "c")) list-len)))

(pp (test my-sort sorted-version? string-list-gen 100))

; tests g:amb, restrict, g:cons, g:list, g:integer
(define (prime? n)
  (and (exact-positive-integer? n)
       (>= n 2)
       (let loop ((k 2))
         (or (> (square k) n)
             (and (not (= (remainder n k) 0))
                  (loop (+ k 1)))))))

; g:amb will shrink from second to first, so make sure the recursive case is
; second
(define (prime-tree)
  ((g:cons (g:restrict prime? (g:integer 0 100))
            (g:list (g:amb (g:constant '()) prime-tree 0.5) 2))))

(pp (sample-from prime-tree))
(pp (reproduce prime-tree generator-state))

; tests other generators

(define cons-gen (g:cons (g:boolean 0.5) (g:boolean 0.5)))
(pp (sample-from cons-gen))
(pp (reproduce cons-gen generator-state))

(define symbol-gen (g:symbol '(a b c) 5))
(pp (sample-from symbol-gen))
(pp (reproduce symbol-gen generator-state))

(pp (sample-from (g:boolean 0.5)))
(pp (reproduce (g:boolean 0.5) generator-state))

(pp (sample-from (g:float 5 10)))
(pp (reproduce (g:float 5 10) generator-state))

(pp (sample-from (g:random-subset (iota 10) 3)))
(pp (reproduce (g:random-subset (iota 10) 3) generator-state))

(pp (sample-from (g:random-choice (iota 10))))
(pp (reproduce (g:random-choice (iota 10)) generator-state))

; tests shrinking over predicate
(define (my-sort l)
  (if (null? l) '()
    (insert (car l) (cdr l))))

(pp (test my-sort sorted-version? string-list-gen 100))

(define (flatten l)
  (cond ((pair? l) (append-map flatten l))
        ((null? l) '())
        (else (list l))))
; tests shrinking over g:amb
(define (small-sum i o) (< (fold + 0 (flatten i)) 10))
(pp (test values small-sum
          (g:list (g:amb prime-tree (g:float 10 15) 0.1) 5) 100))
; -> list of five trees

(pp (test values small-sum
          (g:list (g:amb (g:float 10 15) prime-tree 0.1) 5) 100))
; -> list of five 10s

(pp (sample-from string-list-gen))
(pp (reproduce string-list-gen generator-state))

; tests infinite loop
(define (loop-forever . a) (loop-forever))
(pp (test loop-forever small-sum (g:integer 0 10) 1))

; tests shrinking to longer lists
(define (anti-shrink)
  (define list-len (- 10 ((g:integer 0 10))))
  ((g:list (g:integer 0 10) list-len)))
(pp (test values small-sum anti-shrink))

; tests backtracking
(define (gen-pythag)
  (define a ((g:integer 1 100)))
  (define b ((g:integer 1 100)))
  (define c ((g:restrict (lambda (c) (= (+ (* a a) (* b b)) (* c c)))
                       (g:integer 1 100))))
  (list a b c))
(pp (sample-from gen-pythag))

(define (gen-pythag-assert)
  (define a ((g:integer 1 100)))
  (define b ((g:integer 1 100)))
  (define c ((g:integer 1 100)))
  (g:assert (= (+ (* a a) (* b b)) (* c c)))
  (list a b c))
(pp (sample-from gen-pythag-assert))

(define gen-pythag-restrict
  (g:restrict
    (lambda (l) (= (+ (* (first l) (first l)) (* (second l) (second l)))
                   (* (third l) (third l))))
    (g:list (g:integer 1 100) 3)))
(pp (sample-from gen-pythag-restrict))

(define (random-length-list-and-float)
  ((g:cons
 	(g:list (g:integer 0 10) ((g:integer 0 5)))
 	(g:float 0 5))))
(sample-from random-length-list-and-float)
generator-state
(reproduce random-length-list-and-float generator-state)
(shrink random-length-list-and-float reproduce-state)
reproduce-state


; tests error handling: print (internal error) on an error in f
(define (error-producer x)
  (error "fail"))
(set! verbose #t)
(pp (test error-producer (lambda (x y) #f) (g:integer 0 10)))
