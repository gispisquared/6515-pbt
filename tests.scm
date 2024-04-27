;; tests list-of, integer
(define (my-sort l)
  (if (null? l) '()
    (insert (car l) (cdr l)))))

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
           (eq? (car l1) (car l2))
           (eq-vals? (cdr l1) (cdr l2)))))

(define (integer-list-gen)
  (define list-len ((integer 1 100)))
  ((list-of (integer -1000 1000) list-len)))

(test my-sort sorted-version? string-list-gen 100 100)

;; tests cons-of, string
(define (cons-list-of gen len)
  (if (= len 0) (constant '())
    (cons-of gen (cons-list-of gen (- len 1)))))

(define (string-list-gen)
  (define list-len ((integer 1 10)))
  (define (my-string-gen charset)
    (define string-len ((integer 1 10)))
    (string-gen charset string-len))
  ((cons-list-of (my-string-gen '("a" "b" "c")) list-len)))

(sample-from (cons-of (integer 1 10) (integer 1 10)))
generator-state
