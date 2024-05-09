(load "../sdf/manager/load")
(manage 'new 'term)

(load "load")

(define (gen-symbolic)
  ((g:one-of
     (g:random-choice '(a b c))
     (g:float 0 1)
     (g:cons
       (g:random-choice '(+ - *))
       (g:list gen-symbolic 2)))))
; careful! you need the recursive case to be last, and you need the number of
; times it's called to be bounded in expectation
; ideally you want gen-symbolic to call itself <1 time in expectation, so that
; it produces bounded-size expressions and doesn't trigger the timeout
; in this example since one-of is weighted equally, it calls itself 2/3 times
; in expectation

(define (substitute expr alist)
  (if (pair? expr)
    (cons (substitute (car expr) alist)
          (substitute (cdr expr) alist))
    (let ((expr-val (assq expr alist)))
      (if (pair? expr-val)
        (cadr expr-val)
        expr))))

(define gen-symbolic-and-values
  (g:cons
    gen-symbolic
    (lambda ()
      (list
        (zip '(a b c) ((g:list (g:float 0 1) 3)))))))

(define (evaluate symbolic-and-values)
  (eval (apply substitute symbolic-and-values) system-global-environment))

(define (simplification-works symbolic-and-values)
  (define first (evaluate symbolic-and-values))
  (define second
    (evaluate
      (cons
        (algebra-2 (car symbolic-and-values))
        (cdr symbolic-and-values))))
  (= first second))

(define symbolic-and-values
  (test simplification-works
        (lambda x (cadr x))
        gen-symbolic-and-values
        1000
        1000))
(pp symbolic-and-values)
; sometimes gives a nice one, like
; ((+ (+ b b) a) ((a .04208710849087266) (b .9106713117973909) (c 0)))
(pp (evaluate symbolic-and-values))
(pp
  (evaluate
    (cons
      (algebra-2 (car symbolic-and-values))
      (cdr symbolic-and-values))))
; finds something where simplifying causes a floating-point difference
