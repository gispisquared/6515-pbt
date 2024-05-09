(load "../sdf/manager/load")
(manage 'new 'term)

(load "load")

(define gen-symbolic
  (g:amb
    (g:amb
      (g:float 0 10)
      (g:random-choice '(a b c)))
    (lambda ()
      (list
        ((g:random-choice '(+ - *)))
        (gen-symbolic)
        (gen-symbolic)))))

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

(define symbolic-and-values (sample-from gen-symbolic-and-values))
(define symbolic (car symbolic-and-values))
(pp symbolic-and-values)
(pp (evaluate symbolic-and-values))
(pp (evaluate (cons (algebra-2 symbolic) (cdr symbolic-and-values))))

(define (simplification-works symbolic-and-values)
  (define first (evaluate symbolic-and-values))
  (define second
     (evaluate
       (cons
         (algebra-2 (car symbolic-and-values))
         (cdr symbolic-and-values))))
  (= first second))

(define symbolic-and-values
  (test simplification-works (lambda x (cadr x)) gen-symbolic-and-values))
(pp (car symbolic-and-values))
(pp (algebra-2 (car symbolic-and-values)))
(pp (evaluate symbolic-and-values))
(pp
  (evaluate
    (cons
      (algebra-2 (car symbolic-and-values))
      (cdr symbolic-and-values))))
; finds something where simplifying causes a floating-point difference
