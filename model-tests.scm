(load "load.scm")

(define (insert val sorted-lst)
  (cond
    ((null? sorted-lst) (list val))
    ((string<? val (car sorted-lst)) (cons val sorted-lst))
    (else (cons (car sorted-lst) (insert val (cdr sorted-lst))))))

(define (rep-list-set)
  (define elements '())
  (define (run-command command . args)
    (case command
      ((add)
       (if (not (member (car args) elements))
         (set! elements (cons (car args) elements)))
       'done)
      ((remove)
       (set! elements
         (let lp ((seen '()) (unseen elements))
           (cond
             ((null? unseen) seen)
             ((equal? (car unseen) (car args))
              (append seen (cdr unseen)))
             (else (lp (cons (car unseen) seen) (cdr unseen))))))
       'done)
      ((has)
       (pair? (member (car args) elements)))
      (else (error "Command not defined"))))
  run-command)

(define (rep-alist-set)
  (define elements '())
  (define (run-command command . args)
    (case command
      ((add)
       (set! elements (cons (list (car args) #t) elements))
       'done)
      ((remove)
       (set! elements
         (del-assoc (car args) elements))
       'done)
      ((has)
       (pair? (assoc (car args) elements)))
      (else (error "Command not defined"))))
  run-command)

(define (gen-add)
  (list 'add ((g:integer 0 10))))

(define (gen-remove)
  (list 'remove ((g:integer 0 10))))

(define (gen-has)
  (list 'has ((g:integer 0 10))))

(define (gen-commands)
  ((commands-gen (list gen-has gen-remove gen-add) 100)))

(pp (test (run-both-models rep-list-set rep-alist-set)
          (lambda (in out)
            (equal? (car out) (cdr out)))
          gen-commands
          100))
; works

(define (rep-list-set-buggy)
  (define elements '())
  (define (run-command command . args)
    (case command
      ((add)
       (set! elements (cons (car args) elements))
       'done)
      ((remove)
       (set! elements
         (let lp ((seen '()) (unseen elements))
           (cond
             ((null? unseen) seen)
             ((equal? (car unseen) (car args))
              (append seen (cdr unseen)))
             (else (lp (cons (car unseen) seen) (cdr unseen))))))
       'done)
      ((has)
       (pair? (member (car args) elements)))
      (else (error "Command not defined"))))
  run-command)

(pp
  (test (run-both-models rep-list-set-buggy rep-alist-set)
        (lambda (in out)
          (equal? (car out) (cdr out)))
        (commands-gen (list gen-has gen-remove gen-add) 100)
        100))
; -> '((add 0) (add 0) (remove 0) (has 0))

(define (binary-tree)
  (define head '())
  (define children '())
  (define (run-command command . args)
    (case command
      ((add)
       (cond
         ((null? head)
          (set! head (car args))
          (set! children (cons (binary-tree) (binary-tree))))
         ((string>? head (car args))
          ((car children) command (car args)))
         ((string<? head (car args))
           ((cdr children) command (car args))))
       'done)
      ((sorted)
       (cond
         ((null? head) '())
         ((null? children) (list head))
         (else (append (list head)
                       ((car children) command)
                       ((cdr children) command)))))
      (else (error "Command not defined"))))
  run-command)

(define (sorted? l)
  (or (null? l)
      (null? (cdr l))
      (and (string<=? (car l) (cadr l))
           (sorted? (cdr l)))))

(define (gen-add-str)
  (list 'add ((string-gen '("a" "b" "c") 5))))

(pp (test (run-model binary-tree)
          (lambda (in out)
            (every (lambda (i o)
                     (or (eq? (car i) 'add) (sorted? o))) in out))
          (commands-gen (list gen-add-str (constant '(sorted))) 100)
          100))
; -> ((add "baaaa") (add "aaaaa") (sorted))

; fix bug: walk tree in-order instead of pre-order
(define (binary-tree)
  (define head '())
  (define children '())
  (define (run-command command . args)
    (case command
      ((add)
       (cond
         ((null? head)
          (set! head (car args))
          (set! children (cons (binary-tree) (binary-tree))))
         ((string>? head (car args))
          ((car children) command (car args)))
         ((string<? head (car args))
          ((cdr children) command (car args))))
       'done)
      ((sorted)
       (cond
         ((null? head) '())
         ((null? children) (list head))
         (else (append ((car children) command)
                       (list head)
                       ((cdr children) command)))))
      (else (error "Command not defined"))))
  run-command)

(pp (test (run-model binary-tree)
          (lambda (in out)
            (every (lambda (i o)
                     (or (eq? (car i) 'add) (sorted? o))) in out))
          (commands-gen (list gen-add-str (constant '(sorted))) 100)
          100))
; -> works

; but implementation is still buggy! test by comparing with a model
(define (sorted-list)
  (define lst '())
  (define (run-command command . args)
    (case command
      ((add)
       (set! lst (insert (car args) lst))
       'done)
      ((sorted)
       lst)
      (else (error "Command not defined"))))
  run-command)

(pp (test (run-both-models binary-tree sorted-list)
          (lambda (in out)
            (equal? (car out) (cdr out)))
          (commands-gen (list gen-add-str (constant '(sorted))) 100)
          100))
; -> ((add "aaaaa") (add "aaaaa") (sorted))

(define (binary-tree)
  (define head '())
  (define children '())
  (define (run-command command . args)
    (case command
      ((add)
       (cond
         ((null? head)
          (set! head (car args))
          (set! children (cons (binary-tree) (binary-tree))))
         ((string>? head (car args))
          ((car children) command (car args)))
         (else
          ((cdr children) command (car args))))
       'done)
      ((sorted)
       (cond
         ((null? head) '())
         ((null? children) (list head))
         (else (append ((car children) command)
                       (list head)
                       ((cdr children) command)))))
      (else (error "Command not defined"))))
  run-command)

(pp (test (run-both-models binary-tree sorted-list)
          (lambda (in out)
            (equal? (car out) (cdr out)))
          (commands-gen (list gen-add-str (constant '(sorted))) 100)
          100))
; -> works
