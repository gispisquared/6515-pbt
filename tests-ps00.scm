;(load "ps00/code/load.scm")
(load "ps00/code/p0utils.scm")
(load "ps00/code/ps00.scm")
(load "load.scm")

(set! verbose #t)

(define dh-system (public-dh-system 100))

(define Alyssa (eg-receiver dh-system))

(define (send-alyssa message)
  (eg-send-message message Alyssa))

(define (string-generator)
  (let ((str-len ((g:integer 0 100))))
    ((g:string (list "a" "b" "c" "d" "e") str-len))))

(define (encrypt-decrypt-id? x y)
  (?eq x y))

(send-alyssa "")

(test send-alyssa equal? string-generator 100)
