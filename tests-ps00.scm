(load "load")
(load "ps00/code/load")
(define dh-system (public-dh-system 100))

(define Alyssa (eg-receiver dh-system))

(define (send-alyssa message)
  (eg-send-message message Alyssa))

(define (string-generator)
  (let ((str-len ((integer 0 100))))
    ((string-gen (list "a" "b" "c" "d" "e") str-len))))

(define (encrypt-decrypt-id? x y)
  (?eq x y))

(send-alyssa "")

(test send-alyssa equal? string-generator 100)
