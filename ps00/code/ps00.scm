
;; Through the examples, it looks like the modulo operator is closer to what is
;; described in the problem set. Remainder is similar, but it looks (remainder a
;; b) is actually a - b * (a // b). It does not guarantee a result in the range
;; [0, b].

(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)
  )
)

(define -mod
  (lambda (a b n)
    (modulo (- a b) n)
  )
)

(define *mod
  (lambda (a b n)
    (modulo (* a b) n)
  )
)

;; (+mod 7 5 8) ; -> 4
;; (+mod 10 10 3) ; -> 2
;; (-mod 5 12 2) ; -> 1
;; (*mod 6 6 9) ; -> 0
;; (+mod 99 99 100) ; -> 98
;; (*mod 50 -3 100) ; -> 50

(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
       (modulo (op a1 a2) modulus))))

(define +m12 (modular 12 +))
(define -m12 (modular 12 -))
(define *m12 (modular 12 *))

;; (-m12 (*m12 (+m12 5 8) 3) 7) ; -> 8

;; ((modular 17 +) 13 11) ; -> 7
;; ((modular 17 -) 13 11) ; -> 2
;; ((modular 17 *) 13 11) ; -> 7

; slow-exptmod is a recursive algorithm and it grows Theta(n) in time and space.

; exptmod is a recursive algorithm that grows in Theta(log n) in both time and
; space.

(define (even? n)
  (= (remainder n 2) 0))

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (em base exponent)
      (if (= exponent 0) 1
      (if (even? exponent)
        (square (em base (/ exponent 2)))
        (mod* base (em base (- exponent 1))))))
    em))

;; ((exptmod 10) 2 0) ; -> 1
;; ((exptmod 10) 2 3) ; -> 8
;; ((exptmod 10) 3 4) ; -> 1
;; ((exptmod 100) 2 15) ; -> 68
;; ((exptmod 100) -5 3) ; -> 75
(define s:random random)
(define (random-k-digit-number k)
  (let ((last (s:random 10)))
    (if (= k 1)
        last
        (+ (* 10 (random-k-digit-number (- k 1))) last))
    ))

;; (random-k-digit-number 1) ; -> 1
;; (random-k-digit-number 3) ; -> 938
;; (random-k-digit-number 3) ; -> 773
;; (random-k-digit-number 50) ; -> 81738193025220370877007386144933212352044692612124

(define (count-digits n)
  (if (< (/ n 10) 1)
    1
    (+ (count-digits (/ n 10)) 1)))

;; (count-digits 3) ; -> 1
;; (count-digits 2007) ; -> 4
;; (count-digits 123456789) ; -> 9

(define (big-random n)
  (let ((rand (random-k-digit-number (count-digits n))))
      (if (< rand n)
        rand
        (big-random n))
    ))

;; (big-random 100) ; -> 13
;; (big-random 100) ; -> 0
;; (big-random 1) ; -> 0
;; (big-random 1) ; -> 0
;; (big-random (expt 10 40)) ; -> 5871120712712673243809644338398444626104

; slow prime is an O(n) space and time recursive algorithm
; checking up to sqrt(n) would make this an O(n^1/2) algorithm
; checking only odd factors would theoretically halve the order of growth, but
; would not change asymptotic complexity.

; Fermat's little theorem tests:


(define prime-test-iterations 20)

(define (prime-test-once p)
  (let ((a (big-random p)))
    (= ((exptmod p) a p) (modulo a p))))

(define (prime-test-n-times p n)
  (if (= n 1)
    (prime-test-once p)
    (and (prime-test-n-times p (- n 1)) (prime-test-once p))))

(define prime?
  (lambda (p)
    (if (or (= p 0) (= p 1))
      #f
      (prime-test-n-times p prime-test-iterations))))

;; (prime? 2) ; -> #t
;; (prime? 4) ; -> #f
;; (prime? 1) ; -> #f
;; (prime? 0) ; -> #f
;; (prime? 200) ; -> #f
;; (prime? 199) ; -> #t

; prime? will call prime-test-once a constant number of times, and
; prime-test-once is bounded by the time of exptmod which is O(log n) in both
; space and time.
; we use a recursive implementation.

(define (random-k-digit-prime k)
  (let ((maybe-prime (random-k-digit-number k)))
    (if (prime? maybe-prime)
      maybe-prime
      (random-k-digit-prime k)
  )))

;; (random-k-digit-prime 1) ; -> 5
;; (random-k-digit-prime 2) ; -> 41
;; (random-k-digit-prime 10) ; -> 4851760957
;; (count-digits (random-k-digit-prime 100)) ; -> 100
;; (count-digits (random-k-digit-prime 100)) ; -> 100

; 2 ways the procedure could fail:
; - prime? is probablistic, so it could return a non-prime number with some
; probability
; - it is possible (though perhaps unlikely) that we won't be able to find a
; prime before reaching the recursion depth limit.

(define (ax+by=1 a b)
    (let ((q (quotient a b))
          (r (remainder a b)))
    (if (= r 1)
      (list 1 (- q))
      (let ((xy_prime (ax+by=1 b r)))
        (list (cadr xy_prime) (- (car xy_prime) (* q (cadr xy_prime))))
        )
    )))

(define (inversemod n)
  (lambda (e)
    (if (= (gcd e n) 1)
      (let ((dk (ax+by=1 e n)))
        (if (< (car dk) 0)
          (+ (car dk) n)
          (car dk))
      )
      (error))))

;; ((inversemod 11) 5) ; -> 9
;; ((inversemod 11) 9) ; -> 5
;; ((inversemod 11) 7) ; -> 8
;; ((inversemod 12) 5) ; -> 5
;; ((inversemod 12) 8) ; -> error
;; ((inversemod 101) (random-k-digit-prime 2)) ; -> 29

(define (eg-encrypt message receiver)
  (let ((pk (eg-receiver-public-key receiver))
        (m (string->integer message)))
    (let ((big-p (eg-public-key-number pk))
          (dh-system (eg-public-key-system pk)))
      (let ((p (dh-system-prime dh-system))
            (a (dh-system-primitive-root dh-system))
            (T (random-k-digit-prime (dh-system-size dh-system))))
        (let ((x ((exptmod p) a T))
              (y (*mod m ((exptmod p) big-p T) p)))
          (cons x y))))))

(define (eg-send-message message receiver)
  (let ((encrypted (eg-encrypt message receiver))
        (decrypt (eg-receiver-decryption-procedure receiver)))
    (decrypt encrypted)))

(define dh-system (public-dh-system 100))
(define Alyssa (eg-receiver dh-system))


; The longest string that is correctly decrypted is 41 digits.


; If Eve has access to the receiver decryption procedure, they can just read the
; message. Other attacks might include inpersonating Ben or Alyssa in the
; initial handshake so that they are communicating with Eve instead, or
; tampering with the messages in another way.
