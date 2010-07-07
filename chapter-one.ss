; ================== CODE ==================

; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

; 1.2
(/ 
 (+ 4 5 (- 2 3 (+ 6 (/ 4 3))))
 (* 3 (- 6 2) (- 2 7)))

; 1.3
(define (sum-of-two-largest x y z)
  (cond ((= (min x y z) x) (sum-of-squares y z))
        ((= (min x y z) y) (sum-of-squares x z))
        ((= (min x y z) z) (sum-of-squares x y))))
  
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (square x) (* x x))

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-if-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (new-sqrt-iter guess old-guess x)
  (if (new-good-enough? guess old-guess)
      guess
      (new-sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) old-guess) .001))

; 1.8
(define (cbrt-iter guess old-guess x)
  (if (new-good-enough? guess old-guess)
      guess
      (cbrt-iter (cubic-improve guess x) guess x)))

(define (cubic-improve guess x)
  (/ 
   (+ 
    (/ x (* guess guess))
    (* 2 guess))
   3))

; ================== ANSWERS ==================

; 1.1
; You can do this by looking at the code :)

; 1.2
; Remember that the arithmetic functions take many arguments

; 1.3
; This is the best I could come up with at this point
; I assumed 'min' was part of the standard functions, and it was

; 1.4
; This is an eye-opener - functions can be manipulated like any other value!
; You can call functions by making sure they're the first element in a
; list - it doesn't matter how it got there!

; 1.5
; The following code will throw your interpreter in an infinite loop, if
; it uses applicative-order evaluation
; (test 0 (p))

; This is because (p) is defined recursively as itself, without ever terminating
; A normal-order evaluation interpreter wouldn't try to obtain the value of (p)
; until it actually needed it, which won't ever happen because the if will
; terminate on the first clause, without reaching the substitution of 'y'

; 1.6
; Something unexpected is supposed to happen here, but the results are the same
; (new-if-sqrt-iter 1 (* 1000 1000))
; (sqrt-iter 1 (* 1000 1000))

; I believe that this is scheme-interpreter dependant, as there
; doesn't seem to be any differences in this scheme version (scm)

; My guess is that, just as in 1.5, if the cond statement is implemented in an
; applicative-order fashion, the call will never terminate

; 1.7
; Passing in really big values in exponential form is fine
(sqrt-iter 1 (* 3.0e9 3.0e9))

; However, if you use regular numbers, the method cracks
(sqrt-iter 1  (* 30000000000 30000000000))

; Passing in small numbers cracks even faster
(sqrt-iter 1 (* .0001 .0001))

; A better approach is using a delta and comparing against that
(new-sqrt-iter 1 0 (* 30000000000 30000000000))
(new-sqrt-iter 1 0 (* .0001 .0001))

; 1.8
; Using the improved good-enough? method
(cbrt-iter 1 0 (* 1000 1000 1000))