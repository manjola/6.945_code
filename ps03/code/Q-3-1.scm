;;; MANUSHAQE MUCO
;;; manjola@mit.edu

;(cd "~/6.945/ps03/code/")
(load "vector-extender") ;from pset02

;PROBLEM 3.1
;;;;;;;;;;;;;;;;;
;;a. Would be hard for purely combinator-based arithmetic. Codomain of
;;functon-extender would need to work over functions (self-reference 
;;problem) and vectors. 

;;b. YES. No need to re-map functions; generic works by adding 
;;particular rules to handle function and vector arithmetic when
;;applicable. Order still matters: [FG, SG, VG, N].

 (let ((g (make-generic-arithmetic simple-generic-dispatcher)))
   (add-to-generic-arithmetic! g numeric-arithmetic)
   (extend-generic-arithmetic! g vector-extender)
   (extend-generic-arithmetic! g symbolic-extender)
   (extend-generic-arithmetic! g function-extender)
   (install-arithmetic! g))

(((* 3
     (lambda (x) (lambda (y) (+  x y)))
     (lambda (x) (lambda (y) (vector y x))))
 'a)
4)
;Value 128: (* (* 3 (+ a 4)) #(4 a))

(+ 1 'a 3)
;Value 19: (+ (+ 1 a) 3)

(+ 'a ((+ cos sin) 3))
;Value 20: (+ a -.8488724885405782)

(+ 'a ((+ 'c cos sin) (* 2 'b)))
;Value 22: (+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))

(((+ (lambda (x) (lambda (y) (cons x y)))
     (lambda (x) (lambda (y) (cons y x))))
 3)
4)
;Value 23: (+ (3 . 4) (4 . 3))

(+ 'a ((+ cos sin) 'b))
;Value 24: (+ a (+ (cos b) (sin b)))

(define (unit-circle x)
 (vector (sin x) (cos x)))

((magnitude unit-circle) 'a)
;Value: (sqrt (+ (+ 0 (* (sin a) (sin a))) (* (cos a) (cos a))))

((magnitude (vector sin cos)) 'a)
;Value: (sqrt (+ (+ 0 (* (sin a) (sin a))) (* (cos a) (cos a))))

;;c. 
((vector cos sin) 3)
;The object #(#[compound-procedure 16] #[compound-procedure 17]) is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify a procedure to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

;;It's difficult because we need a way to apply compound-procedures 
;;16 & 17, and then the operation vector, just like it works for
;;(+ sin cos). We need vector to be an operation for the arithmetic, the
;;same way + is. A way to go about this is to use the error call, meaning
;;modify the code where the error is called so it instead applies another
;;"special" procedure that in cases like (vector cos sin) calls a 
;;special-vector-procedure that works like + (in %arithmetic-operator-alist).
;;We'd have to add this for every special procedure like vector, though. 