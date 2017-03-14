;;; MANUSHAQE MUCO
;;; manjola@mit.edu

;(cd "~/6.945/ps02/code/") 
(load "vector-extender")

;PROBLEM 2.3
(define (unit-circle x)
 (vector (sin x) (cos x)))

(define combined-arithmetic
 (extend-arithmetic symbolic-extender numeric-arithmetic))
 
(define vec-before-func
  (extend-arithmetic
   function-extender
   (extend-arithmetic vector-extender combined-arithmetic)))

(define func-before-vec
  (extend-arithmetic
   vector-extender
   (extend-arithmetic function-extender combined-arithmetic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(install-arithmetic! vec-before-func)

;((magnitude unit-circle) 'a)
;Value 18: (sqrt (+ (+ 0 (* (sin a) (sin a))) (* (cos a) (cos a))))

;((magnitude (vector sin cos)) 'a)
;Inapplicable operation: * (#[compound-procedure 19] #[compound-procedure 19])
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(install-arithmetic! func-before-vec)

;((magnitude unit-circle) 'a)
;Inapplicable operation: magnitude (#(... ...))
;To continue, call RESTART with an option number:
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

;((magnitude (vector sin cos)) 'a)
;Value 23: (sqrt (+ (+ 0 (* (sin a) (sin a))) (* (cos a) (cos a))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Is it possible to make an arithmetic for which both evaluate correctly?
;;Yes.
;;- Next pset: generic expressions offers a possible way.
;;- A more tedious way is to make operation-union-dispatch so that
;; (if (not operation) <...>) uninstalls one arithmetic, and installs an 
;; applicable one, instead of returning an error. 


