;;; Manushaqe Muco
;;; manjola@mit.edu

;(cd "~/6.945/ps02/code/")
(load "load")

;Problem 2.1
(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
    (lambda (name)
      (case name
	((additive-identity) #f)
	((multiplicative-identity) #t) 
	(else (default-object))))
    (lambda (operator)
      (let ((procedure
	     (case operator
	       ((+) (lambda (x . y) 
		      (let ((answer (or x y)))
			(if (list? answer) (car answer) answer))))
	       ((*) (lambda (x . y) 
		      (let ((answer (and x y)))
			(if (list? answer) (car answer) answer))))
	       ((-) (lambda (x y) (not y)))
	       ((negate) (lambda (x) (not x)))
	       (else (lambda x (error "Operator not defined"))))))
	(and procedure 
	     (simple-operation operator boolean? procedure))))))
 
(install-arithmetic! boolean-arithmetic)

;;TESTS

(+ #t)
;#t

(+ #t #f #f)
;#t

(+ #t #t #t)
;#t

(* #t #t)
;#t

(* #t #f)
;#f

(- #t)
;#f

(- #f)
;#t


