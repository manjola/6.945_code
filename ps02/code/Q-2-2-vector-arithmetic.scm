;;; MANUSHAQE MUCO
;;; manjola@mit.edu

;(cd "~/6.945/ps02/code/")
(load "load")

;PROBLEM 2.2 (vector-arithmetic version)

;ANSWER TO 2.2b
;;When is it better to get operation from user-environment?
;;If base-arithmetic is more restricted than the user-environment. For example, imagine the user-environment being able to handle + 
;;for both symbols and numerals, while the base-arithmetic for vectors is numeric-arithmetic. Now, our vector arithmetic 
;;will only be able to handle numerals, not symbols if it uses the +operation from the base, instead of the one from the environment. 

;;Downsides: Breaks the mapping of the environment. Once we installed vector-arithmetic, (+ 3 2) no longer works. 

;;DEFINITIONS INDEPENDENT OF WHICH IMPLEMENTION WE USE
(register-predicate! vector? 'vector)

(define (vector-element-wise element-procedure) 
  (lambda vecs 
    (ensure-vector-lengths-match vecs) 
    (apply vector-map element-procedure vecs)))

(define (dot-product-maker + *)
  (lambda vecs
    (let ((multiplied-vecs (apply vector-map * vecs)))
      (let ((list-vecs (vector->list multiplied-vecs)))
	(apply + list-vecs)))))

(define (scalar-scalar *)
  (lambda args
    (apply * args)))

(define (negate-elt -)
  (lambda args
    (apply - args)))
  
(define (scalar-product-maker *)
  (define (scalar-product scalar vec)
    (vector-map (lambda (x) (* scalar x))
		 vec)) 
  scalar-product)

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *) ))
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

(define (ensure-vector-lengths-match vecs) 
 (let ((first-vec-length (vector-length (car vecs))))
  (if (any (lambda (v)
	     (not (n:= (vector-length v)
		     first-vec-length )))
	   vecs)
 (error " Vector dimension mismatch :" vecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INHERETING OPERATIONS FROM ENVIRONMENT IMPLEMENTATION

;CREATE ENVIRONMENT
(install-arithmetic!
 (extend-arithmetic symbolic-extender numeric-arithmetic))

(define vector-arithmetic
  (make-arithmetic 'vector vector? '()
   (lambda (name)
     (default-object))
   (let ((sp (scalar-product-maker *))
	 (ss (scalar-scalar *))
	 (negate (negate-elt -))
	 (v+ (vector-element-wise +))
	 (v- (vector-element-wise -))
	 (vm (vector-magnitude-maker + * sqrt))
	 (dp (dot-product-maker + *)))
     (lambda (operator)
       (let ((procedure
	      (case operator
		((+) v+) 
		((-) v-) 
		((magnitude) vm)
		((*) 
		 (lambda (x y)
		   ;(pp (list x y))
		   (cond ((and (vector? x) (vector? y)) 
			  (dp x y))
			 ((and (vector? x)
			       (or (number? y) (symbol? y)))
			  (sp y x))
			 ((and (vector? y)
			       (or (number? x) (symbol? x)))
			  (sp x y))
			 ((and (or (number? x) (symbol? x))
			       (or (number? y) (symbol? y)))
			  (ss x y))
			 (else
			  (error "unknown types" x y)))))
		((negate) v-)
		(else 
		 (lambda (x . y)
		   "not accounted for")))))
	 (and procedure 
	      (make-operation operator
				(any-arg (operator-arity operator)
					 vector?
					(or number?
					    symbol?))
				procedure)))))))

(install-arithmetic! vector-arithmetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TESTS

(+ #(1 2 a) #(b 2 0) #(1 2 3))
;Value 15: #((+ (+ 1 b) 1) 6 (+ (+ a 0) 3))

(- #(1 2 a) #(b 2 0) #(1 2 3))
;Value 16: #((- (- 1 b) 1) -2 (- (- a 0) 3))

(+ #(0 1 2))
;Value 17: #(0 1 2)

(- #(0 1 2))
;Value 23: #(0 -1 -2)

(- #(2 a b 3))
;Value 31: #(-2 (negate a) (negate b) -3)

(* #(0 3 4) #(1 2 3))
;Value: 18

(* #(0 3 b) #(c 2 a))
;Value 14: (+ (+ (* 0 c) 6) (* b a))

(magnitude #(a b c d))
;Value 20: (sqrt (+ (+ (+ (* a a) (* b b)) (* c c)) (* d d)))

(magnitude #(1 2 4 5))
;Value: 6.782329983125268

(* 3 #(0 1 2))
;Value 96: #(0 3 6)

(* #(0 1 a) 3)
;Value 97: #(0 3 (* 3 a))

(* 'a #(0 1 2))
;Value 128: #((* a 0) (* a 1) (* a 2))

(* #(0 1 3) 'a)
;Value 129: #((* a 0) (* a 1) (* a 3))

(* #(1 2 b) 'a)
;Value 130: #((* a 1) (* a 2) (* a b))

;(+)
;No identity for this arithmetic: additive-identity
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n
;Because we return default-object if zero args given, which gives an error. 
