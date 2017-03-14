;;; MANUSHAQE MUCO
;;; manjola@mit.edu

;(cd "~/6.945/ps02/code/") 
(load "load")

;;PROBLEM 2 (vector-extender-version)

;;ANSWER to 2.2b
;;When is installing from base-arithmetic better?
;;If base-arithmetic is more extensive than user-environment. 
;;Also, we no longer have the downside of "breaking" the environment,
;;once installed vector-extender. (+ 3 2) still works. 

;;Code is shorter, too?

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
	(fold-left + 0 list-vecs)))))

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

(define (ensure-vector-lengths-match vecs) ;BUG
 (let ((first-vec-length (vector-length (car vecs))))
  (if (any (lambda (v)
	     (not (n:= (vector-length v)
		     first-vec-length )))
	   vecs)
 (error " Vector dimension mismatch :" vecs))))

;get procedure from base arithmetic
(define (get-procedure-from-base op base)
  (operation-procedure
   (arithmetic-operation op base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;INSTALLING OPERATIONS FROM BASE

(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
   (lambda (name base-constant)
     (default-object))
   (let ((+ (get-procedure-from-base '+ base-arithmetic))
	 (- (get-procedure-from-base '- base-arithmetic))
	 (* (get-procedure-from-base '* base-arithmetic))
	 (negate (get-procedure-from-base 'negate base-arithmetic))
	 (sqrt (get-procedure-from-base 'sqrt base-arithmetic)))
     (let ((sp (scalar-product-maker *))
           (vneg (vector-element-wise negate))
	   (v+ (vector-element-wise +))
	   (v- (vector-element-wise -))
	   (vm (vector-magnitude-maker + * sqrt))
	   (dp (dot-product-maker + *)))
       (lambda (operator base-operation)
	(let ((procedure
	       (case operator
		 ((+) v+) 
		 ((-) v-) 
		 ((magnitude) vm)
		 ((*) 
		  (lambda (x y)
		    (cond ((and (vector? x) (vector? y)) 
			    (dp x y))
			   ((and (vector? x) (not (vector? y)))
			    (sp y x))
			   ((and (vector? y) (not (vector? x)))
			    (sp x y))
			   (else
			    (error "unknown types" x y)))))
		 ((negate) vneg)
		 (else 
		  (lambda (x . y) "not accounted for")))))
	  (and procedure 
		(make-operation operator
				(any-arg (operator-arity operator)
					 vector?
					 (arithmetic-domain-predicate 
					  base-arithmetic))
				procedure))))))))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender numeric-arithmetic))

(define vector-extended-combined-arithmetic
  (extend-arithmetic vector-extender combined-arithmetic))

(install-arithmetic! vector-extended-combined-arithmetic)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TESTS

(+ #(1 2 a) #(b 2 0) #(1 2 3))
;Value: #((+ (+ 1 b) 1) 6 (+ (+ a 0) 3))

(- #(1 2 a) #(b 2 0) #(1 2 3))
;Value: #((- (- 1 b) 1) -2 (- (- a 0) 3))

(+ #(0 1 2))
;Value: #(0 1 2)

;(- #(0 1 2)) 
;Value 23: #(0 -1 -2)

(- #(2 a b 3)) 
;Value 31: #(-2 (negate a) (negate b) -3)

(* #(0 3 4) #(1 2 3)) 
;Value: 18

(* #(0 3 b) #(c 2 a))  
;Value: (+ (+ (+ 0 (* 0 c)) 6) (* b a))

(magnitude #(a b c d)) 
;Value: (sqrt (+ (+ (+ (+ 0 (* a a)) (* b b)) (* c c)) (* d d)))

(magnitude #(1 2 4 5)) 
;Value: 6.782329983125268

(* 3 #(0 1 2))
;Value: #(0 3 6)

(* #(0 1 a) 3)
;Value: #(0 3 (* 3 a))

(* 'a #(0 1 2))
;Value: #((* a 0) (* a 1) (* a 2))

(* #(0 1 3) 'a)
;Value: #((* a 0) (* a 1) (* a 3))

(* #(1 2 b) 'a)
;Value: #((* a 1) (* a 2) (* a b))
|#
