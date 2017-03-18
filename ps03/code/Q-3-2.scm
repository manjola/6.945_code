;;;Manushaqe Muco
;;;manjola@mit.edu

;;PROBLEM 3.2

;;; Change +-like as instructed.

;;; Change symbolic-extender to:
(pp symbolic-extender)
(named-lambda (symbolic-extender base-arithmetic)
  (make-arithmetic
   'symbolic
   symbolic?
   (list base-arithmetic)

   (lambda (name base-constant)
     (default-object)) ;;;***HERE (additive-identity and multiplicative identity not defined)

   (let ((base-predicate (arithmetic-domain-predicate base-arithmetic)))
     (lambda (operator base-operation)
       (make-operation
        operator
        (any-arg (operator-arity operator) symbolic? base-predicate)
        (lambda args
          (cons operator args)))))))

;;;Now run:
(let ((g (make-generic-arithmetic simple-generic-dispatcher)))
  (add-to-generic-arithmetic! g
    (symbolic-extender numeric-arithmetic))
 (install-arithmetic! g))
;;;and get the following error:

;No identity for this arithmetic: additive-identity
;To continue, call RESTART with an option number:
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

;;WHAT IS HAPPENING?
;In the changed implementation for +-like, case (0) will return the result of the function get-identity, not the function itself. 
;In the case of defining symbolic-extender without additive or multiplicative identities (which +-like is mapped to), if there are no arguments,
;it will call default-object, for which get-identity gives the error above. 

;The new implementation of +-like calls the result of get-identity each time, not only when it is called with zero args. As we try to install the arithmetic,
;we see that it gives an error. In the original implementation, it returns a function, and no error, unless we call it with zero args, say (+). 

;If we defined an additive identity, (+) would return that additive identity instead. 

