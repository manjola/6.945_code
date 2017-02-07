;;Author: Manushaqe Muco 
;;January 2017
;(Only problems 1 - 5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;PROBLEM 1

;(modulo 13 8) ;5
;(remainder 13 8) ;5
;(modulo -13 8) ; 3
;(remainder -13 8) ;-5
;(modulo -13 -8) ;-5
;(remainder -13 -8) ;-5

;;Difference between "remainder" and "modulo".
; -13 = 8*a + b 
;remainder carries the normal division with n=8, giving us back what remains. In our case, -13 = 8(-1) + (-5)
;modulo gives us b in the range [0,n-1] if n>0 or [n-1, 0] if n<0. In our case [0,7], -13 = 8*(-2) + 3 
;modulo is best for our purposes. 

(define +mod
	(lambda (a b n) (modulo (+ a b) n)))

(define -mod
	(lambda (a b n) (modulo (- a b) n)))

(define *mod
	(lambda (a b n) (modulo (* a b) n)))

;(+mod 7 5 8)     ; -> 4
;(+mod 10 10 3)   ; -> 2
;(-mod 5 12 2)    ; -> 1
;(*mod 6 6 9)     ; -> 0
;(+mod 99 99 100) ; -> 98
;(*mod 50 -3 100) ; -> 50


(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus))))

;((modular 17 +) 13 11)   ; -> 7
;((modular 17 -) 13 11)   ; -> 2
;((modular 17 *) 13 11)   ; -> 7

;;PROBLEM 2

;slow-exptmod: linear recursive algorithm
;Order of growth in time: O(n)
;order of growth in space: O(n)

;exptomod: recursive algorithm
;Order of growth in time: O(log n)
;order of growth in space: O(log n)

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
     (mod* x x))
    (define (em base exponent)
      (cond ((= exponent 0) 1)
           ((even? exponent) (square (em base (/ exponent 2))))
	   (else (mod* base (em base (- exponent 1))))))
    em))

;((exptmod 10) 2 0)   ; -> 1
;((exptmod 10) 2 3)   ; -> 8
;((exptmod 10) 3 4)   ; -> 1
;((exptmod 100) 2 15) ; -> 68
;((exptmod 100) -5 3) ; -> 75

;;PROBLEM 3

(define (random-k-digit-number k)
 (if (= k 1) (random 10)
     (+ (* 10 (random-k-digit-number (- k 1))) (random 10))))

;(random-k-digit-number 1)  ; -> 5       (1 digit)
;(random-k-digit-number 3)  ; -> 980      (1-3 digits)
;(random-k-digit-number 3)  ; -> 513      (is it different?)
;(random-k-digit-number 50) ; -> 85583720100999182912992292931534079584236803744659


(define (count-digits n)
 (let ((quot (/ n 10))) 
   (if (< quot 1) 1
       (+ 1 (count-digits quot)))))

;(count-digits 3)         ; -> 1
;(count-digits 2007)      ; -> 4
;(count-digits 123456789) ; -> 9


(define (big-random n)
  (let ((result (random-k-digit-number (count-digits n))))
    (if (< result n) result
	(big-random n))))

;(big-random 100)          ; -> 9  (1-2 digit number)
;(big-random 100)          ; -> 68  (is it different?)
;(big-random 1)            ; -> 0
;(big-random 1)            ; -> 0     (should be always 0)
;(big-random (expt 10 40)) ; -> 973457158013760053934708467547325560780

;;PROBLEM 4

;slow-prime?: iterative algorithm
;order of growth in time: O(n)
;order of growth in space: O(1)

;optimization 1: order of growth in time becomes O(sqrt n)
;optimization 2: will take half as much time, but linear still O(n)

;Fermat Test
(define (test a p)
  (= (modulo a p) ((exptmod p) a p)))

;(test 3 5) -> #t
;(test 3 6) -> #t (fails for composite)

(define prime-test-iterations 20)

(define prime?
  (lambda (p)
    (define (iter i)
      (if (= i prime-test-iterations) #t
	  (let ((a (big-random (- p 1))))
	    (if (test a p) (iter (+ i 1))
			   #f))))
    (if (> p 1) (iter 1)
	#f)))
 
;(prime? 2) ; -> #t
;(prime? 4) ; -> #f
;(prime? 1) ; -> #f
;(prime? 0) ; -> #f
;(prime? 200) ; -> #f
;(prime? 199) ; -> #t

;prime?: iterative algorithm, but makes call to a recursive algorithm, which is exptmod in (test a p)

;order of growth in time: O(m*log n)
;algorithm has a constant number of iterations per se, but it calls on (test a p) which takes O(log n) time because of exptmod. 
;m is the number of itmes prime? calls on (test).

;order of growth in space: O(m*log n). Same reason as above, but space instead of time

;;PROBLEM 5

(define (random-k-digit-prime k)
  (let ((n (random-k-digit-number k)))
    (if (prime? n) n
	(random-k-digit-prime k))))

;;Ways of failing
;1) Generate a non-prime number, since prime? is probabilistic and Fermatr's Little Theorem may introduce false positives. This is rare.
;2) Generate a number with less than k digits.

;(random-k-digit-prime 1) ;5
;(random-k-digit-prime 2)  ;89  
;(random-k-digit-prime 10) ;3095291153
;(count-digits (random-k-digit-prime 100)) ;100  ; Not always 100.
;(count-digits (random-k-digit-prime 100)) ;100

;;PROBLEM 6
(define ax+by=1
  (lambda (a b)
     (let ((q (quotient a b)) (r (remainder a b)))
       (if (= r 1) (list 1 (- q))
           (let ((sol (ax+by=1 b r)))
             (list (cadr sol) (- (car sol) (* q (cadr sol)))))))))
                 
;(ax+by=1 17 13) ; -> (-3 4)    17*-3 + 13*4 = 1
;(ax+by=1 7 3)   ; -> (1 -2)     7*1  + 3*-2 = 1
;(ax+by=1 10 27) ; -> (-8 3)    10*-8 + 3*27 = 1

(define (inversemod n)
  (lambda (e)
    (if (= (gcd e n) 1)
	(let ((result (car (ax+by=1 e n))))
	  (modulo result n))
        (error "a and b must have gcd 1"))))

;((inversemod 11) 5) ; -> 9            5*9 = 45 = 1 (mod 11) 
;((inversemod 11) 9) ; -> 5
;((inversemod 11) 7) ; -> 8            7*8 = 56 = 1 (mod 11)
;((inversemod 12) 5) ; -> 5            5*5 = 25 = 1 (mod 12)
;((inversemod 12) 8) ; -> error        gcd(8,12)=4, so no inverse exists
;((inversemod 101) (random-k-digit-prime 2)) ; -> ? (test your answer with *mod)



