;;; MANUSHAQE MUCO
;;; manjola@mit.edu

;(cd "~/6.945/ps03/code/")
(load "load")

;;DEFINITIONS RELEVANT FOR ALL PROBLEMS IN THIS SECTION

(define (fib n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define trie-full-generic-arithmetic
 (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
   (add-to-generic-arithmetic! g numeric-arithmetic)
   (extend-generic-arithmetic! g function-extender)
   (add-to-generic-arithmetic! g
	 (symbolic-extender numeric-arithmetic))
   g))

;(install-arithmetic! trie-full-generic-arithmetic)

(define full-generic-arithmetic
 (let ((g (make-generic-arithmetic simple-generic-dispatcher)))
   (add-to-generic-arithmetic! g numeric-arithmetic)
   (extend-generic-arithmetic! g function-extender)
   (add-to-generic-arithmetic! g
			       (symbolic-extender numeric-arithmetic))
 g))

;(install-arithmetic! full-generic-arithmetic)

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

;PROBLEM 3.5
;a) No. The edges of the trie are still searched in the order they are added (the latest edges are searched first). 

;b) If the predicate satisfies more than one path in the trie, it will have more than one appropriate handler. (ex: negative & even for -4). 

;c) Not in the generic arithmetic code written before the "Trie-section". In the trie section we have the example of (-4). 

;PROBLEM 3.6
;Identifying equivalent paths, means less paths to compute. The trie will recognize a given-path as other equivalent paths already existing in the trie. 
;Use memoization, so that if we ask for (symbol or number), we return the same object that is already existing in the trie (or add it if it's the first
;time we encounter it, but memoize for future calls). 

;disjoin/conjoin (and disjoin*/conjoin*) return separate predicate objects when called, so we need to memoize when first asked for the predicates. 

;New disjoin and conjoin procedures in predicates.scm
#|
(define disjoin (memoize-multi-arg-equal
		  (lambda predicates
		    (disjoin* predicates))))

(define conjoin (memoize-multi-arg-equal
		  (lambda predicates
		    (conjoin* predicates))))
|#

;TEST
(eqv? (disjoin symbol? number?) (disjoin symbol? number?))
;Value: #t

(eqv? (conjoin symbol? number?) (conjoin symbol? number?))
;Value: #t

;PROBLEM 3.7
;;trie-search performs the SAME as rule-search. This because there's no path overlap for number, symbol, and functions. Searching the trie becomes the same
;;as searching a list of rules. trie-search has advantage when paths overlap. 

(define (test-stormer-counts)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
      (lambda ()
	(x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

;;trie
(test-stormer-counts)
(18 number)
(26 symbolic)
(8 (disjoin any-object function))
(18 function)
(16 (disjoin number symbolic))
;Value: (+ 9.999833334166664e-3 (* (/ (expt h 2) 12) -9.999750002487318e-7))

;;full-generic
(test-stormer-counts)
(8 (disjoin any-object function))
(26 symbolic)
(18 number)
(16 (disjoin number symbolic))
(18 function)
;Value: (+ 9.999833334166664e-3 (* (/ (expt h 2) 12) -9.999750002487318e-7))

;;;FIB
;trie
(with-predicate-counts (lambda () (fib 20)))
(72437 (disjoin any-object function))
(72437 (disjoin number symbolic))
(144873 number)
(144873 symbolic)
(144873 function)
;Value: 6765

;full-generic
(with-predicate-counts (lambda () (fib 20)))
(144873 number)
(72437 (disjoin any-object function))
(144873 symbolic)
(144873 function)
(72437 (disjoin number symbolic))
;Value: 6765