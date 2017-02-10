;;; 6.945 Problem Set 1
;;; 
;;; Manushaqe Muco
;;; manjola@mit.edu

(load "~/6.945/6.945_code/ps01/regexp.scm")


;Problem 1.1
(define (r:* expr)
 ;0 or more copies
   (r:repeat 0 #f expr))

(define (r:+ expr)
 ;1 or more times
   (r:repeat 1 #f expr))

(define r:test-cases
  '(((r:quote "abc") "abc" #t)
    ((r:quote "abc") "xbc" #f)
    ((r:quote "abc") "abx" #f)
    ((r:quote "abc") "ababc" #t)
    ((r:quote ".?{|t+") ".?{|t+" #t) ; test BRE vs ERE quoting
    ((r:quote ".?{|t+") "a{" #f)
    ((r:quote ".?{|t+") "ttt" #f)
    ((r:repeat 0 #f (r:seq (r:alt (r:quote "cat") (r:quote "dog"))
			   (r:quote "qux"))) "dogquxdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 0 #f (r:quote "dog")) (r:quote "qux")) "catdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 0 #f (r:quote "dog")) (r:quote "dogqux")) "catdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 1 #f (r:quote "dog")) (r:quote "dogqux")) "catdogdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 1 #f (r:quote "dog")) (r:quote "dogqux")) "catdogcar" #f)
    ((r:seq (r:bol) (r:quote "abc") (r:eol)) "abc" #t)
    ((r:seq (r:bol) (r:quote "abc") (r:eol)) "aabc" #f)
    ((r:bol) "abc" #t)
    ((r:eol) "abc" #t)
    ((r:seq (r:quote "a") (r:dot) (r:quote "c")) "abc" #t)
    ((r:seq (r:quote "a") (r:dot) (r:quote "c")) "axc" #t)
    ((r:seq (r:quote "a") (r:repeat 0 #f (r:dot)) (r:quote "c")) "ac" #t)
    ((r:seq (r:quote "a") (r:char-from "bc") (r:quote "d")) "abc" #f)
    ((r:seq (r:quote "a") (r:char-from "bc") (r:quote "d")) "acd" #t)
    ((r:seq (r:quote "a") (r:char-from "bcd") (r:quote "e")) "ace" #t)
    ((r:seq (r:quote "a") (r:char-from "-b")) "a-" #t)
    ((r:seq (r:quote "a") (r:char-from "]") (r:quote "b")) "a]b" #t)
    ((r:seq (r:quote "a") (r:char-not-from "bc") (r:quote "d")) "aed" #t)
    ((r:seq (r:quote "a") (r:char-not-from "bc") (r:quote "d")) "abd" #f)
    ((r:seq (r:quote "a") (r:char-not-from "-bc") (r:quote "d")) "a-c" #f)
    ((r:seq (r:eol) (r:quote "b")) "b" #f)
    ((r:seq (r:quote "a") (r:* (r:quote "b")) (r:quote "bc")) "abbbbc" #t)
    ((r:seq (r:quote "a") (r:repeat 4 5 (r:quote "b")) (r:quote "bc")) "abbbbc" #f)
    ((r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c")) (r:quote
							       "d")) "abd" #t)
    ((r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c")) (r:quote
                                                               "d")) "aed" #f)
    ((r:* (r:alt (r:+ (r:quote "a")) (r:quote "b"))) "ab" #t)
    ((r:+ (r:alt (r:+ (r:quote "a")) (r:quote "b"))) "aaaa" #t)
    ((r:* (r:char-not-from "ab")) "cde" #t)
    ((r:seq (r:alt (r:quote "a") (r:quote "b") (r:quote "c") 
		   (r:quote "d") (r:quote "e")) (r:quote "f")) "ef" #t)
    ((r:seq (r:quote "a") (r:* (r:char-from "bc"))
	    (r:* (r:quote "c"))) "abc" #t)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[09]. acatdogdogcats" #t)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[10]. ifacatdogdogs" #f)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[11]. ifacatdogdogsme" #f)
    ))

(define (eval-regexp quoted-expression)
  (eval quoted-expression user-initial-environment))

(define (r:run-tests test-cases grep-proc eval-proc)
  ;; Test the regular expressions for the given test cases, printing
  ;; out cases where they fail, and printing nothing if all tests
  ;; succeed.
  ;; Arguments:
  ;;    test-cases: List of 3-tuple test cases,
  ;;    grep-proc: The procedure to grep with, i.e. r:grep or r:egrep
  ;;    eval-proc: The procedure to evaluate and compile the quoted
  ;;    Scheme regular expression in the test case.
  ;;
  (define temp-filename "tmptests.txt")
  
  (define (write-to-file filename string)
    (let ((outport (open-output-file filename)))
      (display string outport)
      (newline outport)
      (close-output-port outport)))

  (define (run-test test-case)
    (let ((test-string (cadr test-case))
	  (test-query (car test-case)))
      (begin
	(write-to-file temp-filename test-string)
	(grep-proc (eval-proc test-query) temp-filename))))

  (define (test-case-correct? result test-case)
    (let ((expected-result (caddr test-case)))
      (if (not expected-result)
	  (not result)
	  (not (not result)))))

  (define (check-test-case test-case)
    (let ((result (run-test test-case)))
      (if (test-case-correct? result test-case)
	  #t
	  (begin
	    (display "******************* Test failed *****************")
	    (newline)
	    (display "Case: ") (display test-case)
	    (newline)
	    #f))))

  (define (all lst)
    (reduce (lambda (x y) (and x y)) #t lst))
  
  (all (map check-test-case test-cases)))

;#| Tests

(r:run-tests r:test-cases r:grep eval-regexp)
;Value: #t

