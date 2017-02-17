;;; 6.945 Problem Set 1
;;; Manushaqe Muco
;;; manjola@mit.edu

(load "regexp.scm")


;;; Problem 1.1
(define (r:* expr)
 ;0 or more copies
   (r:repeat 0 #f expr))

(define (r:+ expr)
 ;1 or more times
   (r:repeat 1 #f expr))


;;; Problem 1.2
;a) If we try to evaluate (r:repeat 0 1 expr), we'll be stuck in a loop. 
;   We'll need to evaluate (r:repeat 0 1 expr) to evaluate (r:repeat 0 1 expr)

;b) Advantages:
;   (1) Code is more clear and modular. r:? can be reused elsewhere. 
;   (2) The regular expression produced will be shorter. We don't want a regular expression so big 
;   that the shell cannot evaluate it, for big max or min values. 

;c) Ben's proposal advantages:
;   (1) Shorter expression. For Bonnie's case your epression gets longer each time you have to match a character more than 1 times. 
;   In Ben's case it's more concise in specifying how many times you need to match the given character and shorter too. 
;   (2) Uses built-in machinery for BREs (grep utility built-ins), so it will run faster.
;   (3) Because it is used in BRE, it will be available for both BRE and ERE systems. ? could be available only to EREs. 

(define (r:repeat min max expr)
	(apply r:seq (list expr "\\{" (if min (number->string min) "") "," 		
			 (if max (number->string max) "") "\\}"))) 


;;; Problem 1.3 (gist of it)
;;; Reduce nesting indroduced by r:alt, r:repeat, and r:seq. Make r:seq just append string (we can afford to do that, because we don't need to differentiate
;;; unless it's alt or repeat), and "group"/"use parenthesis" at r:alt & r:repeat. Check if expression has been grouped before re-grouping again. 

;;; One intermediate expression to use is lists to differentiate if it's a seq, quote-string, char-from, etc. The tags can then be used to group or check if 
;;; an expression has been regrouped. 


;;; Problem 1.4
;;; r:backref -> expression that was captured with r:alt, r:repeat, r:*, r:+
(define (r:backref number)
	(r:seq "\\\\" (number->string number)))


;;; Problem 1.5 (gist of it)
;;; a. EREs extend on some BREs, and have different quotations. 
;;; ERE: ?, +, | for alt
;;; BRE: special meaning (except .) if preceded by \
;;; ERE: special meaning unless preceded by \ 
;;; 
;;; b. Layer 1: List as intermediate representation of expressions you want to have, grouped as needed. 
;;;    Layer 2: Evaluation step (BRE or ERE) that makes sure the expressions are read correctly, accounting for special quotations. 


