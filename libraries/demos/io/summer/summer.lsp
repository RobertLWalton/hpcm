;; Solution to the Summation Checking Problem
;;
;; File:	summer.lsp
;; Authors:	Bob Walton (walton@seas.harvard.edu)
;; Date:	Mon Feb 11 07:17:52 EST 2013
;;
;; The authors have placed this program in the public
;; domain; they make no warranty and accept no liability
;; for this program.

(defvar *debug* (rest *posix-argv*))

(defun dformat (&rest r)
  (if *debug* (apply #'format t r)))

(defvar *line*)

; Main loop
;
(loop while (setf *line* (read-line nil nil)) do

  ; Print test case name.
  ;
  (format t "~A~%" *line*)

  (let (sum (corrected-sum 0) token)

    ; Read input and compute corrected sum.

    ; Read and sum numbers until non-number.
    ;
    (loop while (numberp (setf token (read))) do
      (incf corrected-sum token))

    ; Skip `='.
    ;
    (assert ( equal token '=))

    ; Read accountant computer's sum.
    ;
    (assert (numberp (setf sum (read))))

    ; Corrected-sum and sum are both approximations
    ; to numbers that are exact multiples of 0.01.
    ; So if they differ, they should differ by at
    ; least 0.01, approximately, and certainly by
    ; more than 0.005, and if they are equal, they
    ; should differ by much, much less than 0.01,
    ; and most certainly by less than 0.005.

    ; If debugging, look at corrected-sum and sum
    ; at maximum precision.
    ;
    (dformat "SUM = ~,16F, CORRECTED SUM = ~,16F\n"
	     sum corrected-sum )

    ; Print output.
    ;
    (cond ((< (abs (- sum corrected-sum )) 0.005 )
	   (format t "~,2F is correct~%" sum ))
	  (t
	   (format t "~,2F should be ~,2F~%"
		     sum corrected-sum )))))
