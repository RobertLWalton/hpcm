;; Solution to the Summation Checking Problem
;;
;; File:	summer.lsp
;; Authors:	Bob Walton (walton@seas.harvard.edu)
;; Date:	Sat Feb  9 19:45:57 EST 2013
;;
;; The authors have placed this program in the public
;; domain; they make no warranty and accept no liability
;; for this program.

(defvar *debug* (rest *posix-argv*))

(defun dformat (&rest r)
  (if *debug* (apply #'format t r)))

(defvar *line*)

; Function to find and print the first word BEFORE q,
; where q indexes a character in *line*.  It is a
; program error if there is no such word.  Returns
; index of the first character of the word printed.
;
(defun print-substitute-word ( q )
 
  ; Move q backward to point just after word.
  ;
  (do () ((progn (assert (> q 0))
		 (alpha-char-p
		   (char *line* (- q 1)))))
    (decf q))

  (let ((p q))

    ; Move q to point at 1st character of word.
    ;
    (decf q)
    (do () ((or (<= q 0)
		(not (alpha-char-p
		       (char *line* (- q 1))))))
      (decf q))

    ; Print word and return.
    ;
    (format t "~A" (subseq *line* q p))
    q ))

; Main loop
;
(loop while (setf *line* (read-line nil t)) do

  ; Print test case name.
  ;
  (format t "~A~%" *line*)

  (let (sum (corrected-sum 0) token)

    ; Read input and compute corrected sum.

    ; Read and sum numbers until non-number.
    ;
    (loop while (numberp (setf token (read nil t))) do
      (incf corrected-sum token))

    ; Skip `='.
    ;
    (assert ( equal token '=))

    ; Read accountant computer's sum.
    ;
    (assert (numberp (setf sum (read nil t))))

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
    (dformat "SUM = ~,16f, CORRECTED SUM = ~,16f\n"
	     sum corrected-sum )

    ; Print output.
    ;
    (cond ((< (abs (- sum corrected-sum )) 0.005 )
	   (format t "~,2f is correct~%" sum ))
	  (t
	   (format t "~,2f should be ~,2f~%"
		     sum corrected-sum )))))
