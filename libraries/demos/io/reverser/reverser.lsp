;; Solution to the Word Order Reverser Problem
;;
;; File:	reverser.lsp
;; Authors:	Bob Walton (walton@seas.harvard.edu)
;; Date:	Sat Feb  9 19:41:42 EST 2013
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

  (dformat "~A~%" *line*)

  ; Set p to beginning of line and q to end of line.
  ;
  (let ((p 0)
	(q (length *line*)))

    ; Print line substituting for words.
    ;
    (loop while (< p (length *line*)) do
      (cond ((alpha-char-p (char *line* p))
	     (loop while (and (< p (length *line*))
			      (alpha-char-p
				(char *line* p)))
	       do
	       (incf p))
	     (setf q (print-substitute-word q)))
	    (t
	      (format t "~A" (char *line* p))
	      (incf p))))
    (format t "~%")))
