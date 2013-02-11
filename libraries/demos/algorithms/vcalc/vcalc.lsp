;; Solution to the Simple Vector Calculator Problem
;;
;; File:	vcalc.lsp
;; Authors:	Bob Walton (walton@seas.harvard.edu)
;; Date:	Sun Feb 10 18:40:31 EST 2013
;;
;; The authors have placed this program in the public
;; domain; they make no warranty and accept no liability
;; for this program.

(defvar *debug* (rest *posix-argv*))

(defun dformat (&rest r)
  (if *debug* (apply #'format t r)))

(defvar *line*)

; Given a floating point number return that converted
; by the ~,14f format BUT with trailing fraction zeros
; suppressed, and for integers, the `.' suppressed.
;
(defun scalar-string (number)
  (let ((result (format nil "~,14f" number)))
    (do ((p (- (length result) 1) (- p 1)))
        ((or (= p 0) (char/= (char result p) #\0))
	 (progn
	   (if (char= (char result p) #\.) (decf p 1))
	   (subseq result 0 p))))))

; Given a string, return the number it represents, or
; nil if it does not represent a number.
;
(defun string-scalar (string)
  (multiple-value-bind (r length)
                       (read-from-string string nil)
    (if (=> length (length string)) r)))

; Returns next token or *EOL* if end of line or *EOF* if
; end of file.  We make *EOL* and *EOF* be printable
; strings as tokens are printed in many error messages.
; Implements one-token backup.  Implements
; *line-number*.
;
; All tokens are non-zero length strings or numbers.
;
(defvar *EOL* "END-OF-LINE")
(defvar *EOF* "END-OF-FILE")
;
(defvar *backup* nil)
    ; Set backup = t to backup.
(defvar *line-number* 0)
    ; Current line number; 1, 2, 3, ...
;
(defvar *last-token* *EOL*)
;
(defvar *NUL* (code-char 0))
    ; Used internally to represent end of file.
; If character is separator return associated
; string, else return nil.
;
(defun is-separator ( c )
  (cond ((char= c #\() "(")
        ((char= c #\)) ")")
        ((char= c #\,) ",")
        ((char= c #\:) ":")
        ((char= c #\Newline) *EOL*)
        ((char= c *NUL*) *EOF*)))
;
(defun get-token ()
  (cond (*backup*
	  (setf *backup* nil)
	  (return-from get-token *last-token*))
	((string= *last-token* *EOL*)
	 (loop
	   do (incf *line-number*)
	   (let ((c (peek-char t nil t #\Space)))
	     (cond ((char= c #\/)
	            (read-char)
		    (setf c (peek-char nil nil
				       t *NUL*))
		    (if (char/= c #\/)
		      (error "line begins with `/'"
			     " not followed by a `/'"))
		    (loop until
			  (char= (read-char nil
					    t #\Newline)
			         #\Newline)))
		   (t (return)))))))

  (let* ((v (is-separator (peek-char t nil t *NUL*))))
    (if v (setf *last-token* v)
      (loop with s = (make-array 
		       '(200)
		       :element-type 'string-char
		       :fill-pointer 0
		       :adjustable t)
	    for c = (peek-char nil nil t *NUL*)
	    until (is-separator c)
	    until (char= c #\Space)
	    while (graphic-char-p c)
	    do
	    (vector-push-extend c s)
	    finally
	    (setf *last-token*
		  (coerce (adjust-array s
			    (list (fill-pointer s)))
			  'string)))))

  (dformat "{~A}" *last-token*)
  *last-token*)

; Print error message made by concatenating ~A printouts
; of arguments without intervening space, and exit.
;
(shadow 'error)
(defun error (&rest args)
  (format t "ERROR in line ~A:~%      " *line-number*)
  (dolist (x args) (format t "~A" x))
  (format t "~%")
  (quit nil 1))

(defun check-not-eof (token)
  (if (equal token *EOF*)
      (error "unexpected end of file")))

(defun skip (desired)
  (let ((token (get-token)))
    (if (not (equal token desired))
	(error "expected `" desired 
	       "' but found `" token "'"))))

(defun get-number ()
  (let* ((token (get-token))
	 (number (string-scalar token)))
    (if (not number)
      (error "expected number and got `" string "'"))
    number))

(defun is-variable (token)
  (and (stringp token)
       (alpha-char-p (char token 0))))

(defun check-variable (token)
   (if (not (is-variable token))
      (error "`" token "' is not a variable")))

(shadow 'vector)
(defstruct vector x y)

(defun vector-string (v)
  (format nil "(~A, ~A)"
	  (scalar-string (vector-x v))
	  (scalar-string (vector-y v))))

(defun vector-negate (v)
  (make-vector :x (- (vector-x v))
	       :y (- (vector-y v))))
(defun vector-add (v1 v2)
  (make-vector :x (+ (vector-x v1) (vector-x v2))
	       :y (+ (vector-y v1) (vector-y v2))))
(defun vector-subtract (v1 v2)
  (make-vector :x (- (vector-x v1) (vector-x v2))
	       :y (- (vector-y v1) (vector-y v2))))
(defun vector-multiply (v1 v2)
  (+ (* (vector-x v1) (vector-x v2))
     (* (vector-y v1) (vector-y v2))))
(defun scalar-multiply (s v)
  (make-vector :x (* s (vector-x v))
	       :y (* s (vector-y v))))
(defun vector-length (v)
  (sqrt (vector-multiply v v)))

(defun vector-angle (v)
  ; We take extra care with angles that are
  ; multiples of 90 degrees.  This is only
  ; necessary if one is using exact equality
  ; with integer coordinates instead of
  ; approximate equality.
  ;
  (cond ((and (= 0 (vector-x v))
	      (= 0 (vector-y v)))
	 (error "angle of zero vector"))
	((= 0 (vector-x v))
	 (if (> (vector-y v) 0) +90 -90))
	((= 0 (vector-y v))
	 (if (> (vector-x v) 0) 0 +180))
	(t
	  (* (/ 180.0 PI) (atan (vector-y v)
			        (vector-x v))))))

(defun vector-rotate (v angle) 
  (let* ((k (floor angle 90))
	 (j (mod k 4))
	 sin cos)
    ; We take extra care with angles that are
    ; multiples of 90 degrees.  This is only
    ; necessary if one is using exact equality
    ; with integer coordinates instead of
    ; approximate equality.
    ;
    (cond ((/= angle (* 90 k))
	   (setf angle (* (/ PI 180.0) angle))
	   (setf sin (sin angle))
	   (setf cos (cos angle)))
	  ((= j 0) (setf sin 0 cos 1))
	  ((= j 1) (setf sin 1 cos 0))
	  ((= j 2) (setf sin 0 cos -1))
	  ((= j 3) (setf sin -1 cos 0)))
    (make-vector :x (- (* cos (vector-x v))
		       (* sin (vector-y v)))
		 :y (+ (* sin (vector-x v))
		       (* cos (vector-y v))))))


(defconstant *BOOLEAN* 1)
(defconstant *SCALAR*  2)
(defconstant *VECTOR*  3)

(defstruct value
  type	; *BOOLEAN*, *SCALAR*, or *VECTOR*.
  b	; Value if *BOOLEAN*.
  s	; Value if *SCALAR*.
  v	; Value if *VECTOR*.
  )

(defun value-string (v)
  (cond ((= *BOOLEAN* (value-type v))
	 (if (value-b v) "true" "false"))
	((= *SCALAR* (value-type v))
	 (scalar-string (value-s v)))
	((= *VECTOR* (value-type v))
	 (vector-string (value-v v)))
	(t
	  (error "bad value type "
		 (format nil "~A" (value-type v))))))

(defvar *variable-table*
  (make-hash-table :test #'equal :size 256))
    ; Maps variable name strings to values.

(defun require-boolean (v1)
  (if (/= (value-type v1) *BOOLEAN*)
    (error "operand should be boolean")))

(defun require-boolean (v1 v2)
  (cond ((/= (value-type v1) *BOOLEAN*)
	 (error "first operand should be boolean"))
        ((/= (value-type v2) *BOOLEAN*)
	 (error "second operand should be boolean"))))

(defun require-scalar (v1)
  (if (/= (value-type v1) *SCALAR*)
    (error "operand should be scalar")))

(defun require-scalar (v1 v2)
  (cond ((/= (value-type v1) *SCALAR*)
	 (error "first operand should be scalar"))
        ((/= (value-type v2) *SCALAR*)
	 (error "second operand should be scalar"))))

(defun require-vector (v1)
  (if (/= (value-type v1) *VECTOR*)
    (error "operand should be vector")))

(defun require-vector (v1 v2)
  (cond ((/= (value-type v1) *VECTOR*)
	 (error "first operand should be vector"))
        ((/= (value-type v2) *VECTOR*)
	 (error "second operand should be vector"))))

(defun require-scalar-vector (v1 v2)
  (cond ((/= (value-type v1) *SCALAR*)
	 (error "first operand should be scalar"))
        ((/= (value-type v2) *VECTOR*)
	 (error "second operand should be vector"))))

(defun require-vector-scalar (v1 v2)
  (cond ((/= (value-type v1) *VECTOR*)
	 (error "first operand should be vector"))
        ((/= (value-type v2) *SCALAR*)
	 (error "second operand should be scalar"))))
 
; Require v1 to be SCALAR or VECTOR and return
; true if SCALAR, false if VECTOR.  If v2 given,
; require it to be of the same type as v1.
;
(defun is-scalar (v1 &optional v2)
  (cond ((and v2 (/= (value-type v1) (value-type v2)))
	 (error "operands should both be scalar"
	        " or both be vector"))
	((= (value-type v1) *SCALAR*) t)
	((= (value-type v1) *VECTOR*) nil)
	(t
	 (error "operand(s) should be"
		" scalar or vector"))))
 

; Reads a constant value and returns its value,
; or a variable with a value and returns a COPY
; of its value.  Always returns a NEW value.
;
(defun get-value ()
  (let ((v (make-value))
	(token (get-token))
	x y n)
    (cond ((equal token "(")
	   (setf (value-type v) *VECTOR*)
	   (setf (value-v v) (make-vector))
	   (setf (vector-x (value-v v)) (get-number))
	   (skip ",")
	   (setf (vector-y (value-v v)) (get-number))
	   (skip ")"))
	  ((equal token "true")
	   (setf (value-type v) *BOOLEAN*)
	   (setf (value-b v) t))
	  ((equal token "false")
	   (setf (value-type v) *BOOLEAN*)
	   (setf (value-b v) nil))
          ((is-variable token)
	   (let ((v2 (gethash token *variable-table*)))
	     (if (equal v2 nil)
	         (error "`" token "' unassigned"))
	     (setf (value-type v) (value-type v2)
	           (value-b    v) (value-b    v2)
	           (value-s    v) (value-s    v2)
	           (value-v    v) (value-b    v2))))
	  ((setf n (string-scalar token))
	   (setf (value-type v) *SCALAR*)
	   (setf (value-s v) n))
	  (t
	   (error "expected true, false, "
		  " scalar constant, "
		  " vector constant, "
		  " or variable but got `"
		  token "'")))
    (dformat "[~A]" (value-string v))
    v))

; Execute `clear ...' statement after `clear' token
; has been read and skipped.
;
(defun execute-clear ()
  (let ((token (get-token)))
    (cond ((equal token *EOL*)
	   (maphash #'(lambda (key val)
			(remhash key *variable-table*))
		    *variable-table*))
	  (t
	    (loop until (equal token *EOL*) do
		  (check-variable token)
		  (remhash token *variable-table*)
		  (setf token (get-token)))))))

; Execute `print{ln} ...' statement after
; `print{ln}' token has been read and skipped,
; BUT do not output final space or line end.
;
(defun execute-print ()
  (loop for token = (get-token)
	for first = t then nil
	with v
	until (equal token *EOL*)
	do
	(if first (setf first nil)
	          (format t " "))
	(setf v (gethash token *variable-table*))
	(format t (if v (value-string v)
	                (token-string v)))))

; Execute `variable = ...' statement after
; `variable =' tokens have been read and skipped.
; Check variable token to be sure its a variable
; name.
;
(defun execute-assign (variable)
  (check-variable variable)
  (if (or (equal variable "true")
	  (equal variable "false"))
      (error "attempt to assign a value to `"
	     variable "'"))
  (let ((op (get-token)) v1 v2)
    ; v1 is both the first value read and the final
    ; resulting value.

    ; Read and process statement up to but not
    ; including *EOL*.
    ;
    (cond ((equal op "-")
	   (setf v1 (get-value))
	   (if (is-scalar v1)
	       (setf (value-s v1)
		     (- (value-s v1)))
	       (setf (value-v v1)
		     (vector-negate (value-v v1)))))
	  ((equal op "|")
	   (setf v1 (get-value))
	   (require-scalar v1)
	   (skip "|")
	   (setf (value-s v1) (abs (value-s v1))))
	  ((equal op "||")
	   (setf v1 (get-value))
	   (require-vector v1)
	   (skip "||")
           (setf (value-s v1)
		 (vector-length (value-v v1)))
           (setf (value-type v1) *SCALAR*))
	  ((equal op "angle")
	   (setf v1 (get-value))
	   (require-vector v1)
           (setf (value-s v1)
		 (vector-angle (value-v v1)))
           (setf (value-type v1) *SCALAR*))
	  ((equal op "!")
	   (setf v1 (get-value))
	   (require-boolean v1)
	   (setf (value-b v1) (not (value-b v1))))
	  (t
	   ; Case where there is either a binary
	   ; operator or no operator.
	   ;
	   (setf *backup* t)
	   (setf v1 (get-value))
	   (setf op (get-token))
	   (cond ((equal op "+")
		  (setf v2 (get-value))
		  (if (is-scalar v1 v2)
		    (setf (value-s v1)
			  (+ (value-s v1)
			     (value-s v2)))
		    (setf (value-v v1)
			  (vector-add
			    (value-v v1)
			    (value-v v2)))))
	         ((equal op "-")
		  (setf v2 (get-value))
		  (if (is-scalar v1 v2)
		    (setf (value-s v1)
			  (- (value-s v1)
			     (value-s v2)))
		    (setf (value-v v1)
			  (vector-subtract
			    (value-v v1)
			    (value-v v2)))))
	         ((equal op "*")
		  (setf v2 (get-value))
		  (if (is-scalar v1)
		    (if (is-scalar v2)
		      (setf (value-s v1)
			    (* (value-s v1)
			       (value-s v2)))
		      (setf (value-s v1)
			    (scalar-multiply
			      (value-s v1)
			      (value-v v2))))
		    (progn
		      (require-vector v1 v2)
		      (setf (value-s v1)
			    (vector-multiply
			      (value-v v1)
			      (value-v v2)))
		      (setf (value-type v1)
			    *SCALAR*))))
	         ((equal op "/")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (if (= 0 (value-s v2))
		    (error "zero divisor"))
		  (setf (value-s v1)
			(/ (value-s v1)
			   (value-s v2))))
	         ((equal op "^")
		  (setf v2 (get-value))
		  (require-vector-scalar v1 v2)
		  (setf (value-v v1)
			(vector-rotate (value-v v1)
			               (value-s v2))))
	         ((equal op "||")
		  (setf v2 (get-value))
		  (require-boolean v1 v2)
		  (setf (value-b v1)
			(or (value-b v1)
			    (value-b v2))))
	         ((equal op "&&")
		  (setf v2 (get-value))
		  (require-boolean v1 v2)
		  (setf (value-b v1)
			(and (value-b v1)
			     (value-b v2))))
	         ((equal op "==")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(= (value-s v1)
			   (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
	         ((equal op "!=")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(/= (value-s v1)
			    (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
	         ((equal op "<")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(< (value-s v1)
			   (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
	         ((equal op "<=")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(<= (value-s v1)
			    (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
	         ((equal op ">")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(> (value-s v1)
			   (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
	         ((equal op ">=")
		  (setf v2 (get-value))
		  (require-scalar v1 v2)
		  (setf (value-b v1)
			(>= (value-s v1)
			    (value-s v2)))
		  (setf (value-type v1) *BOOLEAN*))
		 ((not (equal op *EOL*))
	          (error "`" op "' unrecognized"))
		 (t
		  (setf *backup* t)))))

    ; Skip statement ending *EOL*.
    ;
    (skip *EOL*)

    (setf (gethash variable *variable-table*) v1)

    (deformat "ASSIGN ~A TO ~A"
	      (value-string v1) variable)))

; Main loop to read and execute a statement.
;
(loop for token = (get-token)
      until (equal token *EOF*)
      do
      (if (equal token "if")
	  ; Process `if' statement.  If condition
	  ; is true, just skip past `:'.  Other-
	  ; wise skip past EOL and go to next
	  ; statement.
	  ;
	  (let ((v (get-value)))
	    (require-boolean v)
	    (skip ":")
	    (if (not (value-b v))
	      (loop for token = (get-token)
	            until (equal token *EOL*)
	            do (check-not-eof token))))
	  (setf *backup* t))

      (setf token (get-token))
      (cond ((equal token "clear")
	     (execute-clear))
	    ((equal token "print")
	     (execute-print)
	     (format t " "))
	    ((equal token "println")
	     (execute-print)
	     (format t "~%"))
	    (t
	     (skip "=")
	     (execute-assign token))))
