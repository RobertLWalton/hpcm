;; Solution to the Simple Vector Calculator Problem
;;
;; File:	vcalc.lsp
;; Authors:	Bob Walton (walton@seas.harvard.edu)
;; Date:	Sun Feb 10 06:05:58 EST 2013
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
(defun scalar-string ( number )
  (let ((result (format nil "~,14f" number )))
    (do ((p (- (length result) 1) (- p 1)))
        ((or (= p 0) (char/= (char result p) #\0))
	 (progn
	   (if (char= (char result p) #\.) (decf p 1))
	   (subseq result 0 p))))))

(defvar *EOL* "END-OF-LINE")
(defvar *EOF* "END-OF-FILE")
;
(defun return-separator ( stream char )
  (char-name char))
(defun return-end-of-line ( stream char )
  *EOL*)

(defvar *my-readtable*
  (let ((my-readtable (copy-readtable)))
    (do ((c (code-char (+ 1 (char-code #\Space)))
	    (code-char (+ 1 (char-code c)))))
        ((char= c #\Rubout))
      (set-syntax-from-char c #\a my-readtable))
    (set-macro-character #\( #'return-separator
			 nil my-readtable)
    (set-macro-character #\) #'return-separator
			 nil my-readtable)
    (set-macro-character #\, #'return-separator
			 nil my-readtable)
    (set-macro-character #\: #'return-separator
			 nil my-readtable)
    (set-macro-character #\Newline #'return-end-of-line
			 nil my-readtable)
    my-readtable))

; Returns next token or *EOL* if end of line or *EOF* if
; end of file.  We make *EOL* and *EOF* be printable
; strings as tokens are printed in many error messages.
; Implements one-token backup.  Implements line_number.
;
; All tokens are non-zero length strings or numbers.
;
(defvar *backup* nil)
    ; Set backup = t to backup.
(defvar *line-number* 0)
    ; Current line number; 1, 2, 3, ...
;
(defvar *last-token* *EOL*)
;
(defun get-token ()
  (cond (*backup*
	  (setf *backup* nil)
	  (return *last-token*))
	((string= *last-token* *EOL*)
	 (loop
	   do (incf *line-number*)
	   (let ((c (peek-char t nil t #\Space)))
	     (cond ((char= c #\/)
	            (read-char)
		    (setf c (peek-char nil nil
				       t #\Space))
		    (if (char/= c #\/)
		      (error "line begins with `/'"
			     " not followed by a `/'"))
		    (loop until
			  (char= (read-char nil
					    t #\Newline)
			         #\Newline)))
		   (t return))))))

  (setf last-token (let ((*readtabel* *my-readtable*))
		        (read nil t *EOF*)))
  (dformat "{~A}" *last-token*)
  *last-token*)

; Print error message made by concatenating ~A printouts
; of arguments without intervening space, and exit.
;
(defun error ( &rest args )
  (format t "ERROR in line ~A:~5      " *line-number*)
  (dolist x args (format t "~A" x))
  (format t "~%")
  (quit nil 1))

(defun check-not-eof ( token )
  (if (equal token *EOF*)
      (error "unexpected end of file" )))

(defun skip ( desired )
  (let ((token (get-token)))
    (if (not (equal token desired))
	(error "expected `" desired 
	       "' but found `" token "'" ))))

(defun is-variable ( token )
  (and (stringp token)
       (alpha-char-p (char token 0))))

(defun check-variable ( token )
   (if (not (is-variable token))
      (error "`" token "' is not a variable" )))

(defstruct vector x y)

(defun vector-string ( v )
  (format nil "(~A, ~A)"
	  (scalar-string (vector-x v))
	  (scalar-string (vector-y v))))

(defun vector-negate ( Vector v )
  (make-vector :x (- (vector-x v))
	       :y (- (vector-y v))))
(defun vector-add ( v1 v2 )
  (make-vector :x (+ (vector-x v1) (vector-x v2))
	       :y (+ (vector-y v1) (vector-y v2))
(defun vector-subtract ( v1 v2 )
  (make-vector :x (- (vector-x v1) (vector-x v2))
	       :y (- (vector-y v1) (vector-y v2))
(defun vector-multiply ( v1 v2 )
  (+ (* (vector-x v1) (vector-x v2))
     (* (vector-y v1) (vector-y v2))))
(defun scalar-multiply ( s v )
  (make-vector :x (* s (vector-x v))
	       :y (* s (vector-y v))))
(defun vector-length ( v )
  (sqrt (vector-multiply v v)))

(defun vector-angle ( v )
  ; We take extra care with angles that are
  ; multiples of 90 degrees.  This is only
  ; necessary if one is using exact equality
  ; with integer coordinates instead of
  ; approximate equality.
  ;
  (cond ((and (= 0 (vector-x v))
	      (= 0 (vector-y v)))
	 (error "angle of zero vector" ))
	((= 0 (vector-x v))
	 (if (> (vector-y) 0) +90 -90))
	((= 0 (vector-y v))
	 (if (> (vector-x) 0) 0 +180))
	(t
	  (* (/ 180.0 PI) (atan (vector-y v)
			        (vector-x v))))))

(defun vector-rotate ( v angle ) 
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

(defun value-string ( v )
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

(defun require_boolean ( Value v1 )
{
    if ( v1.type != BOOLEAN )
	error ( "operand should be boolean" );
}
(defun require_boolean ( Value v1, Value v2 )
{
    if ( v1.type != BOOLEAN )
	error
	    ( "first operand should be boolean" );
    else if ( v2.type != BOOLEAN )
	error
	    ( "second operand should be boolean" );
}

(defun require_scalar ( Value v1 )
  (if (/= (value-type v1) *SCALAR*)
    (error "operand should be scalar" )))

(defun require_scalar ( Value v1, Value v2 )
  (cond ((/= (value-type v1) *SCALAR*)
	 (error "first operand should be scalar" ))
        ((/= (value-type v2) *SCALAR*)
	 (error "second operand should be scalar" ))))

(defun require_vector ( v1 )
  (if (/= (value-type v1) *VECTOR*)
    (error "operand should be vector" )))

(defun require_vector ( v1 v2 )
  (cond ((/= (value-type v1) *VECTOR*)
	 (error "first operand should be vector" ))
        ((/= (value-type v2) *VECTOR*)
	 (error "second operand should be vector" ))))

(defun require_scalar_vector ( v1 v2 )
  (cond ((/= (value-type v1) *SCALAR*)
	 (error "first operand should be scalar" ))
        ((/= (value-type v2) *VECTOR*)
	 (error "second operand should be vector" ))))

(defun require_vector_scalar ( v1 v2 )
  (cond ((/= (value-type v1) *VECTOR*)
	 (error "first operand should be vector" ))
        ((/= (value-type v2) *SCALAR*)
	 (error "second operand should be scalar" ))))
 
; Require v1 to be SCALAR or VECTOR and return
; true if SCALAR, false if VECTOR.  If v2 given,
; require it to be of the same type as v1.
;
(defun is-scalar ( v1 &optional v2 )
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
(defun get-value ( )
  (let ((v (make-value))
	(token (get-token))
	x y))
    (cond ((equals token "(")
	   (setf (value-type v) *VECTOR*)
	   (setf (value-v v) (make-vector))
	   (setf (vector-x (value-v v)) (get-number))
	   (skip ",")
	   (setf (vector-y (value-v v)) (get-number))
	   (skip ")"))
	  ((equals token "true")
	   (setf (value-type v) *BOOLEAN*)
	   (setf (value-b v) t))
	  ((equals token "false")
	   (setf (value-type v) *BOOLEAN*)
	   (setf (value-b v) nil))
          ((is-variable token)
	   (let ((v2 (gethash token *variable-table)))
	     (if (equals v2 nil)
	         (error "`" token "' unassigned" ))
	     (setf (value-type v) (value-type v2)
	           (value-b    v) (value-b    v2)
	           (value-s    v) (value-s    v2)
	           (value-v    v) (value-b    v2))))
	  ((numberp token)
	   (setf (value-type v) *SCALAR*)
	   (setf (value-s v) token))
	  (t
	   (error "expected true, false, "
		  " scalar constant, "
		  " vector constant, "
		  " or variable but got `"
		  token "'" )))
    (dformat "[~A]" (value-string v))
    v)

; Execute `clear ...' statement after `clear' token
; has been read and skipped.
;
(defun execute-clear ( )
  (let ((token (get-token)))
    (cond ((equals token *EOL*)
	   (maphash #'(lambda (key val)
			(remhash key *variable-table*))
		    *variable-table*))
	  (t
	    (loop until (equals token *EOL*) do
		  (check-variable token)
		  (remhash token *variable-table*)
		  (setf token (get-token)))))))

; Execute `print{ln} ...' statement after
; `print{ln}' token has been read and skipped,
; BUT do not output final space or line end.
;
(defun execute-print ( )
  (let ((first t) (token (get-token)) v)
    (loop until (equals token *EOL*) do
	  (if first (setf first nil)
	            (format t " "))
	  (setf v (gethash token *variable-table*))
	  (format t (if v (value-string v)
		          (token-string v)))
	  (setf token (get-token))))

    // Execute `variable = ...' statement after
    // `variable =' tokens have been read and skipped.
    // Check variable token to be sure its a variable
    // name.
    //
    static void execute_assign ( String variable )
    {
        check_variable ( variable );

	if ( variable.equals ( "true" )
	     ||
	     variable.equals ( "false" ) )
	    error ( "attempt to assign a value to `"
	            + variable + "'" );

        String op = get_token();

	Value v1 = null;
	    // This is both the first value read and the
	    // final resulting value.

	// Read and process statement up to but not
	// including EOL.
	//
	if ( op.equals ( "-" ) )
	{
	    v1 = get_value();
	    if ( is_scalar ( v1 ) )
	        v1.s = - v1.s;
	    else
	        v1.v = negate ( v1.v );
	}
	else if ( op.equals ( "|" ) )
	{
	    v1 = get_value();
	    require_scalar ( v1 );
	    skip ( "|" );
	    v1.s = Math.abs ( v1.s );
	}
	else if ( op.equals ( "||" ) )
	{
	    v1 = get_value();
	    require_vector ( v1 );
	    skip ( "||" );
	    v1.s = length ( v1.v );
	    v1.type = SCALAR;
	}
	else if ( op.equals ( "angle" ) )
	{
	    v1 = get_value();
	    require_vector ( v1 );
	    v1.s = angle ( v1.v );
	    v1.type = SCALAR;
	}
	else if ( op.equals ( "!" ) )
	{
	    v1 = get_value();
	    require_boolean ( v1 );
	    v1.b = ! v1.b;
	}
	else
	{
	    // Case where there is either a binary
	    // operator or no operator.
	    //
	    backup = true;
	    v1 = get_value();
	    op = get_token();

	    if ( op.equals ( "+" ) )
	    {
		Value v2 = get_value();
		if ( is_scalar ( v1, v2 ) )
		    v1.s += v2.s;
		else
		    v1.v = add ( v1.v, v2.v );
	    }
	    else if ( op.equals ( "-" ) )
	    {
		Value v2 = get_value();
		if ( is_scalar ( v1, v2 ) )
		    v1.s -= v2.s;
		else
		    v1.v = subtract ( v1.v, v2.v );
	    }
	    else if ( op.equals ( "*" ) )
	    {
		Value v2 = get_value();
		if ( is_scalar ( v1 ) )
		{
		    if ( is_scalar ( v2 ) )
			v1.s *= v2.s;
		    else
		    {
			v1.v = multiply ( v1.s, v2.v );
			v1.type = VECTOR;
		    }
		}
		else
		{
		    require_vector ( v1, v2 );
		    v1.s = multiply ( v1.v, v2.v );
		    v1.type = SCALAR;
		}
	    }
	    else if ( op.equals ( "/" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		if ( v2.s == 0 )
		    error ( "zero divisor" );
		v1.s /= v2.s;
	    }
	    else if ( op.equals ( "^" ) )
	    {
		Value v2 = get_value();
		require_vector_scalar ( v1, v2 );
		v1.v = rotate ( v1.v, v2.s );
	    }
	    else if ( op.equals ( "&&" ) )
	    {
		Value v2 = get_value();
		require_boolean ( v1, v2 );
		v1.b = v1.b && v2.b;
	    }
	    else if ( op.equals ( "||" ) )
	    {
		Value v2 = get_value();
		require_boolean ( v1, v2 );
		v1.b = v1.b || v2.b;
	    }
	    else if ( op.equals ( "==" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s == v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "!=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s != v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "<" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s < v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "<=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s <= v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( ">" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s > v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( ">=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s >= v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( ! op.equals ( EOL ) )
	        error ( "`" + op + "' unrecognized" );
	    else backup = true;
	}

	// Skip statement ending EOL.
	//
	skip ( EOL );

	variable_table.put ( variable, v1 );

	dprintln ( "ASSIGN " + v1.toString()
	           + " TO " + variable );
    }


    public static void main ( String[] args )
    {
	debug = ( args.length > 0 );

	// Loop to read and execute a statement.
	//
	while ( true )
	{
	    String token = get_token();
	    if ( token == EOF ) break;

	    if ( token.equals ( "if" ) )
	    {
	        // Process `if' statement.  If condition
		// is true, just skip past `:'.  Other-
		// wise skip past EOL and go to next
		// statement.
		//
	        Value v = get_value();
		require_boolean ( v );
		skip ( ":" );
		if ( ! v.b )
		{
		    while ( true )
		    {
		        token = get_token();
			check_not_eof ( token );
			if ( token.equals ( EOL ) )
			    break;
		    }
		    continue;
		}
		token = get_token();
	    }

	    if ( token.equals ( "clear" ) )
	        execute_clear();
	    else if ( token.equals ( "print" ) )
	    {
	        execute_print();
		print ( " " );
	    }
	    else if ( token.equals ( "println" ) )
	    {
	        execute_print();
		println();
	    }
	    else
	    {
		skip ( "=" );
		execute_assign ( token );
	    }
	}
    }
}

