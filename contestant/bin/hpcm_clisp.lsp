;;;; If you edit this be careful to test it in a student account.
;;;;
;;;; Functions for invoking editor and running tests.
;;;;
;;;; File:         student.lsp
;;;; Author:       CS 51 (Bob Walton)
;;;; Modified by:  CS 182 (Attila Bodis)
;;;; Version:      5
;;;;
;;;; Changes:
;;;;
;;;;   Version 2:
;;;;
;;;;     Fixed RUN so abort goes to top level when output is to screen.
;;;;     Made :PAUSE NIL the default for RUN.
;;;;
;;;;   Version 3:
;;;;
;;;;     Fixed RUN documentation.
;;;;     Changed "PROMPT" to "PAUSE".
;;;;     Fixed ERROR so it will it will not stop with non-NIL :OUT in IBCL.
;;;;     Added V, LVI, RVI.
;;;;
;;;;   Version 4:
;;;;
;;;;     Modified RUN to display multiple return values in the output.
;;;;
;;;;   Version 5:
;;;;
;;;;     Modified RUN to NOT display multiple return values in the output.
;;;;     Modified RUN to display errors and continue.
;;;;     Modified RUN so it would not always pretty print but would
;;;;                  use the global *pretty-print*.
;;;;	 Added SETF's of *PRINT-PRETTY*, *PRINT-CIRCLE*,
;;;;		      *READ-DEFAULT-FLOAT-FORMAT*, *PRINT-RIGHT-MARGIN*
;;;;	 Added PROCLAIM of OPTIMIZE.
;;;;	 Added TRANSCRIBE-ERROR and HANDLER-CASE.
;;;;	 Added various calls to FRESH-LINE and FINISH-LINE.
;;;;
;;;; This file contains environment modifiers and functions that establish
;;;; a proper environment for CS51 students running Kyoto COMMONLISP.
;;;;

; Make Allegro keep definitions when DEFUN compiles.
;
(setf *save-definitions* t)

; Make Allegro not generate warning messages when functions are
; redefined.
;
(setf *warn-if-redefine* nil)
(setf *warn-if-redefine-kernel* nil)

(defvar *RUN-IN*	nil	"Default :IN argument for RUN function" )
(defvar *RUN-OUT*	nil	"Default :OUT argument for RUN function" )
(defvar *RUN-PAUSE*     nil     "Default :PAUSE argument for RUN function" )

(defvar *RUN-INPUT*	nil	"Current input stream of RUN function" )
(defvar *RUN-OUTPUT*	nil	"Current output stream of RUN function" )

(defun BYE () (EXIT))	; Allegro doesn't have this

(defun TRANSCRIBE-ERROR (c)
  (typecase
   c
   #-allegro
   (simple-error
    (list c
	    (apply #'format nil (simple-condition-format-string c)
		   (simple-condition-format-arguments c))))
   #+allegro
   (simple-error
    (list c
	    (apply #'format nil (simple-condition-format-control c)
		   (simple-condition-format-arguments c))))
   (type-error
    (list c
	    `(DATUM ,(type-error-datum c))
	    `(EXPECTED-TYPE ,(type-error-expected-type c))))
   (t
    (list c))))

(defun RUN (&key (IN *run-in*) (OUT *run-out*) (PAUSE *run-pause*))
  
  "
  Process an :IN file (default extension \".in\") as if it were typed
  into the current LISP listener.  E.g.:

	(run :in \"test1\")

  Without an :OUT argument (:OUT defaults to NIL), output goes to the
  screen.   The :OUT argument sends the output to a file (with default
  extension \".out\"); e.g.:

	(run :in \"test1\" :out \"test1\")

  If a :PAUSE argument is given a true value, and if output is to
  the screen, then RUN will pause just before each input expression
  is evaluated, waiting for the user to type a carriage return to
  continue.  The user can also type an S-expression and a carriage
  return, and this will be immediately evaluated.  The S-expression
  may be used to set breaks or traces: e.g. (TRACE my-function)<RETURN>.

  RUN remembers its last set of arguments so these need not be repeated.
  After running RUN once, you may just type:

	(run)

  to rerun with the same arguments.

  If the :OUT argument equals T, then the output file is set to the
  same as the input file with its extension changed to \".out\".
  "

  ; Save the arguments.

  (setf *run-in* in
	*run-out* out
	*run-pause* pause)
  
  ; Process arguments checking type and range.

  (cond
   ((pathnamep in)) ;do nothing
   ((and (symbolp in) (not (null in))) (setf in (pathname in)))
   ((stringp in) (setf in (pathname in)))
   ((streamp in) (setf in (pathname in)))
   (t (error ":IN ~S is not the type of value that can name a file." in)))
  
  (cond
   ((null (pathname-type in))
    (setf in (make-pathname :type "in" :defaults in))))
  
  (cond
   ((eq out t)
    (setf out (make-pathname :type "out" :defaults in)))
   ((pathnamep out)) ;do nothing
   ((null out)) ;do nothing
   ((symbolp out) (setf out (pathname out)))
   ((stringp out) (setf out (pathname out)))
   ((streamp out) (setf out (pathname out)))
   (t (error ":OUT ~S is not the type of value that can name a file." out)))
  
  (cond
   ((and out (null (pathname-type out)))
    (setf out (make-pathname :type "out" :defaults out))))
  
  (unwind-protect
   (let ((eof-value (cons nil nil))
	 (error-tag (cons nil nil))
	 (*terminal-io* *terminal-io*)
	 (*standard-output* *standard-output*)
	 (*standard-input* *standard-input*)
	 (*error-output* *error-output*)
	 (*trace-output* *trace-output*)
	 (*debug-io* *debug-io*)
	 (*query-io* *query-io*)
	 ; Allegro has trouble with nonstandard *error-output*
	 ; if its allowed to break.
	 (*break-on-errors* *break-on-errors*)
	 (*print-level* 100)
	 (*print-length* 1000))
	
	(setf *run-input* (open in :direction :input))
	(if out (setf *run-output*
		      (open out :direction :output :if-exists :supersede)))
	
	(cond
	 ((not out)
	  (setf *standard-input*
		(make-echo-stream *run-input* *terminal-io*)))
	 (out

	  ; Some LISPs do not make these synonym streams for
	  ; *terminal-io*.
	  ;
	  (let ((io (make-echo-stream *run-input* *run-output*)))
	       (setf *standard-output* io)
	       (setf *standard-input* io)
	       (setf *error-output* io)
	       (setf *trace-output* io)
	       (setf *debug-io* io))
	       (setf *break-on-errors* nil)
	  ))
	
	(do ((s-expression (read nil nil eof-value)
			   (read nil nil eof-value)))
	    ((eq s-expression eof-value))
	    (terpri)
	    (cond
	     (out
	      (princ '|=>|)
	      (terpri)
	      (catch error-tag
		     (unwind-protect
		      (write (eval s-expression) :escape t :pretty t)
		      (throw error-tag nil))))
	     (pause
	      ; Flush out any left over returns.
	      (do ()
		  ((null (read-char-no-hang *terminal-io*))))
	      ; Print prompt.
	      (princ '|=> ? |)
	      (force-output)
	      (let ((line (read-line *terminal-io*)))
		   (catch error-tag
			  (unwind-protect
			   (eval (read-from-string line nil))
			   (throw error-tag nil))))
	      (write (eval s-expression) :escape t :pretty t))
	     ((not pause)
	      (princ '|=>|)
	      (terpri)
	      (write (eval s-expression) :escape t :pretty t)))
	    (terpri)))
   
   (cond
    (*run-input* (close *run-input*) (setf *run-input* nil)))
   (cond
    (*run-output* (close *run-output*) (setf *run-output* nil))))
  
  '|RUN DONE|)

(defvar *VI-FILE*	nil	"Default argument for VI function" )
(defvar *VIL-FILE*	nil	"Default argument for VIL function" )
(defvar *VIR-FILE*	nil	"Default first argument for VIR function" )
(defvar *VIR-OUT*	nil	"Default second argument for VIR function" )


(defun INTERNAL-VI (file)
  
  ; If vi is called on a non-existent file, the command argument
  ; +set lisp showmatch autoindent below has not effect.  So we
  ; create the file if it does not exist first.
  
  (let ((f (open file
		 :direction		:output
		 :if-does-not-exist	:create
		 :if-exists		nil)))
       (if f (close f)))
  
  (run-shell-command
   (concatenate 'string
		"cd " (EXCL::CURRENT-DIRECTORY-STRING) "; "
		"vi '+set lisp showmatch autoindent' "
		file)))

(defun VI (&optional (FILE *VI-FILE*))
  
  "
  Invokes the vi(1) editor on the given file.  If no file is given,
  the last file given as a VI argument is used.
  "
  (setf *vi-file* (string file))

  (internal-vi *vi-file*))

(defun VIL (&optional (FILE *VIL-FILE*))
  
  "
  Invokes the vi(1) editor on the given file and when the editor
  terminates, LOADs the file.  If no file is given, the last file
  given as a VIL argument is used.
  "
  (setf *vil-file* (string file))

  (internal-vi *vil-file*)

  (load *vil-file*))

(defun VIR (&optional (FILE *VIR-FILE* FILEP)
		      (OUT (if filep nil *VIR-OUT*)))
  
  "
  Invokes the vi(1) editor on the given file and when the editor
  terminates, RUNs the file.  If no file is given, the last file
  given as a VIR argument is used.  A second argument may be given
  as the RUN :OUT argument, and it too, if not given, reverts to
  the last second argument given to VIR, unless a first argument
  was given, in which case the second argument defaults to NIL.
  "
  (setf *vir-file* (string file))
  (setf *vir-out* out)

  (internal-vi *vir-file*)

  (run :in *vir-file* :out out))
