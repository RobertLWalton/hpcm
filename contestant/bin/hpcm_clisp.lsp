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
;;;;   Version 6:
;;;;
;;;;	Added BYE.
;;;;	Added and fixed VI, VIL, VIR, PICO, PICOL, and PICOR.
;;;;	Note that Franz Allegro requires the system call
;;;;  (run-shell-command
;;;;   (concatenate 'string
;;;;		"cd " (EXCL::CURRENT-DIRECTORY-STRING) "; "
;;;;		"vi '+set lisp sm ai' "	file))
;;;;
;;;;	while CLISP requires only
;;;;  (shell (concatenate 'string "vi '+set lisp sm ai'" file))
;;;;
;;;;

#+allegro
(defun BYE () (EXIT))	; Allegro doesn't have this

; Override defaults for better output
(setf *break-on-errors* t)
(setf *print-pretty* t)
(setf *print-circle* nil)
(setf *read-default-float-format* 'double-float)
#+allegro
(setf *print-right-margin* 56)
#+clisp
(setf system::*prin-linelength* 56)

#+allegro
(proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 1)))

(defvar *RUN-IN*	nil	"Default :IN argument for RUN function" )
(defvar *RUN-OUT*	nil	"Default :OUT argument for RUN function" )
(defvar *RUN-PAUSE*     nil     "Default :PAUSE argument for RUN function" )

(defvar *RUN-INPUT*	nil	"Current input stream of RUN function" )
(defvar *RUN-OUTPUT*	nil	"Current output stream of RUN function" )

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
	 (*query-io* *query-io*)
	 (*standard-output* *standard-output*)
	 (*standard-input* *standard-input*)
	 (*error-output* *error-output*)
	 (*trace-output* *trace-output*)
	 (*debug-io* *debug-io*)
	 (*break-enable* T)
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

	  (setf *terminal-io*
	        (make-echo-stream *run-input* *run-output*))
	  (setf *query-io* *terminal-io*)
	  (setf *standard-input* *terminal-io*)
	  (setf *standard-output* *terminal-io*)
	  (setf *debug-output* *terminal-io*)
	  (setf *trace-output* *terminal-io*)
	  (setf *error-output* *terminal-io*)
	  (setf *break-enable* nil)
	  
	  ))
	
	(do ((s-expression (read nil nil eof-value)
			   (read nil nil eof-value)))
	    ((eq s-expression eof-value))
	    (fresh-line)
	    (cond
	     (out
	      (fresh-line)
	      (princ '|=>|)
	      (fresh-line)
	      (finish-output) ;; In case crash follows.
	      (catch error-tag
		(unwind-protect
		    ;(dolist (val (multiple-value-list ; )))
		    (let ((val (handler-case
				(eval s-expression)
				(error (c) (transcribe-error c)))))
			 (fresh-line)
			 (write val :escape t))
		  (throw error-tag nil)))
	      (finish-output))
	     (pause
					; Flush out any left over returns.
	      (do ()
		  ((null (read-char-no-hang *terminal-io*))))
					; Print prompt.
	      (princ '|=> ? |)
	      (let ((line (read-line *terminal-io*)))
		(catch error-tag
		  (unwind-protect
		      (eval (read-from-string line nil))
		    (throw error-tag nil))))
	      (dolist (val (multiple-value-list (eval s-expression)))
		      (write val :escape t :pretty t)))
	     ((not pause)
	      (princ '|=>|)
	      (fresh-line)
	      (dolist (val (multiple-value-list (eval s-expression)))
		      (write val :escape t :pretty t))))
	    (fresh-line)))
   
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
  ; +set lisp showmatch autoindent below has no effect.  So we
  ; create the file if it does not exist first.
  
  (let ((f (open file
		 :direction		:output
		 :if-does-not-exist	:create
		 :if-exists		nil)))
       (if f (close f)))
  
  (shell (concatenate 'string "vi '+set lisp sm ai' " file)))

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

(defvar *PICO-FILE*	nil	"Default argument for PICO function" )
(defvar *PICOL-FILE*	nil	"Default argument for PICOL function" )
(defvar *PICOR-FILE*	nil	"Default first argument for PICOR function" )
(defvar *PICOR-OUT*	nil	"Default second argument for PICOR function" )


(defun INTERNAL-PICO (file)
  
  (shell (concatenate 'string "pico " file)))

(defun PICO (&optional (FILE *PICO-FILE*))
  
  "
  Invokes the pico(1) editor on the given file.  If no file is given,
  the last file given as a PICO argument is used.
  "
  (setf *pico-file* (string file))

  (internal-pico *pico-file*))

(defun PICOL (&optional (FILE *PICOL-FILE*))
  
  "
  Invokes the pico(1) editor on the given file and when the editor
  terminates, LOADs the file.  If no file is given, the last file
  given as a PICOL argument is used.
  "
  (setf *picol-file* (string file))

  (internal-pico *picol-file*)

  (load *picol-file*))

(defun PICOR (&optional (FILE *PICOR-FILE* FILEP)
		      (OUT (if filep nil *PICOR-OUT*)))
  
  "
  Invokes the pico(1) editor on the given file and when the editor
  terminates, RUNs the file.  If no file is given, the last file
  given as a PICOR argument is used.  A second argument may be given
  as the RUN :OUT argument, and it too, if not given, reverts to
  the last second argument given to PICOR, unless a first argument
  was given, in which case the second argument defaults to NIL.
  "
  (setf *picor-file* (string file))
  (setf *picor-out* out)

  (internal-pico *picor-file*)

  (run :in *picor-file* :out out))

;; (putprop sym pval pname)
;;
;; Associates pval with the property pname for the symbol sym.  Returns pval.
;; (The following definition of putprop is from section 3.1 of the project
;; book.  It is included here for your convenience.)
;;
(defun putprop (sym pval pname)
  (setf (get sym pname) pval))

;; Attempts to fix backspace problems
(defun bksp () (run-shell-command "stty erase '^H'"))
(defun del () (run-shell-command "stty erase '^?'"))
