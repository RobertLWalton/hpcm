;;;; Functions for invoking editor and running tests.
;;;;
;;;; File:	hpcm_clisp.lsp
;;;; Author:	Bob Walton <walton@deas.harvard.edu>
;;;; Modifier:  CS 182 (Attila Bodis)
;;;; Date:	Sat Mar  9 16:02:59 EST 2002
;;;;
;;;; The authors have placed this program in the public
;;;; domain; they make no warranty and accept no
;;;; liability for this program.
;;;;
;;;; RCS Info (may not be true date or author):
;;;;
;;;;   $Author: hc3 $
;;;;   $Date: 2002/03/09 21:33:29 $
;;;;   $RCSfile: hpcm_clisp.lsp,v $
;;;;   $Revision: 1.20 $
;;;;
;;;;
;;;; This file was originally written by the Bob Walton
;;;; for CS51 around 1994 and has been adapted for HPCM.
;;;;
;;;; This file contains environment modifiers and
;;;; functions that establish a proper environment for
;;;; contestants and students running COMMONLISP.
;;;;
;;;; TABLE OF CONTENTS:
;;;;
;;;;	Miscellaneous
;;;;	RUN
;;;;	VI VIL VIR
;;;;	PICO PICOL PICOR

#+allegro
(defun BYE () (EXIT))	; Allegro doesn't have this

;; (put object property value)  
;;
;; Associates value with the given property for the
;; symbol object.  Returns value.
;;
(defun put (object property value)  
  (setf (get object property) value))

;; Attempts to fix backspace/delete problems
(defun fix-backspace ()
  (run-shell-command "stty erase '^H'"))
(defun fix-delete ()
  (run-shell-command "stty erase '^?'"))

; Override defaults for better output
(setf *break-on-errors* t)
(setf *print-pretty* t)
(setf *print-circle* nil)
(setf *read-default-float-format* 'double-float)
#+allegro
(setf *print-right-margin* 56)
#+clisp
(setf system::*prin-linelength* 56)

# The purpose of the following is now unclear.
# (setf LS '|Wrong window, try again...|)

#+allegro
(proclaim '(optimize (speed 2) (safety 1) (space 1)
		     (debug 1)))

(defvar *RUN-IN*	nil
        "Default :IN argument for RUN function" )
(defvar *RUN-OUT*	nil
        "Default :OUT argument for RUN function" )
(defvar *RUN-PAUSE*     nil
        "Default :PAUSE argument for RUN function" )

(defvar *RUN-INPUT*	nil
        "Current input stream of RUN function" )
(defvar *RUN-OUTPUT*	nil
        "Current output stream of RUN function" )

#+clisp
(defun TRANSCRIBE-ERROR (c)
  (fresh-line)
  (princ "ERROR: ")
  (sys::print-condition c *standard-output*))

;; Note: in CLISP (system::unwind-to-driver) resets to
;; top level, if this is ever needed.

# The following code needs to be cleaned up, but we
# have no allegro to test it on so we have left it
# alone for now.
#
#+allegro
(defun TRANSCRIBE-ERROR (c)
  (typecase
   c
   #-allegro
   (simple-error
    (list c
	    (apply #'format nil
	    	   (simple-condition-format-string c)
		   (simple-condition-format-arguments c)
		   )))
   #+allegro
   (simple-error
    (list c
	    (apply #'format nil
	           (simple-condition-format-control c)
		   (simple-condition-format-arguments c)
		   )))
   (type-error
    (list c
	    `(DATUM ,(type-error-datum c))
	    `(EXPECTED-TYPE
	        ,(type-error-expected-type c))))
   (t
    (list c))))

#+clisp
(defun flush-line-feed ()
  (do ()
      ((or (= 0 (system::line-position))
           (not (read-char nil nil nil))))))

  ;; Old code that used to work:
  ;;
  ;; In CLISP, a peek-char is needed after reading a
  ;; (...) to get past the following line feed.
  ;;
  ;; (if (/= 0 (system::line-position))
      ;; (peek-char nil nil nil nil)))

(defun print-prompt ()
  (fresh-line)
  (princ '|--->|)
  (fresh-line))

(defun eval-and-print (s-expression)
  (finish-output) ;; In case crash follows.
  (dolist (val (multiple-value-list
		  (eval s-expression)))
	  (fresh-line)
	  (write val :escape t :pretty t)
	  (fresh-line)))

(defun RUN (&key (IN *run-in*) (OUT *run-out*)
		 (PAUSE *run-pause*))
  
  "
  Process an :IN file somewhat as if it were typed into
  the LISP listener.  E.g.:

	(run :in \"test1\")

  Without an :OUT argument (:OUT defaults to NIL),
  output goes to the screen.   The :OUT argument sends
  the output to a file; e.g.:

	(run :in \"test1\" :out \"test1\")

  If the :IN file does not exist this program tries
  adding the \".in\" extension to it.  In this case,
  if the :OUT file has no extension, this program
  adds the \".out\" extension to it.  An :OUT value of
  T is equivalent to the name of the :IN file with the
  extension changed to \".out\".  An :IN value of T
  is equivalent to the standard input; this just adds
  echoing to the standard input.

  If a :PAUSE argument is given a true value, and if
  output is to the screen, then RUN will pause just
  before each input expression is evaluated, waiting for
  the user to type a carriage return to continue.  The
  user can also type an S-expression and a carriage
  return, and this will be immediately evaluated.  The
  S-expression may be used to set breaks or traces: e.g.
  (TRACE my-function)<RETURN>.

  RUN remembers its last set of arguments so these need
  not be repeated.  After running RUN once, you may just
  type:

	(run)

  to rerun with the same arguments.
  "

  ; Save the arguments.

  (setf *run-in* in
	*run-out* out
	*run-pause* pause)
  
  ; Process arguments checking type and range.

  (cond
   ((eq in t)) ;do nothing
   ((pathnamep in)) ;do nothing
   ((and (symbolp in) (not (null in)))
    (setf in (pathname in)))
   ((stringp in) (setf in (pathname in)))
   ((streamp in) (setf in (pathname in)))
   (t (error (concatenate 'string
                ":IN ~S is not the type of value that"
		" can name a file.")
	     in)))
  
  (cond
   ((and (not (eq in t))
         (null (pathname-type in))
   	 (not (open in :direction :probe)))
    (setf in (make-pathname :type "in" :defaults in))))
 
  (cond
   ((eq out t)
    (if (not (pathnamep in))
        (error (concatenate 'string
	          ":IN ~S is not a pathname while :OUT"
		  " is T" in)))
    (setf out (make-pathname :type "out" :defaults in)))
   ((pathnamep out)) ;do nothing
   ((null out)) ;do nothing
   ((symbolp out) (setf out (pathname out)))
   ((stringp out) (setf out (pathname out)))
   ((streamp out) (setf out (pathname out)))
   (t (error (concatenate 'string
                ":OUT ~S is not the type of value that"
		"can name a file.")
	     out)))
  
  
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
	
	;; Must use *terminal-io* to for standard input.

	(setf *run-input*
	      (if (eq in t) *terminal-io*
	          (open in :direction :input)))
	(if out (setf *run-output*
		      (open out :direction :output
		            :if-exists :supersede)))
	
	(cond
	 ((not out)
	  (setf *standard-input*
		(make-echo-stream *run-input*
		                  *terminal-io*))
	  (fresh-line))
	 (out

	  (setf *terminal-io*
	        (make-echo-stream *run-input*
		                  *run-output*))
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
	    (cond
	     (out
	      ;; Output to a file.

	      #+clisp
	      (flush-line-feed)
	      (print-prompt)
	      (finish-output) ;; In case crash follows.
	      (catch error-tag
		(unwind-protect
		    (dolist (val (handler-case
				  (multiple-value-list
				   (eval s-expression))
				  (error (c)
				   (transcribe-error c))
			 ))
			 (fresh-line)
			 (write val :escape t)
			 (fresh-line))
		  (throw error-tag nil)))
	      (finish-output))
	     (pause

	      ;; Output to the terminal with pauses.

	      ; Loop to read expressions at a pause.
	      ;
	      (do ((done nil))
	          (done)

		  ; Flush out any left over returns.
		  ;
	          (do ()
		      ((null (read-char-no-hang
		                *terminal-io*))))
		  (print-prompt)
		  (finish-output) ;; In case crash
		  		  ;; follows.
	          (let ((line (read-line *terminal-io*
	                                 nil "")))
		    (cond
		      ((equal line "")
		       (setq done t))
		      (t
		       (catch error-tag
		         (unwind-protect
	                     (dolist
			      (val (multiple-value-list
	                            (eval
				     (read-from-string
				      line nil nil))))
	                      (fresh-line)
			      (write val :escape t
			                 :pretty t)
		              (fresh-line))
		           (throw error-tag nil)))))))
	      (dolist (val (multiple-value-list
	                      (eval s-expression)))
		      (write val :escape t :pretty t)
		      (fresh-line)))
	     ((not pause)

	      ;; Output to the terminal without pauses.

	      #+clisp
	      (flush-line-feed)
	      (print-prompt)
	      (finish-output) ;; In case crash follows.
	      (dolist (val (multiple-value-list
	                      (eval s-expression)))
	              (fresh-line)
		      (write val :escape t :pretty t)
		      (fresh-line))))
	    (fresh-line)))
   
   (cond
    (*run-input*
     (if (not (eq *run-input* *terminal-io*))
         (close *run-input*))
     (setf *run-input* nil)))
   (cond
    (*run-output* (close *run-output*)
                  (setf *run-output* nil))))
  
  '|RUN DONE|)


(defvar *VI-FILE*	nil
        "Default argument for VI function" )
(defvar *VIL-FILE*	nil
        "Default argument for VIL function" )
(defvar *VIR-FILE*	nil
        "Default first argument for VIR function" )
(defvar *VIR-OUT*	nil
        "Default second argument for VIR function" )

(defun INTERNAL-VI (file)
  ; If vi is called on a non-existent file, the command
  ; argument +set lisp showmatch autoindent below has no
  ; effect.  So we create the file if it does not exist
  ; first.
  
  (let ((f (open file
		 :direction		:output
		 :if-does-not-exist	:create
		 :if-exists		nil)))
       (if f (close f)))
  
  (shell (concatenate 'string "vim '+set lisp sm ai'
                              " file)))

(defun VI (&optional (FILE *VI-FILE*))
  
  "
  Invokes the vi(1) editor on the given file.  If no
  file is given, the last file given as a VI argument is
  used.
  "
  (setf *vi-file* (string file))

  (internal-vi *vi-file*))

(defun VIL (&optional (FILE *VIL-FILE*))
  
  "
  Invokes the vi(1) editor on the given file and when
  the editor terminates, LOADs the file.  If no file is
  given, the last file given as a VIL argument is used.
  "
  (setf *vil-file* (string file))

  (internal-vi *vil-file*)

  (load *vil-file*))

(defun VIR (&optional (FILE *VIR-FILE* filep)
		      (OUT (if filep nil *VIR-OUT*)))
  "
  Invokes the vi(1) editor on the given file and when
  the editor terminates, RUNs the file.  If no file is
  given, the last file given as a VIR argument is used.
  A second argument may be given as the RUN :OUT argu-
  ment, and it too, if not given, reverts to the last
  second argument given to VIR, unless a first argument
  was given, in which case the second argument defaults
  to NIL.
  "
  (setf *vir-file* (string file))
  (setf *vir-out* out)

  (internal-vi *vir-file*)

  (run :in *vir-file* :out out))

(defvar *PICO-FILE*	nil
        "Default argument for PICO function" )
(defvar *PICOL-FILE*	nil
        "Default argument for PICOL function" )
(defvar *PICOR-FILE*	nil
        "Default first argument for PICOR function" )
(defvar *PICOR-OUT*	nil
        "Default second argument for PICOR function" )


(defun INTERNAL-PICO (file)
  
  (shell (concatenate 'string "pico " file)))

(defun PICO (&optional (FILE *PICO-FILE*))
  
  "
  Invokes the pico(1) editor on the given file.  If no
  file is given, the last file given as a PICO argument
  is used.
  "
  (setf *pico-file* (string file))

  (internal-pico *pico-file*))

(defun PICOL (&optional (FILE *PICOL-FILE*))
  
  "
  Invokes the pico(1) editor on the given file and when
  the editor terminates, LOADs the file.  If no file is
  given, the last file given as a PICOL argument is
  used.
  "
  (setf *picol-file* (string file))

  (internal-pico *picol-file*)

  (load *picol-file*))

(defun PICOR (&optional (FILE *PICOR-FILE* filep)
		        (OUT (if filep nil *PICOR-OUT*)
			))
  
  "
  Invokes the pico(1) editor on the given file and when
  the editor terminates, RUNs the file.  If no file is
  given, the last file given as a PICOR argument is
  used.  A second argument may be given as the RUN :OUT
  argument, and it too, if not given, reverts to the
  last second argument given to PICOR, unless a first
  argument was given, in which case the second argument
  defaults to NIL.
  "
  (setf *picor-file* (string file))
  (setf *picor-out* out)

  (internal-pico *picor-file*)

  (run :in *picor-file* :out out))
