(defun main () (read-a-paragraph 1))

;; Counts are expressed as a triple:
;;
;;	(line-count word-count character-count)

(defvar blank-line '(1 0 0))
(defvar end-of-file '(0 0 0))

(defun read-a-paragraph (paragraph)
  (let ( (counts (read-a-line)) )
    (cond
      ((equal counts blank-line)
       (read-a-paragraph paragraph))
      ((not (equal counts end-of-file))
       (read-rest-of-paragraph counts paragraph)))))

(defun read-rest-of-paragraph (counts paragraph)
  (let ( (line-counts (read-a-line)))
    (cond ((or (equal line-counts blank-line)
	       (equal line-counts end-of-file))
	   (format t "Paragraph ~S" paragraph)
	   (format t ": ~S lines" (first counts))
	   (format t ", ~S words" (second counts))
	   (format t ", ~S characters.~%"
	           (third counts))
	   (if (equal line-counts blank-line)
	       (read-a-paragraph (1+ paragraph))))
	  (t
	   (read-rest-of-paragraph
	     (mapcar #'+ line-counts counts)
	     paragraph)))))

	       
(defun read-a-line ()
  (let ( (line (read-line t nil 'eof)) )
    (cond
      ((eq line 'eof) '(0 0 0))
      (t `(1 ,(read-a-word line 0 (length line) 0)
	     ,(length line))))))

(defun read-a-word (line index length count)
  (cond
    ((>= index length) count)
    ((char= #\Space (aref line index))
     (read-a-word line (1+ index) length count))
    (t
     (read-rest-of-word line (1+ index) length count))))

(defun read-rest-of-word (line index length count)
  (cond
    ((>= index length) (1+ count))
    ((char= #\Space (aref line index))
     (read-a-word line (1+ index) length (1+ count)))
    (t
     (read-rest-of-word line (1+ index) length count))))
