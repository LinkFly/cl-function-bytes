(defpackage :cl-function-bytes 
  (:use :cl :metatilities :iterate)
  (:import-from :split-sequence #:split-sequence)
  (:shadowing-import-from :iterate #:minimize #:finish)
  (:export
   #:get-bytes-str-from-disassemble-output
   #:split-on-bytes
   #:function-size))

(in-package :cl-function-bytes)

#|
(setq fn (LAMBDA (FIELD R C V)
           (DECLARE (IGNORABLE FIELD R C V)
                    (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0))
                    (TYPE (SIMPLE-ARRAY FIXNUM (9 9)) FIELD)
                    (TYPE FIXNUM V))
           (AND (/= (AREF FIELD 0 4) V) (/= (AREF FIELD 1 4) V)
                (/= (AREF FIELD 2 4) V) (/= (AREF FIELD 3 4) V)
                (/= (AREF FIELD 4 4) V) (/= (AREF FIELD 5 4) V)
                (/= (AREF FIELD 6 3) V) (/= (AREF FIELD 6 4) V)
                (/= (AREF FIELD 6 5) V) (/= (AREF FIELD 7 3) V)
                (/= (AREF FIELD 7 4) V) (/= (AREF FIELD 7 5) V)
                (/= (AREF FIELD 8 0) V) (/= (AREF FIELD 8 1) V)
                (/= (AREF FIELD 8 2) V) (/= (AREF FIELD 8 3) V)
                (/= (AREF FIELD 8 5) V) (/= (AREF FIELD 8 6) V)
                (/= (AREF FIELD 8 7) V) (/= (AREF FIELD 8 8) V))))
|#

(defun disassemble-output-to-string (fn)
  (with-output-to-string (str-stream)
    (disassemble fn :stream str-stream)))

#|
(setq str (disassemble-output-to-string fn))
|#

#|
(setq strs (remove-if (compose #'zerop #'length) 
		      (split-sequence #\Newline str)))
|#

(defun get-bytes-str-from-disassemble-output (fn)
  (let* ((disassemble-output-str (disassemble-output-to-string fn))
	 (strs-with-disassembles 
	  (remove-if #'(lambda (line) (string= ";;;" (subseq line 0 3)))
		     (remove-if (compose #'zerop #'length) 
				(rest (split-sequence #\Newline
						      disassemble-output-str))))))
    (remove-if #'(lambda (char) 
		   (or (char= #\Newline char) 
		       (char= #\Space char)))
	       (apply #'concatenate
		      'string
		      (mapcar #'(lambda (line)
				  (let* ((sentences 
					  (remove-if (compose #'zerop #'length)
						     (split-sequence #\Space line)))
					 (sent-3 (third sentences)))
				    (if (char/= #\L (elt sent-3 0))
					sent-3
					(fourth sentences))))
			      strs-with-disassembles)))))

(setq strs-with-disassembles strs)

(defun split-on-bytes (str)
  (iter (for i from 0 below (length str) by 2)
	(collect (coerce (list (elt str i)
			       (elt str (1+ i)))
			 'string))))

;(split-on-bytes (get-bytes-str-from-disassemble-output fn))

(defun function-size (fn)
  (/ (length (get-bytes-str-from-disassemble-output fn))
     2))

(remove-if (compose #'zerop #'length) (split-sequence #\Space ";       91:       8B051C000000     MOV EAX, [#x1C]"))

