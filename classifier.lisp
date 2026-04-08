(load "read-csv.lisp")
(use-package :read-csv)

(defun make-classifier (data &optional verbose)
  (progn
    (let ((posts 0) (categories nil) (words nil))
      (if verbose (format t "training data:~%"))
      (dolist (post data)
	(progn
	  (if verbose (format t "  label = ~a, content = ~a~%" (third post) (fourth post)))
	  (incf posts)

	  (let ((pair (assoc (third post) categories :test #'string=)))
	    (if (not pair) (push (cons (third post) 1) categories)
		(incf (cdr pair))))

	  (dolist (word (sb-unicode:words (fourth post)))
	    (tagbody start
	     (if (string= word " ") (go end)
		 (print word))
	       end))
	  ))
      (format t "trained on ~a posts" posts) ;; add post count
      )
    )
  )

(if (or (> 2 (length *posix-argv*)) (< 3 (length *posix-argv*)))
    (progn
     (format t "Usage: classifier.exe TRAIN_FILE [TEST_FILE]~%")
     ;; (exit :code 1)
     ))

(let
    ((trainfile (with-open-file (s (second *posix-argv*))
		  (read-csv:parse-csv s))))
  (make-classifier (cdr trainfile) (= 2 (length *posix-argv*))))
  

