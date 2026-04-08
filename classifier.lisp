(load "read-csv.lisp")
(use-package :read-csv)

(defun make-predictor (data &optional verbose)
  (progn
    (let ((posts 0) (categories nil) (wordsinpost nil) (wordsincat nil))
      (if verbose (format t "training data:~%"))
      (dolist (post data)
	(progn
	  (if verbose (format t "  label = ~a, content = ~a~%" (third post) (fourth post)))
	  (incf posts)

	  (let ((pair (assoc (third post) categories :test #'string=)))
	    (if (not pair) (push (cons (third post) 1) categories)
		(incf (cdr pair))))
	  
	  (let ((uwords nil))
	    (dolist (word (sb-unicode:words (fourth post)))
	      (tagbody start
		 (if (string= word " ") (go end))
		 (if (member word uwords :test #'string=) (go end))
		 (push word uwords)
		 (let ((catwordpair (assoc (list (third post) word) wordsincat :test
					   (lambda (x y) (and (string= (first x) (first y))
							      (string= (second x) (second y))))))
		       (wordpair (assoc word wordsinpost :test #'string=)))
		   (if (not catwordpair) (push (cons (list (third post) word) 1) wordsincat)
		       (incf (cdr catwordpair)))
		   (if (not wordpair) (push (cons word 1) wordsinpost)
		       (incf (cdr wordpair))))
	       end))
	    ))
	)
      (format t "trained on ~a examples~%" posts) ;; add post count
      (if verbose (format t "vocabulary size = ~a~%" (length wordsinpost)))
      (format t "~%")
      (if verbose
	  (progn
	    (format t "classes:~%")
	    (dolist (category categories)
	      (format t "  ~a, ~a examples, log-prior = ~3$~%"
		      (car category) (cdr category) (log (/ (cdr category) posts))))
	    (format t "classifier parameters:~%")
	    (dolist (catword wordsincat)
	      (format t "  ~a:~a, count = ~a, log-likelihood = ~3$~%"
		      (first (car catword)) (second (car catword)) (cdr catword)
		      (log (/ (cdr catword) (cdr (assoc (first (car catword)) categories
							:test #'string=))))))
	    ))
      (lambda (post)
	(let ((probabilities nil))
	  (progn
	    (dolist (category categories)
	      (let* ((probability (log (/ (cdr category) posts))))
		(dolist (word (sb-unicode:words post))
		  (tagbody start
		     (if (string= word " ") (go end))
		     (incf probability
			   (let ((nposts (assoc (list (car category) word) wordsincat :test
						(lambda (x y) (and (string= (first x) (first y))
								   (string= (second x) (second y)))))))
			     (if (not nposts)
				 (let ((wordn (assoc word wordsinpost :test #'string=)))
				   (if (not wordn) (log (/ 1 posts)) (log (/ (cdr wordn) posts))))
				 (log (/ (cdr nposts) (cdr category))))))
		   end))
		(push (list (car category) probability) probabilities))
		)
	    (reduce (lambda (x y) (if (< (second x) (second y)) x y)) probabilities))
	  )))
      )
    )

(if (or (> 2 (length *posix-argv*)) (< 3 (length *posix-argv*)))
    (progn
      (format t "Usage: classifier.exe TRAIN_FILE [TEST_FILE]~%")
      (exit :code 1)
      ))

(let*
    ((trainfile (with-open-file (s (second *posix-argv*)) (read-csv:parse-csv s)))
     (predictor (make-predictor (cdr trainfile) (= 2 (length *posix-argv*)))))
  (if (= 3 (length *posix-argv*))
      (let ((testfile (with-open-file (s (third *posix-argv*)) (read-csv:parse-csv s)))
	    (correct 0))
	(format t "test data:~%")
	(dolist (post (cdr testfile))
	  (let ((result (funcall predictor (fourth post))))
	    (format t "  correct = ~a, predicted = ~a, log-probability score = ~3$~%  content = ~a~%~%"
		    (third post)
		    (first result)
		    (second result)
		    (fourth post))
	    (if (string= (third post) (first result)) (incf correct))))
	(format t "performance: ~a / ~a posts printed correctly~%"
		correct
		(length (cdr testfile)))
	)))
