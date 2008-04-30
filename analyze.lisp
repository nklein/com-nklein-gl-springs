(in-package :com.nklein.gl-springs)

(defun get-filenames-from-asd (asd)
    (with-open-file (in asd :direction :input)
	(do ((form (read in nil) (read in nil)))
	    ((null form))
	    (if (eql (first form) 'asdf:defsystem)
		(let ((plist (cddr form)))
		    (return (mapcar #'(lambda (ll)
					(concatenate 'string
					    (getf ll :file) ".lisp"))
				    (getf plist :components))))))))

(defun get-defining-forms-from-file (ff)
    (with-open-file (in ff :direction :input)
	(do ((form (read in nil) (read in nil))
	     ret)
	    ((null form) (remove-duplicates ret))
	    (let ((func (first form)))
		(if (or (eql func 'defun) (eql func 'defmacro)
			(eql func 'defmethod))
		    (push (cons (second form) (cdddr form)) ret))))))

(defun add-particle-from-form (ss ff)
    (spring-system-add-particle ss (make-instance 'particle :name (car ff))))

(defun add-springs-from-form (ss ff)
    (let* ((func  (car ff))
	   (start (spring-system-find-particle ss func)))
	(if start
	    (labels ((trec (tt)
		    (cond
			((atom tt)
			    (let ((end (spring-system-find-particle ss tt)))
				(if end
				    (progn
					(format t "SPRING: ~A to ~A~%" func tt)
					(spring-system-add-spring ss
					    start end 1.0 1.0)))))
			(t
			    (trec (car tt))
			    (trec (cdr tt))))))
		(trec (cdr ff))))))

(defun springs-from-asd (asd)
    (let* ((ss    (make-instance 'spring-system))
	   (files (get-filenames-from-asd asd))
           (forms (mapcan #'get-defining-forms-from-file files)))
	(format t "names: ~A~%" (mapcar #'(lambda (x) (car x)) forms))
	(mapc #'(lambda (ff) (add-particle-from-form ss ff)) forms)
	(mapc #'(lambda (ff) (add-springs-from-form ss ff)) forms)
	ss))
