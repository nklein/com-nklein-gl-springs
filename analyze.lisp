(in-package :com.nklein.gl-springs)

(defun get-filenames-from-components (plist prefix)
    (let ((prefix prefix)
	  (comps  (getf plist :components))
	  (module (getf plist :module)))
	(if module
	    (typecase module
		(sequence (setf prefix (concatenate 'string prefix "/" module)))
		(t        (setf prefix (concatenate 'string
						    prefix
						    "/"
						    (symbol-name module))))))
	(mapcan #'(lambda (ll)
			(let ((ff (getf ll :file)))
			    (if ff
				(list
				    (concatenate 'string prefix "/" ff ".lisp"))
				(get-filenames-from-components ll prefix))))
		comps)))

(defun get-filenames-from-asd (asd)
    (with-open-file (in asd :direction :input)
	(do ((form (read in nil) (read in nil)))
	    ((null form))
	    (if (equalp (symbol-name (first form)) "DEFSYSTEM")
		(let ((plist (cddr form)))
		    (return (get-filenames-from-components plist ".")))))))

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
					    start end 0.1 1.0)))))
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
