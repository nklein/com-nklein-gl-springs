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
    (let ((fname (car ff)))
	(setf (gethash fname ss) nil)))

(defun add-springs-from-form-rec (ss fname ff so-far)
    (cond
	((null ff))
	((atom ff)
	    (multiple-value-bind (ll ll-p) (gethash ff ss)
		(declare (ignore ll))
		(if (and ll-p (not (equal fname ff)))
		    (let ((ent (assoc ff so-far)))
			(if ent
			    (incf (cdr ent))
			    (push (cons ff 1) so-far))))))
	(t
	    (setf so-far (add-springs-from-form-rec ss fname (car ff) so-far))
	    (setf so-far (add-springs-from-form-rec ss fname (cdr ff) so-far))))
    so-far)

(defun add-springs-from-form (ss ff)
    (let ((fname (car ff)))
	(setf (gethash fname ss)
	    (sort
		(add-springs-from-form-rec ss fname (cdr ff) (gethash fname ss))
		#'string-lessp
		:key #'car))))

(defun springs-from-asd (asd)
    (let* ((ss    (make-hash-table))
	   (files (get-filenames-from-asd asd))
           (forms (mapcan #'get-defining-forms-from-file files))
	   ret)
	(mapc #'(lambda (ff) (add-particle-from-form ss ff)) forms)
	(mapc #'(lambda (ff) (add-springs-from-form ss ff)) forms)
	(maphash #'(lambda (key val) (push (cons key val) ret)) ss)
	(sort ret #'string-lessp :key #'car)))
