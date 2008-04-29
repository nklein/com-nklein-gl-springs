(in-package :com.nklein.gl-springs)

(defclass spring-system ()
    ((particles :initform (make-hash-table))
     (springs   :initform nil)))

;;;;------------------------------------------------------------------------
(defgeneric spring-system-update (ss elapsed)
    (:documentation "Update the whole system based on the elapsed time"))

(defgeneric spring-system-add-particle (ss pp)
    (:documentation "Add a particle to the system"))

(defgeneric spring-system-find-particle (ss name)
    (:documentation "Find a particle by name"))

(defgeneric spring-system-delete-particle (ss pp)
    (:documentation "Delete a particle from the system"))

(defgeneric spring-system-add-spring (ss p0 p1 kk ll)
    (:documentation "Add a spring connecting these two particles"))

(defgeneric spring-system-with-each-particle (ss fn)
    (:documentation "Iterate over each particle"))

(defgeneric spring-system-with-each-spring (ss fn)
    (:documentation "Iterate over each spring"))

;;;;------------------------------------------------------------------------
(defun particle-repulsive-force (v0 v1)
    (if (not (eql v0 v1))
	(let* ((p0 (particle-pos v0))
	       (p1 (particle-pos v1))
	       (dir (v- p1 p0))
	       (len (vnorm dir)))
	    (particle-add-force-to-acc v1 (v/ dir (* len len len))))))

(defmethod spring-system-update ((ss spring-system) elapsed)
    (with-slots (particles springs) ss
	;; reset our accumulators
	(spring-system-with-each-particle ss
		#'(lambda (pp) (particle-reset-acc pp)))
	;; big n^2 particles pushing each other away
	;; there is redudancy here, because F(a,b) = -F(b,a), but
	;; I'm not worrying about that right now
	(spring-system-with-each-particle ss
		#'(lambda (p0)
			(spring-system-with-each-particle ss
				#'(lambda (p1)
					(particle-repulsive-force p0 p1)))))
	;; apply forces from springs
	(spring-system-with-each-spring ss
		#'(lambda (s)
		    (let ((start (spring-start s))
			  (end (spring-end s))
			  (ff (spring-calculate-force s)))
			(particle-add-force-to-acc start (v* ff -1.0s0))
			(particle-add-force-to-acc end ff))))
	;; apply the forces
	(spring-system-with-each-particle ss
		#'(lambda (pp)
		    (particle-apply-force pp elapsed)))))

(defmethod spring-system-add-particle ((ss spring-system) (pp particle))
    (with-slots (particles) ss
	(setf (gethash (particle-name pp) particles) pp)))

(defmethod spring-system-find-particle ((ss spring-system) name)
    (with-slots (particles) ss
	(gethash name particles)))

(defmethod spring-system-delete-particle ((ss spring-system) (pp particle))
    (with-slots (particles springs) ss
	(setf springs (remove-if #'(lambda (x)
					(or (eql pp (spring-start x))
					    (eql pp (spring-end x)))) springs))
	(remhash (particle-name pp) particles)))

(defmethod spring-system-add-spring
    ((ss spring-system) (p0 particle) (p1 particle) kk ll)
    (with-slots (springs) ss
	(push (make-instance 'spring :start p0 :end p1 :k kk :length ll)
	      springs)))

(defmethod spring-system-with-each-particle ((ss spring-system) fn)
    (with-slots (particles) ss
	(maphash #'(lambda (k v) (declare (ignore k))(funcall fn v))
		 particles)))

(defmethod spring-system-with-each-spring ((ss spring-system) fn)
    (with-slots (springs) ss
	(mapcar fn springs)))
