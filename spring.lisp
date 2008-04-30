(in-package :com.nklein.gl-springs)

(defclass spring ()
    ((start :initarg :start :reader spring-start
		:initform (error "Must specify starting particle"))
     (end   :initarg :end :reader spring-end
		:initform (error "Must specify ending particle"))
     (constant :initarg :k :reader spring-constant :initform 1.0s0)
     (rest-length :initarg :length :accessor spring-rest-length
	 :initform 1.0s0)))

;;;;------------------------------------------------------------------------
(defgeneric spring-calculate-force (ss)
    (:documentation
	"Calculate the force exerted on the end by the spring"))

;;;;------------------------------------------------------------------------
(defmethod spring-calculate-force ((ss spring))
    (with-slots (start end constant rest-length) ss
	(let* ((p0   (particle-pos start))
	       (p1   (particle-pos end))
	       (diff (v- p1 p0))
	       (len  (vnorm diff))
	       (goal (v* diff (/ rest-length len))))
	    (v* (v- goal diff) constant))))
