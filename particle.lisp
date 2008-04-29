(in-package :com.nklein.gl-springs)

(defclass particle ()
    ((name :initarg :name :accessor particle-name)
     (pos  :initarg :pos  :reader particle-pos
	     :initform (random-vector 'single-float 3))
     (mass :initarg :mass :reader particle-mass :initform 1.0s0)
     prev
     acc))

;;;;------------------------------------------------------------------------
(defgeneric particle-reset-acc (nn)
    (:documentation
	"Prepare the accumulator to handle the same size vectors as the pos"))

(defgeneric particle-add-force-to-acc (nn vec)
    (:documentation "Add something into the accumulation vector"))

(defgeneric particle-apply-force (nn elapsed)
    (:documentation "Apply the force to the current particle"))

;;;;------------------------------------------------------------------------
(defmethod initialize-instance :after ((nn particle) &key)
    "Reset the accumulator and copy the current-position to the previous"
    (with-slots (pos prev) nn
	(setf prev (map 'vector #'identity pos)))
    (particle-reset-acc nn))

(defmethod particle-reset-acc ((nn particle))
    (with-slots (pos acc) nn
	(setf acc (map 'vector #'(lambda (x) (* 0s0 x)) pos))))

(defmethod particle-add-force-to-acc ((nn particle) vec)
    (with-slots (acc) nn
	(setf acc (v+ acc vec))))

(defmethod particle-apply-force ((nn particle) elapsed)
    "Use Verlet integration to calculate the new position"
    (with-slots (pos prev acc mass) nn
	(let ((aa (v/ acc mass))
	      (e2 (* elapsed elapsed)))
	    (let ((xx (v+ (v- (v* pos 2.0s0) prev) (v* aa e2))))
		    (setf prev pos
			  pos  xx)))))
