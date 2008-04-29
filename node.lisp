(in-package :com.nklein.gl-springs)

(defclass node ()
    ((name :initarg :name)
     (pos  :initarg :pos  :initform (random-vector 'single-float 3))
     (mass :initarg :mass :initform 1.0s0)
     prev
     acc))

;;;;------------------------------------------------------------------------
(defgeneric node-reset-acc (nn)
    (:documentation
	"Prepare the accumulator to handle the same size vectors as the pos"))

(defgeneric node-add-force-to-acc (nn vec)
    (:documentation "Add something into the accumulation vector"))

(defgeneric node-apply-force (nn elapsed)
    (:documentation "Apply the force to the current node"))

;;;;------------------------------------------------------------------------
(defmethod initialize-instance :after ((nn node) &key)
    "Reset the accumulator and copy the current-position to the previous"
    (with-slots (pos prev) nn
	(setf prev (map 'vector #'identity pos)))
    (node-reset-acc nn))

(defmethod node-reset-acc ((nn node))
    (with-slots (pos acc) nn
	(setf acc (map 'vector #'(lambda (x) (* 0s0 x)) pos))))

(defmethod node-add-force-to-acc ((nn node) vec)
    (with-slots (acc) nn
	(setf acc (v+ acc vec))))

(defmethod node-apply-force ((nn node) elapsed)
    "Use Verlet integration to calculate the new position"
    (with-slots (pos prev acc mass) nn
	(let ((aa (v/ acc mass))
	      (e2 (* elapsed elapsed)))
	    (let ((xx (v+ (v- (v* pos 2.0s0) prev) (v* aa e2))))
		    (setf prev pos
			  pos  xx)))))
