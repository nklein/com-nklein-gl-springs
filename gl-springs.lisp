(in-package :com.nklein.gl-springs)

(defclass spring-window (glut:window)
    ((spring-system :initarg :system :accessor spring-window-spring-system)
     (last-time :initform (get-internal-real-time)))
    (:default-initargs :width 640 :height 480 :title "gl-springs"
	    :mode '(:single :rgb)))

(defmacro apply-vector (fn p0 nn)
    (let (vals)
	(dotimes (ii nn)
	    (push `(aref ,p0 ,ii) vals))
	`(,fn ,@(nreverse vals))))

(defun draw-spring (ss)
    (let ((p0 (particle-pos (spring-start ss)))
	  (p1 (particle-pos (spring-end ss))))
	(gl:with-primitives :lines
	    (apply-vector gl:vertex p0 3)
	    (apply-vector gl:vertex p1 3))))

(defun draw-particle (pp)
    (let ((pos (particle-pos pp)))
	(gl:with-primitives :points
	    (apply-vector gl:vertex pos 3))))

(defmethod glut:display-window :before ((w spring-window))
    (gl:clear-color 0 0 0 0)
    (gl:line-width 3.0)
    (gl:point-size 9.0)
    (gl:shade-model :flat))

(defmethod glut:display ((w spring-window))
    (gl:clear :color-buffer-bit)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (glu:look-at 0.0 0.0 15.0 0.0 0.0 0.0 0 1 0)
    (gl:rotate (* 30.0 (/ (get-internal-real-time) 1000.0)) 0 1 0)
    (with-slots (spring-system) w
	(gl:color 1 1 1)
	(spring-system-with-each-spring   spring-system #'draw-spring)
	(gl:color 0.3 0.3 1)
	(spring-system-with-each-particle spring-system #'draw-particle))
    (gl:flush))

(defmethod glut:idle ((w spring-window))
    (with-slots (last-time) w
	(let* ((spf (/ 60.0s0))	; nominal seconds per frame
	       (cur (get-internal-real-time))
	       (dt  (/ (- cur last-time) 1000.0s0)))
	    (if (< dt spf)
		(sleep (- spf dt)))
	    (setf last-time cur)
	    (with-slots (spring-system) w
		(spring-system-update spring-system spf))))
    (glut:post-redisplay))

(defmethod glut:reshape ((w spring-window) width height)
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:frustum -1 1 -1 1 1.5 500)
    (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w spring-window) key xx yy)
    (declare (ignore xx yy))
    (when (eql key #\z)
	(glut:destroy-current-window)))

(defmethod glut:visibility ((w spring-window) state)
    (case state
	(:visible (glut:enable-event w :idle))
	(t (glut:disable-event w :idle))))

(defun gl-springs (ss)
    (glut:display-window (make-instance 'spring-window :system ss)))
