(in-package :com.nklein.gl-springs)

(defclass spring-window (glut:window)
    ((spring-system :initarg :system :accessor spring-window-spring-system))
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
    (declare (ignore pp)))

(defmethod glut:display-window :before ((w spring-window))
    (gl:clear-color 0 0 0 0)
    (gl:shade-model :flat))

(defmethod glut:display ((w spring-window))
    (gl:clear :color-buffer-bit)
    (gl:color 1 1 1)
    (gl:load-identity)
    (glu:look-at 0.0 0.0 30.0 0.0 0.0 0.0 0 1 0)
    (gl:line-width 2)
    (with-slots (spring-system) w
	(spring-system-with-each-spring   spring-system #'draw-spring)
	(spring-system-with-each-particle spring-system #'draw-particle))
    (gl:flush))

(defmethod glut:idle ((w spring-window))
    (with-slots (spring-system) w
	(spring-system-update spring-system (/ 1500.0)))
    (glut:post-redisplay))

(defmethod glut:reshape ((w spring-window) width height)
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:frustum -1 1 -1 1 1.5 100)
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
