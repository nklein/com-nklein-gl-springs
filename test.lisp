(require :com.nklein.gl-springs)
(use-package :com.nklein.gl-springs)
(use-package :com.nklein.util.vec-math)

(defvar *n0* nil)
(defvar *n1* nil)
(defvar *n2* nil)
(defvar *n3* nil)
(defvar *n4* nil)
(defvar *ss* nil)

(setf *n0* (make-instance 'particle :name "n0"))
(setf *n1* (make-instance 'particle :name "n1"))
(setf *n2* (make-instance 'particle :name "n2"))
(setf *n3* (make-instance 'particle :name "n3"))
(setf *n4* (make-instance 'particle :name "n4"))

(setf *ss* (make-instance 'spring-system))
(spring-system-add-particle *ss* *n0*)
(spring-system-add-particle *ss* *n1*)
(spring-system-add-particle *ss* *n2*)
(spring-system-add-particle *ss* *n3*)
(spring-system-add-particle *ss* *n4*)
(spring-system-add-spring *ss* *n0* *n1* 1.0 0.0)
(spring-system-add-spring *ss* *n0* *n2* 1.0 0.0)
(spring-system-add-spring *ss* *n0* *n3* 1.0 0.0)
(spring-system-add-spring *ss* *n0* *n4* 1.0 0.0)
(spring-system-add-spring *ss* *n1* *n2* 1.0 0.0)
(spring-system-add-spring *ss* *n1* *n3* 1.0 0.0)
(spring-system-add-spring *ss* *n1* *n4* 1.0 0.0)
(spring-system-add-spring *ss* *n2* *n3* 1.0 0.0)
(spring-system-add-spring *ss* *n2* *n4* 1.0 0.0)
(spring-system-add-spring *ss* *n3* *n4* 1.0 0.0)

(gl-springs *ss*)
