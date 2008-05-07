(defpackage :com.nklein.gl-springs
    (:use :cl :com.nklein.util.general :com.nklein.util.vec-math)
    (:export :gl-springs
	     :particle
		:particle-name
		:particle-pos
	     :spring
		:spring-start
		:spring-end
		:spring-constant
		:spring-rest-length
	     :spring-system
		:make-spring-system
		:read-spring-system-from-file
		:springs-from-asd
		:spring-system-add-particle
		:spring-system-add-spring))
