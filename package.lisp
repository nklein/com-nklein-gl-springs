(defpackage :com.nklein.gl-springs
    (:use :cl :com.nklein.util.general :com.nklein.util.vec-math)
    (:export :gl-springs
	     :springs-from-asd
	     :particle
		:particle-name
		:particle-pos
	     :spring
		:spring-start
		:spring-end
		:spring-constant
		:spring-rest-length
	     :spring-system
		:spring-system-add-particle
		:spring-system-add-spring))
