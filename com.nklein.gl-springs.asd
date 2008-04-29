(asdf:defsystem :com.nklein.gl-springs
    :depends-on (com.nklein.util.general
		 com.nklein.util.vec-math
		 cffi cl-opengl cl-glu cl-glut)
    :components ((:file "package")
		 (:file "particle"      :depends-on ("package"))
		 (:file "spring"        :depends-on ("package" "particle"))
		 (:file "spring-system" :depends-on
					     ("package" "particle" "spring"))
		 (:file "gl-springs"    :depends-on (
					     "package"
					     "particle"
					     "spring"
					     "spring-system"
					))))
