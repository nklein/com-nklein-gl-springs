(asdf:defsystem :com.nklein.gl-springs
    :depends-on (:com.nklein.util.general
		 :com.nklein.util.vec-math)
    :components ((:file "package")
		 (:file "node"   :depends-on ("package"))))
