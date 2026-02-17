;;;; conditions.lisp — Error conditions for gmsh-cl
;;;;
;;;; *** DO NOT EDIT — part of gmsh-cl core infrastructure ***

(in-package :gmsh/internal)

(define-condition gmsh-error (error)
  ((code :initarg :code :reader gmsh-error-code)
   (message :initarg :message :reader gmsh-error-message))
  (:report (lambda (c s)
             (format s "Gmsh error ~D: ~A"
                     (gmsh-error-code c)
                     (gmsh-error-message c)))))
