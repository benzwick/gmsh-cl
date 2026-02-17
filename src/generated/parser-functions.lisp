;;;; parser-functions.lisp — Generated wrappers for gmsh/parser
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/parser)

(defun get-names (&key (search ""))
  "Get the names of the variables in the Gmsh parser matching the
`search' regular expression. If `search' is empty, return all the
names."
  (cffi:with-foreign-objects ((names-out :pointer) (names-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%parser-get-names names-out names-n-out search ierr))
    (foreign-string-array-to-list (cffi:mem-ref names-out :pointer) (cffi:mem-ref names-n-out :unsigned-long))))

(defun set-number (name value)
  "Set the value of the number variable `name' in the Gmsh parser. Create
the variable if it does not exist; update the value if the variable
exists."
  (with-foreign-array (value-ptr value-n value :double)
      (with-ierr (ierr)
      (gmsh/internal::%parser-set-number name value-ptr value-n ierr))))

(defun set-string (name value)
  "Set the value of the string variable `name' in the Gmsh parser. Create
the variable if it does not exist; update the value if the variable
exists."
  (with-string-array (value-ptr value-n value)
      (with-ierr (ierr)
      (gmsh/internal::%parser-set-string name value-ptr value-n ierr))))

(defun get-number (name)
  "Get the value of the number variable `name' from the Gmsh parser.
Return an empty vector if the variable does not exist."
  (cffi:with-foreign-objects ((value-out :pointer) (value-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%parser-get-number name value-out value-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref value-out :pointer) (cffi:mem-ref value-n-out :unsigned-long) :double)))

(defun get-string (name)
  "Get the value of the string variable `name' from the Gmsh parser.
Return an empty vector if the variable does not exist."
  (cffi:with-foreign-objects ((value-out :pointer) (value-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%parser-get-string name value-out value-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref value-out :pointer) (cffi:mem-ref value-n-out :unsigned-long))))

(defun clear (&key (name ""))
  "Clear all the Gmsh parser variables, or remove a single variable if
`name' is given."
  (with-ierr (ierr)
      (gmsh/internal::%parser-clear name ierr)))

(defun parse (file-name)
  "Parse the file `fileName' with the Gmsh parser."
  (with-ierr (ierr)
      (gmsh/internal::%parser-parse file-name ierr)))

