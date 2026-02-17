;;;; onelab-functions.lisp — Generated wrappers for gmsh/onelab
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/onelab)

(defun set (data &key (format "json"))
  "Set one or more parameters in the ONELAB database, encoded in
`format'."
  (with-ierr (ierr)
      (gmsh/internal::%onelab-set data format ierr)))

(defun get (&key (name "") (format "json"))
  "Get all the parameters (or a single one if `name' is specified) from
the ONELAB database, encoded in `format'."
  (cffi:with-foreign-object (data-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%onelab-get data-out name format ierr))
    (foreign-string-result (cffi:mem-ref data-out :pointer))))

(defun get-names (&key (search ""))
  "Get the names of the parameters in the ONELAB database matching the
`search' regular expression. If `search' is empty, return all the
names."
  (cffi:with-foreign-objects ((names-out :pointer) (names-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%onelab-get-names names-out names-n-out search ierr))
    (foreign-string-array-to-list (cffi:mem-ref names-out :pointer) (cffi:mem-ref names-n-out :unsigned-long))))

(defun set-number (name value)
  "Set the value of the number parameter `name' in the ONELAB database.
Create the parameter if it does not exist; update the value if the
parameter exists."
  (with-foreign-array (value-ptr value-n value :double)
      (with-ierr (ierr)
      (gmsh/internal::%onelab-set-number name value-ptr value-n ierr))))

(defun set-string (name value)
  "Set the value of the string parameter `name' in the ONELAB database.
Create the parameter if it does not exist; update the value if the
parameter exists."
  (with-string-array (value-ptr value-n value)
      (with-ierr (ierr)
      (gmsh/internal::%onelab-set-string name value-ptr value-n ierr))))

(defun get-number (name)
  "Get the value of the number parameter `name' from the ONELAB database.
Return an empty vector if the parameter does not exist."
  (cffi:with-foreign-objects ((value-out :pointer) (value-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%onelab-get-number name value-out value-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref value-out :pointer) (cffi:mem-ref value-n-out :unsigned-long) :double)))

(defun get-string (name)
  "Get the value of the string parameter `name' from the ONELAB database.
Return an empty vector if the parameter does not exist."
  (cffi:with-foreign-objects ((value-out :pointer) (value-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%onelab-get-string name value-out value-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref value-out :pointer) (cffi:mem-ref value-n-out :unsigned-long))))

(defun get-changed (name)
  "Check if any parameters in the ONELAB database used by the client
`name' have been changed."
  (with-ierr (ierr)
      (gmsh/internal::%onelab-get-changed name ierr)))

(defun set-changed (name value)
  "Set the changed flag to value `value' for all the parameters in the
ONELAB database used by the client `name'."
  (with-ierr (ierr)
      (gmsh/internal::%onelab-set-changed name value ierr)))

(defun clear (&key (name ""))
  "Clear the ONELAB database, or remove a single parameter if `name' is
given."
  (with-ierr (ierr)
      (gmsh/internal::%onelab-clear name ierr)))

(defun run (&key (name "") (command ""))
  "Run a ONELAB client. If `name' is provided, create a new ONELAB client
with name `name' and executes `command'. If not, try to run a client
that might be linked to the processed input files."
  (with-ierr (ierr)
      (gmsh/internal::%onelab-run name command ierr)))

