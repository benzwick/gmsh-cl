;;;; plugin-functions.lisp — Generated wrappers for gmsh/plugin
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/plugin)

(defun set-number (name option value)
  "Set the numerical option `option' to the value `value' for plugin
`name'. Plugins available in the official Gmsh release are listed in
the \"Gmsh plugins\" chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-plugins)."
  (with-ierr (ierr)
      (gmsh/internal::%plugin-set-number name option (to-double value) ierr)))

(defun set-string (name option value)
  "Set the string option `option' to the value `value' for plugin `name'.
Plugins available in the official Gmsh release are listed in the
\"Gmsh plugins\" chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-plugins)."
  (with-ierr (ierr)
      (gmsh/internal::%plugin-set-string name option value ierr)))

(defun run (name)
  "Run the plugin `name'. Return the tag of the created view (if any).
Plugins available in the official Gmsh release are listed in the
\"Gmsh plugins\" chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-plugins)."
  (with-ierr (ierr)
      (gmsh/internal::%plugin-run name ierr)))

