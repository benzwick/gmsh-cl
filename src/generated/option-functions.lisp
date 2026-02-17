;;;; option-functions.lisp — Generated wrappers for gmsh/option
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/option)

(defun set-number (name value)
  "Set a numerical option to `value'. `name' is of the form
\"Category.Option\" or \"Category[num].Option\". Available categories
and options are listed in the \"Gmsh options\" chapter of the Gmsh
reference manual (https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-
options)."
  (with-ierr (ierr)
      (gmsh/internal::%option-set-number name (to-double value) ierr)))

(defun get-number (name)
  "Get the `value' of a numerical option. `name' is of the form
\"Category.Option\" or \"Category[num].Option\". Available categories
and options are listed in the \"Gmsh options\" chapter of the Gmsh
reference manual (https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-
options)."
  (cffi:with-foreign-object (value-out :double)
      (with-ierr (ierr)
      (gmsh/internal::%option-get-number name value-out ierr))
    (cffi:mem-ref value-out :double)))

(defun set-string (name value)
  "Set a string option to `value'. `name' is of the form
\"Category.Option\" or \"Category[num].Option\". Available categories
and options are listed in the \"Gmsh options\" chapter of the Gmsh
reference manual (https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-
options)."
  (with-ierr (ierr)
      (gmsh/internal::%option-set-string name value ierr)))

(defun get-string (name)
  "Get the `value' of a string option. `name' is of the form
\"Category.Option\" or \"Category[num].Option\". Available categories
and options are listed in the \"Gmsh options\" chapter of the Gmsh
reference manual (https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-
options)."
  (cffi:with-foreign-object (value-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%option-get-string name value-out ierr))
    (foreign-string-result (cffi:mem-ref value-out :pointer))))

(defun set-color (name r g b &key (a 255))
  "Set a color option to the RGBA value (`r', `g', `b', `a'), where where
`r', `g', `b' and `a' should be integers between 0 and 255. `name' is
of the form \"Category.Color.Option\" or
\"Category[num].Color.Option\". Available categories and options are
listed in the \"Gmsh options\" chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-options). For
conciseness \"Color.\" can be ommitted in `name'."
  (with-ierr (ierr)
      (gmsh/internal::%option-set-color name r g b a ierr)))

(defun get-color (name)
  "Get the `r', `g', `b', `a' value of a color option. `name' is of the
form \"Category.Color.Option\" or \"Category[num].Color.Option\".
Available categories and options are listed in the \"Gmsh options\"
chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-options). For
conciseness \"Color.\" can be ommitted in `name'."
  (cffi:with-foreign-objects ((r-out :int) (g-out :int) (b-out :int) (a-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%option-get-color name r-out g-out b-out a-out ierr))
    (values (cffi:mem-ref r-out :int) (cffi:mem-ref g-out :int) (cffi:mem-ref b-out :int) (cffi:mem-ref a-out :int))))

(defun restore-defaults ()
  "Restore all options to default settings."
  (with-ierr (ierr)
      (gmsh/internal::%option-restore-defaults  ierr)))

