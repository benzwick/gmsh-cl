;;;; fields.lisp — Declarative mesh size field setup

(in-package :gmsh/mesh)

(export '(field))

(defun field (type &key (tag -1) numbers strings number-lists as-background as-boundary-layer)
  "Create and configure a mesh size field in one call.

   TYPE is the field type string (e.g. \"Distance\", \"Threshold\", \"Box\", \"Min\").
   TAG is the field tag (-1 for automatic assignment).
   NUMBERS is a list of (name value) pairs, set via field-set-number.
   STRINGS is a list of (name value) pairs, set via field-set-string.
   NUMBER-LISTS is a list of (name list) pairs, set via field-set-numbers.
   AS-BACKGROUND when true, sets this field as the background mesh.
   AS-BOUNDARY-LAYER when true, sets this field as a boundary layer.

   Returns the field tag.

   Example:
     (mesh:field \"Threshold\" :tag 2
       :numbers '((\"InField\" 1) (\"SizeMin\" 0.005) (\"SizeMax\" 0.15)
                  (\"DistMin\" 0.15) (\"DistMax\" 0.5)))"
  (let ((f (field-add type :tag tag)))
    (dolist (e numbers)
      (field-set-number f (first e) (second e)))
    (dolist (e strings)
      (field-set-string f (first e) (second e)))
    (dolist (e number-lists)
      (field-set-numbers f (first e) (second e)))
    (when as-background
      (field-set-as-background-mesh f))
    (when as-boundary-layer
      (field-set-as-boundary-layer f))
    f))
