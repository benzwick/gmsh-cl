;;;; mesh-helpers.lisp — Convenience macros for mesh operations

(in-package :gmsh/mesh)

(export '(define-size-callback))

(defmacro define-size-callback (name (dim tag x y z lc) &body body)
  "Define and register a mesh size callback.
   NAME is the callback symbol. DIM, TAG, X, Y, Z, LC are the parameter names
   for element dimension, element tag, spatial coordinates, and current mesh size.
   BODY should return a double-float mesh size value.

   Example:
     (mesh:define-size-callback my-callback (dim tag x y z lc)
       (declare (ignore dim tag y z))
       (min lc (+ (* 0.02d0 x) 0.01d0)))"
  `(progn
     (cffi:defcallback ,name :double ((,dim :int) (,tag :int)
                                      (,x :double) (,y :double)
                                      (,z :double) (,lc :double))
       ,@body)
     (set-size-callback (cffi:callback ,name))))
