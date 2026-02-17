;;;; geo.lisp — Batch geometry creation helpers for the built-in CAD kernel

(in-package :gmsh/geo)

(export '(points lines line-loop))

(defun points (coords &key (size 0))
  "Create multiple points. COORDS is a list of (x y z) triples.
   Returns list of tags."
  (mapcar (lambda (c)
            (point (first c) (second c) (third c) :mesh-size size))
          coords))

(defun lines (&rest point-pairs)
  "Create multiple lines. Each arg is (start-tag end-tag).
   Returns list of tags."
  (mapcar (lambda (pair)
            (line (first pair) (second pair)))
          point-pairs))

(defun line-loop (point-tags &key (tag -1))
  "Create lines connecting consecutive points (closing the loop),
   then create a curve loop from them. Returns the curve-loop tag."
  (let* ((n (length point-tags))
         (line-tags (loop for i below n
                         collect (line (nth i point-tags)
                                      (nth (mod (1+ i) n) point-tags)))))
    (curve-loop line-tags :tag tag)))
