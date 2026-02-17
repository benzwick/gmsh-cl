;;;; geo.lisp — Batch geometry creation helpers for the built-in CAD kernel

(in-package :gmsh/geo)

(export '(points lines line-loop polygon surfaces))

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

(defun polygon (coords &key (size 0) (tag -1))
  "Create a planar polygon from COORDS, a list of (x y z) triples.
   Creates points, lines, a curve loop, and a plane surface.
   Returns (values surface-tag curve-loop-tag point-tags line-tags)."
  (let* ((pts (points coords :size size))
         (n (length pts))
         (lns (loop for i below n
                    collect (line (nth i pts) (nth (mod (1+ i) n) pts))))
         (cl (curve-loop lns))
         (s (plane-surface (list cl) :tag tag)))
    (values s cl pts lns)))

(defun surfaces (curve-loop-tags &key (tag -1))
  "Create plane surfaces from a list of curve-loop tags.
   If TAG is specified, it's used for the first surface only.
   Returns list of surface tags."
  (loop for cl in curve-loop-tags
        for first = t then nil
        collect (plane-surface (list cl) :tag (if first tag -1))))
