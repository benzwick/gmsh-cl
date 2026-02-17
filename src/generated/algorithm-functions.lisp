;;;; algorithm-functions.lisp — Generated wrappers for gmsh/algorithm
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/algorithm)

(defun triangulate (coordinates &key (edges '()))
  "Triangulate the points given in the `coordinates' vector as
concatenated pairs of u, v coordinates, with (optional) constrained
edges given in the `edges' vector as pairs of indexes (with numbering
starting at 1), and return the triangles as concatenated triplets of
point indexes (with numbering starting at 1) in `triangles'."
  (with-foreign-array (coordinates-ptr coordinates-n coordinates :double)
      (with-foreign-array (edges-ptr edges-n edges :unsigned-long)
      (cffi:with-foreign-objects ((triangles-out :pointer) (triangles-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%algorithm-triangulate coordinates-ptr coordinates-n triangles-out triangles-n-out edges-ptr edges-n ierr))
    (foreign-array-to-list (cffi:mem-ref triangles-out :pointer) (cffi:mem-ref triangles-n-out :unsigned-long) :unsigned-long)))))

(defun tetrahedralize (coordinates &key (triangles '()))
  "Tetrahedralize the points given in the `coordinates' vector as
concatenated triplets of x, y, z coordinates, with (optional)
constrained triangles given in the `triangles' vector as triplets of
indexes (with numbering starting at 1), and return the tetrahedra as
concatenated quadruplets of point indexes (with numbering starting at
1) in `tetrahedra'. Steiner points might be added in the `steiner'
vector."
  (with-foreign-array (coordinates-ptr coordinates-n coordinates :double)
      (with-foreign-array (triangles-ptr triangles-n triangles :unsigned-long)
      (cffi:with-foreign-objects ((tetrahedra-out :pointer) (tetrahedra-n-out :unsigned-long) (steiner-out :pointer) (steiner-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%algorithm-tetrahedralize coordinates-ptr coordinates-n tetrahedra-out tetrahedra-n-out steiner-out steiner-n-out triangles-ptr triangles-n ierr))
    (values (foreign-array-to-list (cffi:mem-ref tetrahedra-out :pointer) (cffi:mem-ref tetrahedra-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref steiner-out :pointer) (cffi:mem-ref steiner-n-out :unsigned-long) :double))))))

