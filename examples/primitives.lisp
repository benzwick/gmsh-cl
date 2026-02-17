;;; primitives.lisp — All OpenCASCADE primitive shapes in one file
;;;
;;; Demonstrates every OCC primitive: sphere, cylinder, box, torus, cone,
;;; wedge, rectangle, disk — with various parameter combinations.
;;; Port of boolean/primitives.geo

(gmsh:add "primitives")

(opt:set-number "Mesh.MeshSizeMin" 0.1)
(opt:set-number "Mesh.MeshSizeMax" 0.1)

;; 3D primitives — one column per shape
(let ((x 0))
  ;; Spheres with different angular extents
  (occ:sphere (prog1 x (incf x)) 0 0 0.3)
  (occ:sphere (prog1 x (incf x)) 0 0 0.3 :angle3 (/ pi 4))
  (occ:sphere (prog1 x (incf x)) 0 0 0.3 :angle1 (- (/ pi 4)) :angle2 (/ pi 4))
  (occ:sphere (prog1 x (incf x)) 0 0 0.3 :angle1 (- (/ pi 4)) :angle2 (/ pi 4)
              :angle3 (/ pi 2))
  (occ:sphere (prog1 x (incf x)) 0 0 0.3 :angle1 (- (/ pi 2)) :angle2 (/ pi 2)
              :angle3 (/ pi 4))

  ;; Cylinders
  (occ:cylinder (prog1 x (incf x)) 0 0 0.5 0 0 0.5)
  (occ:cylinder (prog1 x (incf x)) 0 0 0.5 0 0 0.5 :angle (/ pi 3))

  ;; Box
  (occ:box (prog1 x (incf x)) 0 0 0.5 0.5 0.5)

  ;; Torus variants
  (occ:torus (prog1 x (incf x)) 0 0 0.3 0.1)
  (occ:torus (prog1 x (incf x)) 0 0 0.3 0.1 :angle (/ pi 3))

  ;; Cones
  (occ:cone (prog1 x (incf x)) 0 0 0.5 0 0 0.5 0)
  (occ:cone (prog1 x (incf x)) 0 0 0.5 0 0 0.5 0 :angle (/ pi 3))
  (occ:cone (prog1 x (incf x)) 0 0 0.5 0 0 0.5 0.2 :angle (/ pi 3))

  ;; Wedges
  (occ:wedge (prog1 x (incf x)) 0 0 0.5 0.5 0.5)
  (occ:wedge (prog1 x (incf x)) 0 0 0.5 0.5 0.5 :ltx 0.8))

;; 2D primitives
(let ((x 0)
      (y -1.5))
  ;; Rectangles
  (occ:rectangle (prog1 x (incf x)) y 0 0.5 0.5)
  (occ:rectangle (prog1 x (incf x)) y 0 0.5 0.5 :rounded-radius 0.1)

  ;; Disks
  (occ:disk (prog1 x (incf x)) y 0 0.3 0.3)
  (occ:disk (prog1 x (incf x)) y 0 0.4 0.2))

(occ:synchronize)
(mesh:generate :dim 3)
;; (gmsh:write "/tmp/primitives.msh")
