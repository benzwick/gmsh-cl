;;; naca_boundary_layer_3d.lisp — 3D NACA airfoil with boundary layer
;;;
;;; Demonstrates geo:extrude-boundary-layer in 3D, building a NACA 0012
;;; airfoil profile with a rounded wing tip, extruding a boundary layer,
;;; and creating a surrounding air domain.
;;; Port of api/naca_boundary_layer_3d.py

(gmsh:add "NACA 0012 with a round tip")

;; Incidence angle and extrusion length
(let* ((incidence (- (/ pi 18)))
       (z 0.63)
       (lc1 0.01)
       (lc2 0.3)
       ;; xy coordinates of top part of NACA 0012 profile
       (naca '(0.9987518 0.0014399 0.9976658 0.0015870 0.9947532 0.0019938
               0.9906850 0.0025595 0.9854709 0.0032804 0.9791229 0.0041519
               0.9716559 0.0051685 0.9630873 0.0063238 0.9534372 0.0076108
               0.9427280 0.0090217 0.9309849 0.0105485 0.9182351 0.0121823
               0.9045085 0.0139143 0.8898372 0.0157351 0.8742554 0.0176353
               0.8577995 0.0196051 0.8405079 0.0216347 0.8224211 0.0237142
               0.8035813 0.0258337 0.7840324 0.0279828 0.7638202 0.0301515
               0.7429917 0.0323294 0.7215958 0.0345058 0.6996823 0.0366700
               0.6773025 0.0388109 0.6545085 0.0409174 0.6313537 0.0429778
               0.6078921 0.0449802 0.5841786 0.0469124 0.5602683 0.0487619
               0.5362174 0.0505161 0.5120819 0.0521620 0.4879181 0.0536866
               0.4637826 0.0550769 0.4397317 0.0563200 0.4158215 0.0574033
               0.3921079 0.0583145 0.3686463 0.0590419 0.3454915 0.0595747
               0.3226976 0.0599028 0.3003177 0.0600172 0.2784042 0.0599102
               0.2570083 0.0595755 0.2361799 0.0590081 0.2159676 0.0582048
               0.1964187 0.0571640 0.1775789 0.0558856 0.1594921 0.0543715
               0.1422005 0.0526251 0.1257446 0.0506513 0.1101628 0.0484567
               0.0954915 0.0460489 0.0817649 0.0434371 0.0690152 0.0406310
               0.0572720 0.0376414 0.0465628 0.0344792 0.0369127 0.0311559
               0.0283441 0.0276827 0.0208771 0.0240706 0.0145291 0.0203300
               0.0093149 0.0164706 0.0052468 0.0125011 0.0023342 0.0084289
               0.0005839 0.0042603 0.0000000 0.0000000)))

  (opt:set-number "Mesh.Algorithm3D" 10)

  ;; Create profile in z=0 plane from points
  (let* ((l (length naca))
         (pts '()))
    ;; Top of profile
    (loop for i from 0 below (/ l 2)
          do (push (occ:point (elt naca (* 2 i)) (elt naca (1+ (* 2 i))) 0) pts))
    ;; Bottom of profile (reversed y)
    (loop for i from (- (/ l 2) 2) downto 0
          do (push (occ:point (elt naca (* 2 i)) (- (elt naca (1+ (* 2 i)))) 0) pts))
    (setf pts (nreverse pts))

    (let* ((spl (occ:spline pts))
           ;; Circle as trailing edge
           (trail-center (occ:point 0.9985510 0.0 0))
           (cir (occ:circle-arc (car (last pts)) trail-center (first pts)))
           ;; Extrude the profile along z
           (ext (occ:extrude (list (cons 1 spl) (cons 1 cir)) 0 0 z)))

      ;; Cut the extruded profile at leading and trailing edge
      (let ((bb (multiple-value-list (occ:get-bounding-box 1 cir))))
        (let* ((p1 (occ:point 0 0 0))
               (p2 (occ:point 0 0 z))
               (c1 (occ:line p1 p2))
               (p3 (occ:point (fourth bb) 0 0))
               (p4 (occ:point (fourth bb) 0 z))
               (c2 (occ:line p3 p4)))
          (occ:fragment (list (cons 1 c1) (cons 1 c2)) (occ:get-entities :dim 2))

          ;; Retrieve tip curves that will be revolved
          (let ((eps 1e-6))
            (opt:set-number "Geometry.OCCBoundsUseStl" 1)
            (occ:synchronize)
            (let* ((tc (gmsh:get-entities-in-bounding-box
                        (- eps) (- eps) (- z eps)
                        (+ (fourth bb) eps) 1 (+ z eps) :dim 1))
                   ;; Create rounded wing tip by revolution
                   (rev (occ:revolve tc 0 0 z 1 0 0 (/ pi 2)))
                   ;; Second 90-degree revolution
                   (rev-surfs (loop for dt in rev
                                    for i from 0
                                    when (= 0 (mod i 4))
                                      collect dt)))
              (occ:revolve rev-surfs 0 0 z 1 0 0 (/ pi 2))

              ;; Glue surfaces, set mesh size and sync
              (occ:fragment (occ:get-entities :dim 2) '())
              (occ:mesh-set-size (occ:get-entities :dim 0) lc1)

              ;; Rotate the profile
              (occ:rotate (occ:get-entities :dim 2) 0.25 0 0 0 0 1 incidence)
              (occ:synchronize)

              ;; Create boundary layer
              (let* ((n-layers 10)
                     (r 2)
                     (d (let ((acc (list 1.7d-5)))
                          (loop for i from 1 below n-layers
                                do (push (+ (car acc)
                                            (* (car (last acc))
                                               (expt r i)))
                                         acc))
                          (nreverse acc)))
                     (num-elts (make-list n-layers :initial-element 1))
                     (extbl (geo:extrude-boundary-layer (gmsh:get-entities :dim 2)
                                                        :num-elements num-elts
                                                        :heights d
                                                        :recombine t)))

                ;; Get "top" surfaces of the boundary layer
                (let ((top '()))
                  (loop for i from 1 below (length extbl)
                        when (= 3 (car (nth i extbl)))
                          do (push (nth (1- i) extbl) top))
                  (setf top (nreverse top))

                  ;; Extract boundary, create symmetry plane
                  (geo:synchronize)
                  (let* ((bnd (gmsh:get-boundary top))
                         (cl2 (geo:curve-loop (gmsh:tags-of bnd)))
                         (p1 (geo:point -1 -1 0 :mesh-size lc2))
                         (p2 (geo:point 2 -1 0 :mesh-size lc2))
                         (p3 (geo:point 2 1 0 :mesh-size lc2))
                         (p4 (geo:point -1 1 0 :mesh-size lc2))
                         (l1 (geo:line p1 p2))
                         (l2 (geo:line p2 p3))
                         (l3 (geo:line p3 p4))
                         (l4 (geo:line p4 p1))
                         (cl3 (geo:curve-loop (list l1 l2 l3 l4)))
                         (s2 (geo:plane-surface (list cl3 cl2))))

                    ;; Create 3D air box
                    (let* ((p11 (geo:point -1 -1 (* 2 z) :mesh-size lc2))
                           (p12 (geo:point 2 -1 (* 2 z) :mesh-size lc2))
                           (p13 (geo:point 2 1 (* 2 z) :mesh-size lc2))
                           (p14 (geo:point -1 1 (* 2 z) :mesh-size lc2))
                           (l11 (geo:line p11 p12))
                           (l12 (geo:line p12 p13))
                           (l13 (geo:line p13 p14))
                           (l14 (geo:line p14 p11))
                           (l-1-11 (geo:line p1 p11))
                           (l-2-12 (geo:line p2 p12))
                           (l-3-13 (geo:line p3 p13))
                           (l-4-14 (geo:line p4 p14))
                           (cl3b (geo:curve-loop (list l11 l12 l13 l14)))
                           (s3 (geo:plane-surface (list cl3b)))
                           (cl4 (geo:curve-loop (list l1 l-2-12 (- l11) (- l-1-11))))
                           (s4 (geo:plane-surface (list cl4)))
                           (cl5 (geo:curve-loop (list l2 l-3-13 (- l12) (- l-2-12))))
                           (s5 (geo:plane-surface (list cl5)))
                           (cl6 (geo:curve-loop (list l3 l-4-14 (- l13) (- l-3-13))))
                           (s6 (geo:plane-surface (list cl6)))
                           (cl7 (geo:curve-loop (list l4 l-1-11 (- l14) (- l-4-14))))
                           (s7 (geo:plane-surface (list cl7))))

                      (let ((box-surfs (append (mapcar #'cdr top)
                                               (list s2 s3 s4 s5 s6 s7))))
                        (let ((sl (geo:surface-loop box-surfs)))
                          (geo:volume (list sl))))
                      (geo:synchronize)

                      (mesh:generate :dim 3))))))))))))
;; (gmsh:write "/tmp/naca_boundary_layer_3d.msh")
