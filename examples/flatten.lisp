;;; flatten.lisp — Flatten a mesh by setting all z coordinates to 0
;;;
;;; This demonstrates modifying a mesh by getting nodes/elements, modifying,
;;; and storing back. For just changing coordinates, see flatten2.lisp.

;; Create some geometry to have a mesh to work with
(gmsh:add "flatten")
(occ:sphere 0 0 0 1)
(occ:synchronize)
(mesh:generate :dim 2)

(let ((node-data (make-hash-table :test 'equal))
      (elem-data (make-hash-table :test 'equal)))

  ;; Get the nodes and elements
  (dolist (e (gmsh:get-entities))
    (setf (gethash e node-data)
          (multiple-value-list (mesh:get-nodes :dim (car e) :tag (cdr e))))
    (setf (gethash e elem-data)
          (multiple-value-list (mesh:get-elements :dim (car e) :tag (cdr e)))))

  ;; Delete the mesh
  (mesh:clear)

  ;; Store new mesh with z=0
  (maphash (lambda (e nod)
             (let ((coords (second nod)))
               ;; Set every 3rd coordinate (z) to 0
               (loop for i from 2 below (length coords) by 3
                     do (setf (nth i coords) 0.0))
               (mesh:add-nodes (car e) (cdr e) (first nod) coords)
               (let ((el (gethash e elem-data)))
                 (mesh:add-elements (car e) (cdr e)
                                    (first el) (second el) (third el)))))
           node-data))
