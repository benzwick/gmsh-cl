;;; mirror_mesh.lisp — Mirror a mesh to create symmetric copies

;; Create a simple non-uniform mesh of a rectangle
(occ:rectangle 0 0 0 1 0.5)
(occ:synchronize)
(mesh:set-size (gmsh:get-entities :dim 0) 0.1)
(mesh:set-size '((0 . 2)) 0.01)
(mesh:generate :dim 3)

;; Get the mesh data
(let ((m (make-hash-table :test 'equal)))
  (dolist (e (gmsh:get-entities))
    (setf (gethash e m)
          (list (gmsh:get-boundary (list e))
                (multiple-value-list (mesh:get-nodes :dim (car e) :tag (cdr e)))
                (multiple-value-list (mesh:get-elements :dim (car e) :tag (cdr e))))))

  ;; Transform the mesh and create new discrete entities to store it
  (labels ((transform (offset-entity offset-node offset-element tx ty tz)
             (let ((sorted-keys (sort (loop for k being the hash-keys of m collect k)
                                      (lambda (a b)
                                        (or (< (car a) (car b))
                                            (and (= (car a) (car b))
                                                 (< (cdr a) (cdr b))))))))
               (dolist (e sorted-keys)
                 (let ((entry (gethash e m)))
                   (destructuring-bind (bnd nod ele) entry
                     (gmsh:add-discrete-entity
                      (car e) :tag (+ (cdr e) offset-entity)
                      :boundary (mapcar (lambda (b)
                                          (* (+ (abs (cdr b)) offset-entity)
                                             (if (minusp (cdr b)) -1 1)))
                                        bnd))
                     (let ((coord (loop for i from 0 below (length (second nod)) by 3
                                        append (list (* (nth i (second nod)) tx)
                                                     (* (nth (1+ i) (second nod)) ty)
                                                     (* (nth (+ i 2) (second nod)) tz)))))
                       (mesh:add-nodes (car e) (+ (cdr e) offset-entity)
                                       (mapcar (lambda (n) (+ n offset-node)) (first nod))
                                       coord))
                     (mesh:add-elements
                      (car e) (+ (cdr e) offset-entity)
                      (first ele)
                      (mapcar (lambda (typ)
                                (mapcar (lambda (t-val) (+ t-val offset-element)) typ))
                              (second ele))
                      (mapcar (lambda (typ)
                                (mapcar (lambda (n) (+ n offset-node)) typ))
                              (third ele)))
                     (when (minusp (* tx ty tz))
                       (mesh:reverse :dim-tags (list (cons (car e)
                                                           (+ (cdr e) offset-entity)))))))))))

    (transform 1000 1000000 1000000 -1 1 1)
    (transform 2000 2000000 2000000 1 -1 1)
    (transform 3000 3000000 3000000 -1 -1 1))

  ;; Remove duplicate nodes on internal boundaries
  (mesh:remove-duplicate-nodes))
