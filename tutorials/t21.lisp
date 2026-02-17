;;; t21.lisp — Mesh partitioning

;; Gmsh can partition meshes using different algorithms, e.g. the graph
;; partitioner Metis or the `SimplePartition' plugin. For all the partitioning
;; algorithms, the relationship between mesh elements and mesh partitions is
;; encoded through the creation of new (discrete) elementary entities, called
;; "partition entities".
;;
;; Partition entities behave exactly like other discrete elementary entities; the
;; only difference is that they keep track of both a mesh partition index and
;; their parent elementary entity.
;;
;; The major advantage of this approach is that it allows to maintain a full
;; boundary representation of the partition entities, which Gmsh creates
;; automatically if `Mesh.PartitionCreateTopology' is set.

;; Let us start by creating a simple geometry with two adjacent squares sharing
;; an edge:
(gmsh:add "t21")
(occ:rectangle 0 0 0 1 1 :tag 1)
(occ:rectangle 1 0 0 1 1 :tag 2)
(occ:fragment '((2 . 1)) '((2 . 2)))
(occ:synchronize)
(mesh:set-size (gmsh:get-entities :dim 0) 0.05)

;; We create one physical group for each square, and we mesh the resulting
;; geometry:
(gmsh:add-physical-group 2 '(1) :tag 100 :name "Left")
(gmsh:add-physical-group 2 '(2) :tag 200 :name "Right")
(mesh:generate :dim 2)

;; We now define several ONELAB parameters to fine-tune how the mesh will be
;; partitioned:
(onelab:set "[
  {
    \"type\":\"number\",
    \"name\":\"Parameters/0Mesh partitioner\",
    \"values\":[0],
    \"choices\":[0, 1],
    \"valueLabels\":{\"Metis\":0, \"SimplePartition\":1}
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/1Number of partitions\",
    \"values\":[3],
    \"min\":1,
    \"max\":256,
    \"step\":1
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/2Create partition topology (BRep)?\",
    \"values\":[1],
    \"choices\":[0, 1]
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/3Create ghost cells?\",
    \"values\":[0],
    \"choices\":[0, 1]
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/3Create new physical groups?\",
    \"values\":[0],
    \"choices\":[0, 1]
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/3Write file to disk?\",
    \"values\":[1],
    \"choices\":[0, 1]
  },
  {
    \"type\":\"number\",
    \"name\":\"Parameters/4Write one file per partition?\",
    \"values\":[0],
    \"choices\":[0, 1]
  }
]")

(flet ((partition-mesh ()
         ;; Number of partitions
         (let ((n (truncate (first (onelab:get-number
                                    "Parameters/1Number of partitions")))))

           ;; Should we create the boundary representation of the partition
           ;; entities?
           (opt:set-number "Mesh.PartitionCreateTopology"
                           (first (onelab:get-number
                                   "Parameters/2Create partition topology (BRep)?")))

           ;; Should we create ghost cells?
           (opt:set-number "Mesh.PartitionCreateGhostCells"
                           (first (onelab:get-number
                                   "Parameters/3Create ghost cells?")))

           ;; Should we automatically create new physical groups on the
           ;; partition entities?
           (opt:set-number "Mesh.PartitionCreatePhysicals"
                           (first (onelab:get-number
                                   "Parameters/3Create new physical groups?")))

           ;; Should we keep backward compatibility with pre-Gmsh 4, e.g. to
           ;; save the mesh in MSH2 format?
           (opt:set-number "Mesh.PartitionOldStyleMsh2" 0)

           ;; Should we save one mesh file per partition?
           (opt:set-number "Mesh.PartitionSplitMeshFiles"
                           (first (onelab:get-number
                                   "Parameters/4Write one file per partition?")))

           (if (= (first (onelab:get-number "Parameters/0Mesh partitioner")) 0)
               ;; Use Metis to create N partitions
               (mesh:partition n)
               ;; Use the `SimplePartition' plugin to create chessboard-like
               ;; partitions
               (progn
                 (plugin:set-number "SimplePartition" "NumSlicesX" n)
                 (plugin:set-number "SimplePartition" "NumSlicesY" 1)
                 (plugin:set-number "SimplePartition" "NumSlicesZ" 1)
                 (plugin:run "SimplePartition")))

           ;; Save mesh file (or files, if `Mesh.PartitionSplitMeshFiles' is
           ;; set):
           ;; (when (= (first (onelab:get-number
           ;;                  "Parameters/3Write file to disk?")) 1)
           ;;   (gmsh:write "/tmp/t21.msh"))

           ;; Iterate over partitioned entities and print some info (see the
           ;; first extended tutorial `x1.py' for additional information):
           (let ((entities (gmsh:get-entities)))
             (dolist (e entities)
               (let ((partitions (gmsh:get-partitions (car e) (cdr e))))
                 (when (plusp (length partitions))
                   (format t "Entity ~A of type ~A~%"
                           e (gmsh:get-type (car e) (cdr e)))
                   (format t " - Partition(s): ~A~%" partitions)
                   (multiple-value-bind (parent-dim parent-tag)
                       (gmsh:get-parent (car e) (cdr e))
                     (format t " - Parent: (~A ~A)~%" parent-dim parent-tag))
                   (format t " - Boundary: ~A~%"
                           (gmsh:get-boundary (list e))))))))))
  (partition-mesh))
