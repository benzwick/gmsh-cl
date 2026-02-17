;;; t14.lisp — Homology and cohomology computation

;; Homology computation in Gmsh finds representative chains of (relative)
;; (co)homology space bases using a mesh of a model. The representative basis
;; chains are stored in the mesh as physical groups of Gmsh, one for each chain.

;; Create an example geometry
(gmsh:add "t14")

(let ((m 0.5)   ; mesh size
      (h 2))    ; geometry height in the z-direction

  (geo:point 0 0 0 :mesh-size m :tag 1)
  (geo:point 10 0 0 :mesh-size m :tag 2)
  (geo:point 10 10 0 :mesh-size m :tag 3)
  (geo:point 0 10 0 :mesh-size m :tag 4)

  (geo:point 4 4 0 :mesh-size m :tag 5)
  (geo:point 6 4 0 :mesh-size m :tag 6)
  (geo:point 6 6 0 :mesh-size m :tag 7)
  (geo:point 4 6 0 :mesh-size m :tag 8)

  (geo:point 2 0 0 :mesh-size m :tag 9)
  (geo:point 8 0 0 :mesh-size m :tag 10)
  (geo:point 2 10 0 :mesh-size m :tag 11)
  (geo:point 8 10 0 :mesh-size m :tag 12)

  (geo:line 1 9 :tag 1)
  (geo:line 9 10 :tag 2)
  (geo:line 10 2 :tag 3)

  (geo:line 2 3 :tag 4)
  (geo:line 3 12 :tag 5)
  (geo:line 12 11 :tag 6)

  (geo:line 11 4 :tag 7)
  (geo:line 4 1 :tag 8)
  (geo:line 5 6 :tag 9)

  (geo:line 6 7 :tag 10)
  (geo:line 7 8 :tag 11)
  (geo:line 8 5 :tag 12)

  (geo:curve-loop '(6 7 8 1 2 3 4 5) :tag 13)
  (geo:curve-loop '(11 12 9 10) :tag 14)
  (geo:plane-surface '(13 14) :tag 15)

  (let ((e (geo:extrude '((2 . 15)) 0 0 h)))

    (geo:synchronize)

    ;; Create physical groups, which are used to define the domain of the
    ;; (co)homology computation and the subdomain of the relative (co)homology
    ;; computation.

    ;; Whole domain
    (let* ((domain-tag (cdr (nth 1 e)))
           (domain-physical-tag 1001)

           ;; Four "terminals" of the model
           (terminal-tags (list (cdr (nth 3 e))
                                (cdr (nth 5 e))
                                (cdr (nth 7 e))
                                (cdr (nth 9 e))))
           (terminals-physical-tag 2001))

      (gmsh:add-physical-group 3 (list domain-tag)
                               :tag domain-physical-tag
                               :name "Whole domain")

      (gmsh:add-physical-group 2 terminal-tags
                               :tag terminals-physical-tag
                               :name "Terminals")

      ;; Find domain boundary tags
      (let* ((boundary-dimtags (gmsh:get-boundary (list (cons 3 domain-tag))
                                                   :combined nil
                                                   :oriented nil))
             (boundary-tags (mapcar #'cdr boundary-dimtags))
             (complement-tags (remove-if (lambda (tag) (member tag terminal-tags))
                                         boundary-tags))
             (boundary-physical-tag 2002)
             (complement-physical-tag 2003))

        ;; Whole domain surface
        (gmsh:add-physical-group 2 boundary-tags
                                 :tag boundary-physical-tag
                                 :name "Boundary")

        ;; Complement of the domain surface with respect to the four terminals
        (gmsh:add-physical-group 2 complement-tags
                                 :tag complement-physical-tag
                                 :name "Complement")

        ;; Find bases for relative homology spaces of the domain modulo the four
        ;; terminals.
        (mesh:add-homology-request :type "Homology"
                                   :domain-tags (list domain-physical-tag)
                                   :subdomain-tags (list terminals-physical-tag)
                                   :dims '(0 1 2 3))

        ;; Find homology space bases isomorphic to the previous bases: homology
        ;; spaces modulo the non-terminal domain surface, a.k.a the thin cuts.
        (mesh:add-homology-request :type "Homology"
                                   :domain-tags (list domain-physical-tag)
                                   :subdomain-tags (list complement-physical-tag)
                                   :dims '(0 1 2 3))

        ;; Find cohomology space bases isomorphic to the previous bases: cohomology
        ;; spaces of the domain modulo the four terminals, a.k.a the thick cuts.
        (mesh:add-homology-request :type "Cohomology"
                                   :domain-tags (list domain-physical-tag)
                                   :subdomain-tags (list terminals-physical-tag)
                                   :dims '(0 1 2 3))))))

;; Generate the mesh and perform the requested homology computations
(mesh:generate :dim 3)

;; For more information, see M. Pellikka, S. Suuriniemi, L. Kettunen and
;; C. Geuzaine. Homology and cohomology computation in finite element
;; modeling. SIAM Journal on Scientific Computing 35(5), pp. 1195-1214, 2013.

;; (gmsh:write "/tmp/t14.msh")
