;;; msh_attributes.lisp — Setting and reading MSH file attributes

(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate)

(gmsh:set-attribute "Comments" '("Hello this is my comment on the block model"
                                  "... and a second comment"))
;; (gmsh:write "/tmp/block_with_attribute.msh")
