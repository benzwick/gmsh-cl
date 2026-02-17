;;; step_header_data.lisp — Set STEP header data and export

(occ:torus 0 0 0 1 0.2)
(occ:synchronize)

(opt:set-string "Geometry.OCCSTEPDescription" "A simple torus geometry")
(opt:set-string "Geometry.OCCSTEPModelName" "Torus")
(opt:set-string "Geometry.OCCSTEPAuthor" "gnikit")
(opt:set-string "Geometry.OCCSTEPOrganization" "Gmsh")
(opt:set-string "Geometry.OCCSTEPPreprocessorVersion" "Gmsh")
(opt:set-string "Geometry.OCCSTEPOriginatingSystem" "-")
(opt:set-string "Geometry.OCCSTEPAuthorization" "")

;; (gmsh:write "/tmp/step_header_data.stp")
