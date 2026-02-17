;;; t9.lisp — Plugins

;; Plugins can be added to Gmsh in order to extend its capabilities. For example,
;; post-processing plugins can modify views, or create new views based on
;; previously loaded views. Several default plugins are statically linked with
;; Gmsh, e.g. Isosurface, CutPlane, CutSphere, Skin, Transform or Smooth.
;;
;; Plugins can be controlled through the API functions with the `plugin'
;; prefix, or from the graphical interface (right click on the view button, then
;; `Plugins').

;; Let us for example include a three-dimensional scalar view:
(gmsh:merge (namestring (merge-pathnames "_reference/gmsh/tutorials/view3.pos"
                                         (asdf:system-source-directory :gmsh-cl))))

(let ((v (view:get-tags)))
  (when (/= (length v) 1)
    (logger:write "Wrong number of views!" :level "error")
    (error "Wrong number of views!"))

  ;; We then set some options for the `Isosurface' plugin (which extracts an
  ;; isosurface from a 3D scalar view), and run it:
  (plugin:set-number "Isosurface" "Value" 0.67)
  (plugin:set-number "Isosurface" "View" 0)
  (let ((v1 (plugin:run "Isosurface")))

    ;; We also set some options for the `CutPlane' plugin (which computes a
    ;; section of a 3D view using the plane A*x+B*y+C*z+D=0), and then run it:
    (plugin:set-number "CutPlane" "A" 0)
    (plugin:set-number "CutPlane" "B" 0.2)
    (plugin:set-number "CutPlane" "C" 1)
    (plugin:set-number "CutPlane" "D" 0)
    (plugin:set-number "CutPlane" "View" 0)
    (let ((v2 (plugin:run "CutPlane")))

      ;; Add a title (By convention, for window coordinates a value greater than
      ;; 99999 represents the center. We could also use
      ;; `General.GraphicsWidth / 2', but that would only center the string for
      ;; the current window size.):
      (plugin:set-string "Annotate" "Text" "A nice title")
      (plugin:set-number "Annotate" "X" 1.0e5)
      (plugin:set-number "Annotate" "Y" 50)
      (plugin:set-string "Annotate" "Font" "Times-BoldItalic")
      (plugin:set-number "Annotate" "FontSize" 28)
      (plugin:set-string "Annotate" "Align" "Center")
      (plugin:set-number "Annotate" "View" 0)
      (plugin:run "Annotate")

      (plugin:set-string "Annotate" "Text" "(and a small subtitle)")
      (plugin:set-number "Annotate" "Y" 70)
      (plugin:set-string "Annotate" "Font" "Times-Roman")
      (plugin:set-number "Annotate" "FontSize" 12)
      (plugin:run "Annotate")

      ;; We finish by setting some options:
      (view:option-set-number (elt v 0) "Light" 1)
      (view:option-set-number (elt v 0) "IntervalsType" 1)
      (view:option-set-number (elt v 0) "NbIso" 6)
      (view:option-set-number (elt v 0) "SmoothNormals" 1)
      (view:option-set-number v1 "IntervalsType" 2)
      (view:option-set-number v2 "IntervalsType" 2))))
