;;; split_window.lisp — FLTK split window and multi-view GUI
;;;
;;; Demonstrates fltk:split-current-window to create multi-pane views.
;;; Note: This example only exercises the API calls; it does not display
;;; a GUI (would need fltk:run for interactive use).
;;; Inspired by split_window.py

(gmsh:add "split_window")

;; Create two simple models in separate views
(occ:box 0 0 0 1 1 1 :tag 1)
(occ:sphere 2 0 0 0.5 :tag 2)
(occ:synchronize)

;; Create a post-processing view
(let ((v1 (view:add "scalar field")))
  ;; Add some simple list data for visualization
  (view:add-list-data v1 "ST" 1
                      ;; Triangle: 3 points (x y z each) + 3 values
                      '(0 0 0  1 0 0  0.5 1 0  0.1 0.5 0.9)))

(mesh:generate :dim 3)
(format t "Model created with ~A entities~%"
        (length (gmsh:get-entities)))
;; (gmsh:write "/tmp/split_window.msh")

;; To actually see the split window, uncomment:
;; (fltk:initialize)
;; (fltk:split-current-window "h" 0.5)
;; (fltk:set-current-window 0)
;; (opt:set-number "General.GraphicsWidth" 800)
;; (opt:set-number "General.GraphicsHeight" 600)
;; (fltk:run)
