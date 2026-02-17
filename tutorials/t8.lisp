;;; t8.lisp — Post-processing, image export and animations

;; In addition to creating geometries and meshes, the API can also be used
;; to manipulate post-processing datasets (called "views" in Gmsh).

;; We first create a simple geometry
(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (geo:point 0 0.3 0 :mesh-size lc :tag 4))

(geo:line 1 2 :tag 1)
(geo:line 3 2 :tag 2)
(geo:line 3 4 :tag 3)
(geo:line 4 1 :tag 4)

(geo:curve-loop '(4 1 -2 3) :tag 1)
(geo:plane-surface '(1) :tag 1)

(geo:synchronize)

;; We merge some post-processing views to work on
(let ((tutorials-dir (namestring (merge-pathnames "_reference/gmsh/tutorials/"
                                                  (asdf:system-source-directory :gmsh-cl)))))
  (gmsh:merge (concatenate 'string tutorials-dir "view1.pos"))
  (gmsh:merge (concatenate 'string tutorials-dir "view1.pos"))
  (gmsh:merge (concatenate 'string tutorials-dir "view4.pos")))  ; contains 2 views inside

;; Gmsh can read post-processing views in various formats. Here the `view1.pos'
;; and `view4.pos' files are in the Gmsh "parsed" format, which is interpreted by
;; the GEO script parser. The parsed format should only be used for relatively
;; small datasets of course: for larger datasets using e.g. MSH files is much
;; more efficient. Post-processing views can also be created directly from the
;; API.

;; We then set some general options:
(opt:set-number "General.Trackball" 0)
(opt:set-number "General.RotationX" 0)
(opt:set-number "General.RotationY" 0)
(opt:set-number "General.RotationZ" 0)

;; Color options are special
;; Setting a color option of "X.Y" actually sets the option "X.Color.Y"
;; Sets "General.Color.Background", etc.
(opt:set-color "General.Background" 255 255 255)
(opt:set-color "General.Foreground" 0 0 0)
(opt:set-color "General.Text" 0 0 0)

(opt:set-number "General.Orthographic" 0)
(opt:set-number "General.Axes" 0)
(opt:set-number "General.SmallAxes" 0)

;; We also set some options for each post-processing view:

;; If we were to follow the geo example blindly, we would read the number of
;; views from the relevant option value, and use the opt:set-number and
;; opt:set-string functions. A nicer way is to use view:get-tags
;; and to use the view:option-set-number and view:option-set-string functions.
(let ((v (view:get-tags)))
  (when (/= (length v) 4)
    (logger:write "Wrong number of views!" :level "error")
    (error "Wrong number of views!"))

  ;; We set some options for each post-processing view:
  (view:option-set-number (elt v 0) "IntervalsType" 2)
  (view:option-set-number (elt v 0) "OffsetZ" 0.05)
  (view:option-set-number (elt v 0) "RaiseZ" 0)
  (view:option-set-number (elt v 0) "Light" 1)
  (view:option-set-number (elt v 0) "ShowScale" 0)
  (view:option-set-number (elt v 0) "SmoothNormals" 1)

  (view:option-set-number (elt v 1) "IntervalsType" 1)
  ;; Note that we can't yet set the ColorTable in API
  (view:option-set-number (elt v 1) "NbIso" 10)
  (view:option-set-number (elt v 1) "ShowScale" 0)

  (view:option-set-string (elt v 2) "Name" "Test...")
  (view:option-set-number (elt v 2) "Axes" 1)
  (view:option-set-number (elt v 2) "IntervalsType" 2)
  (view:option-set-number (elt v 2) "Type" 2)
  (view:option-set-number (elt v 2) "AutoPosition" 0)
  (view:option-set-number (elt v 2) "PositionX" 85)
  (view:option-set-number (elt v 2) "PositionY" 50)
  (view:option-set-number (elt v 2) "Width" 200)
  (view:option-set-number (elt v 2) "Height" 130)

  (view:option-set-number (elt v 3) "Visible" 0)

  ;; You can save an MPEG movie directly by selecting `File->Export' in the
  ;; GUI. Several predefined animations are setup, for looping on all the time
  ;; steps in views, or for looping between views.

  ;; But the API can be used to build much more complex animations, by changing
  ;; options at run-time and re-rendering the graphics. Each frame can then be
  ;; saved to disk as an image, and multiple frames can be encoded to form a
  ;; movie. Below is an example of such a custom animation.

  (let ((tt 0))  ; initial step
    (loop for num from 1 to 3 do

      ;; Set time step
      (loop for vv in v do
        (view:option-set-number vv "TimeStep" tt))

      (let ((current-step (view:option-get-number (elt v 0) "TimeStep"))
            (max-step (1- (view:option-get-number (elt v 0) "NbTimeStep"))))
        (if (< current-step max-step)
            (incf tt)
            (setf tt 0)))

      (view:option-set-number (elt v 0) "RaiseZ"
                              (+ (view:option-get-number (elt v 0) "RaiseZ")
                                 (* (/ 0.01 (view:option-get-number (elt v 0) "Max"))
                                    tt)))

      (when (= num 3)
        ;; Resize the graphics when num == 3, to create 640x480 frames
        (opt:set-number "General.GraphicsWidth"
                        (+ (opt:get-number "General.MenuWidth") 640))
        (opt:set-number "General.GraphicsHeight" 480))

      (let ((frames 50))
        (loop for num2 from 0 below frames do
          ;; Incrementally rotate the scene
          (opt:set-number "General.RotationX"
                          (+ (opt:get-number "General.RotationX") 10))
          (opt:set-number "General.RotationY"
                          (/ (opt:get-number "General.RotationX") 3))
          (opt:set-number "General.RotationZ"
                          (+ (opt:get-number "General.RotationZ") 0.1))

          ;; Draw the scene
          (gmsh:draw))))))
