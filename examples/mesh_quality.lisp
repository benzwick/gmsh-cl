;;; mesh_quality.lisp — Computing element quality measures

(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 3)

;; Get element qualities
(multiple-value-bind (types ele-tags node-tags)
    (mesh:get-elements :dim 3)
  (declare (ignore types node-tags))
  (let ((q (mesh:get-element-qualities (first ele-tags) :quality-name "minSICN")))
    (format t "Quality of first element: ~A~%" (first q))))

;; Alternative using plugin
(plugin:set-number "AnalyseMeshQuality" "ICNMeasure" 1.0)
(plugin:set-number "AnalyseMeshQuality" "CreateView" 1.0)
(let ((t-view (plugin:run "AnalyseMeshQuality")))
  (multiple-value-bind (data-type tags data time num-comp)
      (view:get-model-data t-view 0)
    (declare (ignore time num-comp))
    (format t "ICN for element ~A = ~A~%" (first tags) (first data))))
