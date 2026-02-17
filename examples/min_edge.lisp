;;; min_edge.lisp — Find the element with the shortest edge

(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate)

(multiple-value-bind (types tags node-tags)
    (mesh:get-elements :dim 3)
  (declare (ignore types node-tags))
  (let ((qual (mesh:get-element-qualities (first tags) :quality-name "minEdge")))
    ;; Find the element with the minimum edge length
    (let ((min-q (reduce #'min qual))
          (min-idx (position (reduce #'min qual) qual)))
      (format t "Min edge: ~A (element ~A)~%" min-q (nth min-idx (first tags))))))
