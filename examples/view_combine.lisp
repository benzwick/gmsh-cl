;;; view_combine.lisp — Combining multiple views

(let ((tri1 (list 0.0 1.0 1.0  0.0 0.0 1.0  0.0 0.0 0.0))
      (tri2 (list 0.0 1.0 0.0  0.0 1.0 1.0  0.0 0.0 0.0)))

  (loop for step below 10 do
    (setf tri1 (nconc tri1 (list 10.0 10.0 (+ 12.0 step))))
    (setf tri2 (nconc tri2 (list 10.0 (+ 12.0 step) (+ 13.0 step)))))

  (let ((v1 (view:add "some data")))
    (view:add-list-data v1 "ST" 1 tri1)

    (let ((v2 (view:add "some other data")))
      (view:add-list-data v2 "ST" 1 tri2)

      (view:combine "elements" "all" :remove nil)

      (view:add-alias v1))))
