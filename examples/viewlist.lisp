;;; viewlist.lisp — List-based view creation

(let ((tri1 (list 0.0 1.0 1.0  0.0 0.0 1.0  0.0 0.0 0.0))
      (tri2 (list 0.0 1.0 0.0  0.0 1.0 1.0  0.0 0.0 0.0)))

  (loop for step below 10 do
    (setf tri1 (nconc tri1 (list 10.0 10.0 (+ 12.0 step))))
    (setf tri2 (nconc tri2 (list 10.0 (+ 12.0 step) (+ 13.0 step)))))

  (let ((v (view:add "some data")))
    (view:add-list-data v "ST" 2 (append tri1 tri2))
    ;; (view:write v "data.pos")
    ))
