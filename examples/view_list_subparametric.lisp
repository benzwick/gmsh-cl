;;; view_list_subparametric.lisp — Quadratic field on a linear segment

(let ((tag (view:add "Quadratic field on a linear segment")))

  ;; Coordinates of the linear segment (2 nodes) in physical space
  (let ((segment (list -2.0 3.0      ; x
                        9.0 14.0     ; y
                        0.0 0.0)))   ; z
    ;; Append the 3 function values
    (setf segment (nconc segment (list 1.0 3.5 -1.0)))

    ;; Quadratic field basis (3 functions)
    (let ((p (list 0 0 0  1 0 0  2 0 0))
          (phi (list 0.0 -0.5  0.5
                     0.0  0.5  0.5
                     1.0  0.0 -1.0))
          ;; Linear geometry basis (2 functions)
          (p-g (list 0 0 0  1 0 0))
          (phi-g (list 0.5 -0.5    ; phi_1 = 1/2*(1 - xi)
                       0.5  0.5))) ; phi_2 = 1/2*(1 + xi)
      (view:set-interpolation-matrices tag "Line" 3 phi p :d-geo 2 :coef-geo phi-g :exp-geo p-g))

    (view:add-list-data tag "SL" 1 segment))

  ;; Adaptive visualization
  (view:option-set-number tag "AdaptVisualizationGrid" 1)
  (view:option-set-number tag "TargetError" -1)
  (view:option-set-number tag "MaxRecursionLevel" 7))
