;;; view_list_superparametric.lisp — Linear field on a quadratic segment

(let ((tag (view:add "Linear field on a quadratic segment")))

  ;; Coordinates of the quadratic segment (3 nodes) in physical space
  (let ((segment (list -2.0 3.0 -1.0      ; x
                        9.0 14.0 12.5     ; y
                        0.0 0.0 0.0)))    ; z
    ;; Append the 2 function values
    (setf segment (nconc segment (list 1.0 3.5)))

    ;; Linear field basis (2 functions)
    (let ((p (list 0 0 0  1 0 0))
          (phi (list 0.5 -0.5    ; phi_1 = 1/2*(1 - xi)
                     0.5  0.5))  ; phi_2 = 1/2*(1 + xi)
          ;; Quadratic geometry basis (3 functions)
          (p-g (list 0 0 0  1 0 0  2 0 0))
          (phi-g (list 0.0 -0.5  0.5
                       0.0  0.5  0.5
                       1.0  0.0 -1.0)))
      (view:set-interpolation-matrices tag "Line" 2 phi p :d-geo 3 :coef-geo phi-g :exp-geo p-g))

    (view:add-list-data tag "SL" 1 segment))

  ;; Adaptive visualization
  (view:option-set-number tag "AdaptVisualizationGrid" 1)
  (view:option-set-number tag "TargetError" -1)
  (view:option-set-number tag "MaxRecursionLevel" 7))
