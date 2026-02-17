;;; view_list_isoparametric.lisp — Isoparametric quadratic field on quadratic segment

(let ((tag (view:add "Quadratic field on a quadratic segment")))

  ;; Coordinates of the quadratic segment (3 nodes) in physical space
  (let ((segment (list -2.0 3.0 -1.0      ; x
                       9.0 14.0 12.5      ; y
                       0.0 0.0 0.0)))     ; z
    ;; Append the 3 function values
    (setf segment (nconc segment (list 1.0 3.5 -1.0)))

    ;; Define the canonical polynomial basis for the field interpolation
    (let ((p (list 0 0 0    ; xi^0
                   1 0 0    ; xi^1
                   2 0 0))  ; xi^2
          ;; From monomial to Lagrange basis
          (phi (list 0.0 -0.5  0.5    ; phi_1 at xi=-1
                     0.0  0.5  0.5    ; phi_2 at xi= 1
                     1.0  0.0 -1.0))) ; phi_3 at xi= 0
      ;; Isoparametric: same basis for geometry and field
      (view:set-interpolation-matrices tag "Line" 3 phi p :d-geo 3 :coef-geo phi :exp-geo p))

    (view:add-list-data tag "SL" 1 segment))

  ;; Adaptive visualization
  (view:option-set-number tag "AdaptVisualizationGrid" 1)
  (view:option-set-number tag "TargetError" 1e-3)
  (view:option-set-number tag "MaxRecursionLevel" 7))
