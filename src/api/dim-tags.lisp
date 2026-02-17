;;;; dim-tags.lisp — Ergonomic constructors and accessors for dim-tag pairs

(in-package :gmsh)

(export '(volume-tag surface-tag curve-tag point-tag
          volume-tags surface-tags curve-tags point-tags
          dim tag tags-of
          volumes-of surfaces-of curves-of points-of))

;;; Constructors — single dim-tag pair

(defun volume-tag (id)
  "Create a volume (dim=3) dim-tag pair."
  (cons 3 id))

(defun surface-tag (id)
  "Create a surface (dim=2) dim-tag pair."
  (cons 2 id))

(defun curve-tag (id)
  "Create a curve (dim=1) dim-tag pair."
  (cons 1 id))

(defun point-tag (id)
  "Create a point (dim=0) dim-tag pair."
  (cons 0 id))

;;; Constructors — lists of dim-tag pairs

(defun volume-tags (ids)
  "Create a list of volume (dim=3) dim-tag pairs from a list of IDs."
  (mapcar (lambda (id) (cons 3 id)) ids))

(defun surface-tags (ids)
  "Create a list of surface (dim=2) dim-tag pairs from a list of IDs."
  (mapcar (lambda (id) (cons 2 id)) ids))

(defun curve-tags (ids)
  "Create a list of curve (dim=1) dim-tag pairs from a list of IDs."
  (mapcar (lambda (id) (cons 1 id)) ids))

(defun point-tags (ids)
  "Create a list of point (dim=0) dim-tag pairs from a list of IDs."
  (mapcar (lambda (id) (cons 0 id)) ids))

;;; Accessors

(defun dim (pair)
  "Extract dimension from a dim-tag pair."
  (car pair))

(defun tag (pair)
  "Extract tag from a dim-tag pair."
  (cdr pair))

(defun tags-of (dim-tags)
  "Extract all tags from a list of dim-tag pairs."
  (mapcar #'cdr dim-tags))

;;; Filters

(defun volumes-of (dim-tags)
  "Return only the volume (dim=3) entities from DIM-TAGS."
  (remove-if-not (lambda (p) (= 3 (car p))) dim-tags))

(defun surfaces-of (dim-tags)
  "Return only the surface (dim=2) entities from DIM-TAGS."
  (remove-if-not (lambda (p) (= 2 (car p))) dim-tags))

(defun curves-of (dim-tags)
  "Return only the curve (dim=1) entities from DIM-TAGS."
  (remove-if-not (lambda (p) (= 1 (car p))) dim-tags))

(defun points-of (dim-tags)
  "Return only the point (dim=0) entities from DIM-TAGS."
  (remove-if-not (lambda (p) (= 0 (car p))) dim-tags))
