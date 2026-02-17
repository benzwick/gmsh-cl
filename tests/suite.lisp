;;;; suite.lisp — Test suite definition, helpers, and macros

(in-package :gmsh-cl/tests)

(def-suite :gmsh-cl
  :description "All gmsh-cl tests")

(def-suite :gmsh-cl/core
  :description "Core lifecycle and error handling"
  :in :gmsh-cl)

(def-suite :gmsh-cl/geo
  :description "Built-in geometry kernel"
  :in :gmsh-cl)

(def-suite :gmsh-cl/occ
  :description "OpenCASCADE geometry kernel"
  :in :gmsh-cl)

(def-suite :gmsh-cl/mesh
  :description "Meshing"
  :in :gmsh-cl)

(def-suite :gmsh-cl/view
  :description "Post-processing views"
  :in :gmsh-cl)

(def-suite :gmsh-cl/options
  :description "Options"
  :in :gmsh-cl)

(def-suite :gmsh-cl/tutorials
  :description "Tutorial integration tests"
  :in :gmsh-cl)

;;; --- Paths ---

(defvar *project-root*
  (asdf:system-source-directory :gmsh-cl)
  "Root directory of the gmsh-cl project.")

(defun tutorial-path (name)
  "Return absolute path to a tutorial file."
  (namestring (merge-pathnames (format nil "tutorials/~A" name) *project-root*)))

(defun tutorial-data-path (name)
  "Return absolute path to a tutorial data file in the gmsh reference."
  (namestring (merge-pathnames (format nil "_reference/gmsh/tutorials/~A" name) *project-root*)))

(defun example-path (name)
  "Return absolute path to an example file."
  (namestring (merge-pathnames (format nil "examples/~A" name) *project-root*)))

;;; --- Test macro ---

(defmacro with-gmsh-test (() &body body)
  "Initialize gmsh, run BODY, finalize on exit. Each test gets a clean gmsh state.
   On SBCL, masks floating-point traps that gmsh's C code may trigger."
  `(progn
     (gmsh:initialize)
     (unwind-protect
          #+sbcl (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
                   ,@body)
          #-sbcl (progn ,@body)
       (gmsh:finalize))))

;;; --- Runner ---

(defun run-tests ()
  "Run all gmsh-cl tests. Returns T if all passed."
  (let ((results (fiveam:run :gmsh-cl)))
    (fiveam:explain! results)
    (fiveam:results-status results)))
