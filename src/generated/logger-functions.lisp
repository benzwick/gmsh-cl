;;;; logger-functions.lisp — Generated wrappers for gmsh/logger
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/logger)

(defun write (message &key (level "info"))
  "Write a `message'. `level' can be \"info\", \"warning\" or \"error\"."
  (with-ierr (ierr)
      (gmsh/internal::%logger-write message level ierr)))

(defun start ()
  "Start logging messages."
  (with-ierr (ierr)
      (gmsh/internal::%logger-start  ierr)))

(defun get ()
  "Get logged messages."
  (cffi:with-foreign-objects ((log-out :pointer) (log-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%logger-get log-out log-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref log-out :pointer) (cffi:mem-ref log-n-out :unsigned-long))))

(defun stop ()
  "Stop logging messages."
  (with-ierr (ierr)
      (gmsh/internal::%logger-stop  ierr)))

(defun get-wall-time ()
  "Return wall clock time (in s)."
  (with-ierr (ierr)
      (gmsh/internal::%logger-get-wall-time  ierr)))

(defun get-cpu-time ()
  "Return CPU time (in s)."
  (with-ierr (ierr)
      (gmsh/internal::%logger-get-cpu-time  ierr)))

(defun get-memory ()
  "Return memory usage (in Mb)."
  (with-ierr (ierr)
      (gmsh/internal::%logger-get-memory  ierr)))

(defun get-total-memory ()
  "Return total available memory (in Mb)."
  (with-ierr (ierr)
      (gmsh/internal::%logger-get-total-memory  ierr)))

(defun get-last-error ()
  "Return last error message, if any."
  (cffi:with-foreign-object (error-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%logger-get-last-error error-out ierr))
    (foreign-string-result (cffi:mem-ref error-out :pointer))))

