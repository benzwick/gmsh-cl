;;;; library.lisp — Load libgmsh shared library
;;;;
;;;; *** DO NOT EDIT — part of gmsh-cl core infrastructure ***

(in-package :gmsh/internal)

(cffi:define-foreign-library libgmsh
  (:unix (:or "libgmsh.so" "libgmsh.so.4" "libgmsh.so.4.14"))
  (:darwin (:or "libgmsh.dylib" "libgmsh.4.dylib"))
  (:windows (:or "gmsh.dll" "gmsh-4.14.dll"))
  (t (:default "libgmsh")))

(cffi:use-foreign-library libgmsh)

;;; gmshFree / gmshMalloc — needed to free output arrays allocated by gmsh
(cffi:defcfun ("gmshFree" %free) :void
  (p :pointer))

(cffi:defcfun ("gmshMalloc" %malloc) :pointer
  (n :unsigned-long))
