(defsystem "gmsh-cl"
  :version "0.1.0"
  :description "Common Lisp interface for Gmsh"
  :license "GPL-2.0+"
  :depends-on ("cffi" "bordeaux-threads")
  :serial t
  :components
  ((:module "src"
    :components
    ((:module "generated"
      :components
      ((:file "packages")))
     (:module "core"
      :components
      ((:file "library")
       (:file "conditions")
       (:file "util")))
     (:module "generated-rest"
      :pathname "generated"
      :components
      ((:file "bindings")
       (:file "gmsh-functions")
       (:file "geo-functions")
       (:file "geo-mesh")
       (:file "occ-functions")
       (:file "occ-mesh")
       (:file "mesh-functions")
       (:file "view-functions")
       (:file "option-functions")
       (:file "plugin-functions")
       (:file "fltk-functions")
       (:file "onelab-functions")
       (:file "logger-functions")
       (:file "parser-functions")
       (:file "algorithm-functions")
       (:file "graphics-functions")))
     (:module "api"
      :components
      ((:file "gmsh")
       (:file "geo")
       (:file "occ")
       (:file "recording")))))))

(defsystem "gmsh-cl/tests"
  :description "Tests for gmsh-cl"
  :depends-on ("gmsh-cl" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :components
    ((:file "package")
     (:file "suite")
     (:file "test-core")
     (:file "test-geo")
     (:file "test-occ")
     (:file "test-mesh")
     (:file "test-view")
     (:file "test-options")
     (:file "test-tutorials")))))
