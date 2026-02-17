;;;; test-tutorials.lisp — Integration tests that load tutorial files

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/tutorials)

;;; Each test loads a tutorial inside with-gmsh-test and verifies it
;;; completes without error.

(test tutorial-t1
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t1.lisp")))))

(test tutorial-t2
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t2.lisp")))))

(test tutorial-t3
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t3.lisp")))))

(test tutorial-t4
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t4.lisp")))))

(test tutorial-t5
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t5.lisp")))))

(test tutorial-t6
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t6.lisp")))))

(test tutorial-t7
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t7.lisp")))))

(test tutorial-t8
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t8.lisp")))))

(test tutorial-t9
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t9.lisp")))))

(test tutorial-t10
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t10.lisp")))))

(test tutorial-t11
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t11.lisp")))))

(test tutorial-t12
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t12.lisp")))))

(test tutorial-t13
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t13.lisp")))))

(test tutorial-t14
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t14.lisp")))))

(test tutorial-t15
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t15.lisp")))))

(test tutorial-t16
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t16.lisp")))))

(test tutorial-t17
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t17.lisp")))))

(test tutorial-t18
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t18.lisp")))))

(test tutorial-t19
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t19.lisp")))))

(test tutorial-t20
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t20.lisp")))))

(test tutorial-t21
  (with-gmsh-test ()
    (finishes (load (tutorial-path "t21.lisp")))))

(test tutorial-x1
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x1.lisp")))))

(test tutorial-x2
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x2.lisp")))))

(test tutorial-x3
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x3.lisp")))))

(test tutorial-x4
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x4.lisp")))))

(test tutorial-x5
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x5.lisp")))))

(test tutorial-x6
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x6.lisp")))))

(test tutorial-x7
  (with-gmsh-test ()
    (finishes (load (tutorial-path "x7.lisp")))))
