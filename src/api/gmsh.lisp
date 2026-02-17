;;;; gmsh.lisp — Lifecycle macros and convenience functions

(in-package :gmsh)

(export '(with-gmsh with-model start-gui with-recording record-call
          translate-geo-line translate-geo-file))

(defmacro with-gmsh ((&rest options) &body body)
  "Initialize gmsh, execute BODY, finalize on exit.
   OPTIONS are keyword arguments passed to INITIALIZE."
  `(progn
     (initialize ,@options)
     (unwind-protect
          (progn ,@body)
       (finalize))))

(defmacro with-model ((name) &body body)
  "Add a named model, execute BODY."
  `(progn
     (add ,name)
     ,@body))

(defun start-gui ()
  "Start the gmsh GUI in a background thread.
   The FLTK event loop runs continuously in the thread.
   Returns the thread object.
   Requires bordeaux-threads."
  (fltk:initialize)
  (bt:make-thread
   (lambda ()
     (loop
       (fltk:wait :time 0.05)
       (sleep 0.01)))
   :name "gmsh-fltk-event-loop"))
