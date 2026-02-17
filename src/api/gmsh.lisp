;;;; gmsh.lisp — Lifecycle macros and convenience functions

(in-package :gmsh)

(export '(with-gmsh with-model start-gui stop-gui with-gui-lock
          with-recording record-call
          translate-geo-line translate-geo-file))

(defvar *gui-running* nil
  "Set to T when the GUI event loop is running, NIL to stop it.")

(defvar *recording-stream* nil
  "When non-nil, a stream to which CL API calls are written.")

(defvar *recording-active* nil
  "When non-nil, API calls are being recorded.")

(defmacro with-gmsh ((&rest options) &body body)
  "Initialize gmsh, execute BODY, finalize on exit.
   OPTIONS are keyword arguments passed to INITIALIZE.
   On SBCL, masks floating-point traps that gmsh's C code may trigger."
  `(progn
     (initialize ,@options)
     (unwind-protect
          #+sbcl (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
                   ,@body)
          #-sbcl (progn ,@body)
       (stop-gui)
       (finalize))))

(defmacro with-model ((name) &body body)
  "Add a named model, execute BODY."
  `(progn
     (add ,name)
     ,@body))

(defun start-gui (&key (block t))
  "Start the Gmsh GUI. FLTK requires the event loop on the main thread.
   If BLOCK is T (default), calls fltk:run which blocks until the window is closed.
   If BLOCK is NIL, starts a REPL in a background thread and runs FLTK on the
   main thread. Closing the window returns control to the caller.
   When *recording-stream* is active, REPL commands are also recorded."
  (fltk:initialize)
  (if block
      (fltk:run)
      (progn
        (setf *gui-running* t)
        (let* ((rec-stream *recording-stream*)
               (rec-active *recording-active*)
               (repl-thread
                (bt:make-thread
                 (lambda ()
                   (let ((*recording-stream* rec-stream)
                         (*recording-active* rec-active))
                     #+sbcl (sb-ext:enable-debugger)
                     (format t "~&;; gmsh REPL — GUI is open. Type (gmsh:stop-gui) to close.~%")
                     (loop while *gui-running*
                           do (format t "~&gmsh> ")
                              (finish-output)
                              (handler-case
                                  (let* ((line (read-line *standard-input* nil nil))
                                         (trimmed (if line
                                                      (string-trim '(#\Space #\Tab) line)
                                                      "")))
                                    (unless line (return))
                                    (unless (zerop (length trimmed))
                                      (when (and *recording-active* *recording-stream*)
                                        (format *recording-stream* ";;+~%~A~%" trimmed)
                                        (force-output *recording-stream*))
                                      (let ((vals (multiple-value-list
                                                   (eval (read-from-string trimmed)))))
                                        (dolist (v vals)
                                          (format t "~S~%" v)))
                                      ;; Thread-safe GUI refresh via FLTK lock
                                      (ignore-errors
                                       (fltk:lock)
                                       (unwind-protect
                                            (progn
                                              (ignore-errors (geo:synchronize))
                                              (ignore-errors (occ:synchronize)))
                                         (fltk:unlock)
                                         (fltk:awake :action "update")))))
                                (error (e) (format t "~&Error: ~A~%" e))))))
                 :name "gmsh-repl")))
          (declare (ignore repl-thread))
          (unwind-protect
               (loop while *gui-running*
                     do (handler-case
                            (progn
                              (fltk:wait :time 0.05)
                              (when (zerop (fltk:is-available))
                                (setf *gui-running* nil)))
                          (gmsh/internal:gmsh-error ()
                            (setf *gui-running* nil))))
            (setf *gui-running* nil))))))

(defun stop-gui ()
  "Stop the GUI event loop."
  (setf *gui-running* nil))

(defmacro with-gui-lock (() &body body)
  "Execute BODY with the FLTK GUI lock held, then awake the event loop.
   Use this to safely update GUI state from threads other than the FLTK event loop."
  `(progn
     (fltk:lock)
     (unwind-protect (progn ,@body)
       (fltk:unlock)
       (fltk:awake :action "update"))))
