;;;; fltk-functions.lisp — Generated wrappers for gmsh/fltk
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/fltk)

(defun initialize ()
  "Create the FLTK graphical user interface. Can only be called in the
main thread."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-initialize  ierr)))

(defun finalize ()
  "Close the FLTK graphical user interface. Can only be called in the
main thread."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-finalize  ierr)))

(defun wait (&key (time -1.0))
  "Wait at most `time' seconds for user interface events and return. If
`time' < 0, wait indefinitely. First automatically create the user
interface if it has not yet been initialized. Can only be called in
the main thread."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-wait (to-double time) ierr)))

(defun update ()
  "Update the user interface (potentially creating new widgets and
windows). First automatically create the user interface if it has not
yet been initialized. Can only be called in the main thread: use
`awake(\"update\")' to trigger an update of the user interface from
another thread."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-update  ierr)))

(defun awake (&key (action ""))
  "Awake the main user interface thread and process pending events, and
optionally perform an action (currently the only `action' allowed is
\"update\")."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-awake action ierr)))

(defun lock ()
  "Block the current thread until it can safely modify the user
interface."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-lock  ierr)))

(defun unlock ()
  "Release the lock that was set using lock."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-unlock  ierr)))

(defun run (&key (option-file-name ""))
  "Run the event loop of the graphical user interface, i.e. repeatedly
call `wait()'. First automatically create the user interface if it has
not yet been initialized. If an `optionFileName' is given, load it
before entering the loop, and save all options and visibility
information into it after exiting the loop. Can only be called in the
main thread."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-run option-file-name ierr)))

(defun is-available ()
  "Check if the user interface is available (e.g. to detect if it has
been closed)."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-is-available  ierr)))

(defun select-entities (&key (dim -1))
  "Select entities in the user interface. Return the selected entities as
a vector of (dim, tag) pairs. If `dim' is >= 0, return only the
entities of the specified dimension (e.g. points if `dim' == 0)."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%fltk-select-entities dim-tags-out dim-tags-n-out dim ierr))))

(defun select-elements ()
  "Select elements in the user interface."
  (cffi:with-foreign-objects ((element-tags-out :pointer) (element-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%fltk-select-elements element-tags-out element-tags-n-out ierr))))

(defun select-views ()
  "Select views in the user interface."
  (cffi:with-foreign-objects ((view-tags-out :pointer) (view-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%fltk-select-views view-tags-out view-tags-n-out ierr))))

(defun split-current-window (&key (how "v") (ratio 0.5))
  "Split the current window horizontally (if `how' == \"h\") or
vertically (if `how' == \"v\"), using ratio `ratio'. If `how' ==
\"u\", restore a single window."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-split-current-window how (to-double ratio) ierr)))

(defun set-current-window (&key (window-index 0))
  "Set the current window by speficying its index (starting at 0) in the
list of all windows. When new windows are created by splits, new
windows are appended at the end of the list."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-set-current-window window-index ierr)))

(defun set-status-message (message &key (graphics nil))
  "Set a status message in the current window. If `graphics' is set,
display the message inside the graphic window instead of the status
bar."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-set-status-message message (if graphics 1 0) ierr)))

(defun show-context-window (dim tag)
  "Show context window for the entity of dimension `dim' and tag `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-show-context-window dim tag ierr)))

(defun open-tree-item (name)
  "Open the `name' item in the menu tree."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-open-tree-item name ierr)))

(defun close-tree-item (name)
  "Close the `name' item in the menu tree."
  (with-ierr (ierr)
      (gmsh/internal::%fltk-close-tree-item name ierr)))

