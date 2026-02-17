;;;; util.lisp — Helpers for generated CFFI wrappers
;;;;
;;;; *** DO NOT EDIT — part of gmsh-cl core infrastructure ***

(in-package :gmsh/internal)

;;; ---------------------------------------------------------------------------
;;; Error checking
;;; ---------------------------------------------------------------------------

(defmacro with-ierr ((ierr) &body body)
  "Allocate an int for ierr, run BODY, signal gmsh-error if ierr != 0.
   Returns the value of the last form in BODY."
  `(cffi:with-foreign-object (,ierr :int)
     (setf (cffi:mem-ref ,ierr :int) 0)
     (let ((result (progn ,@body)))
       (let ((err-code (cffi:mem-ref ,ierr :int)))
         (unless (zerop err-code)
           (error 'gmsh-error :code err-code
                              :message (format nil "gmsh returned error code ~D" err-code))))
       result)))

;;; ---------------------------------------------------------------------------
;;; Type coercion
;;; ---------------------------------------------------------------------------

(declaim (inline to-double))
(defun to-double (x)
  "Coerce X to double-float."
  (coerce x 'double-float))

;;; ---------------------------------------------------------------------------
;;; Input array helpers
;;; ---------------------------------------------------------------------------

(defmacro with-foreign-array ((ptr size sequence element-type) &body body)
  "Convert a CL SEQUENCE to a temporary C array of ELEMENT-TYPE.
   Binds PTR to the foreign pointer and SIZE to the element count."
  (let ((seq (gensym "SEQ"))
        (i (gensym "I")))
    `(let* ((,seq ,sequence)
            (,size (length ,seq)))
       (cffi:with-foreign-object (,ptr ,element-type ,size)
         (loop for ,i from 0
               for elt in ,seq
               do (setf (cffi:mem-aref ,ptr ,element-type ,i)
                        ,(case element-type
                           (:double `(to-double elt))
                           (otherwise 'elt))))
         ,@body))))

(defun pairs-to-foreign (pairs ptr-place size-place)
  "Convert a list of (dim . tag) cons cells to a flat int array [d0 t0 d1 t1 ...].
   Sets PTR-PLACE to a newly gmshMalloc'd pointer and SIZE-PLACE to the total int count."
  (let* ((n (length pairs))
         (total (* 2 n))
         (p (%malloc (* total (cffi:foreign-type-size :int)))))
    (loop for pair in pairs
          for i from 0 by 2
          do (setf (cffi:mem-aref p :int i) (car pair))
             (setf (cffi:mem-aref p :int (1+ i)) (cdr pair)))
    (setf (cffi:mem-ref ptr-place :pointer) p)
    (setf (cffi:mem-ref size-place :unsigned-long) total)))

(defmacro with-pairs-array ((ptr size pairs) &body body)
  "Convert a list of (dim . tag) cons cells to a temporary flat int array.
   Binds PTR to the pointer and SIZE to total int count (2 * n-pairs)."
  (let ((ps (gensym "PAIRS"))
        (n (gensym "N"))
        (i (gensym "I")))
    `(let* ((,ps ,pairs)
            (,n (length ,ps)))
       (cffi:with-foreign-object (,ptr :int (* 2 ,n))
         (let ((,size (* 2 ,n)))
           (loop for pair in ,ps
                 for ,i from 0 by 2
                 do (setf (cffi:mem-aref ,ptr :int ,i) (car pair))
                    (setf (cffi:mem-aref ,ptr :int (1+ ,i)) (cdr pair)))
           ,@body)))))

(defmacro with-string-array ((ptr size strings) &body body)
  "Convert a list of CL strings to a temporary array of C string pointers.
   Binds PTR to the pointer and SIZE to the count."
  (let ((ss (gensym "STRINGS"))
        (n (gensym "N"))
        (i (gensym "I")))
    `(let* ((,ss ,strings)
            (,n (length ,ss)))
       (cffi:with-foreign-object (,ptr :pointer ,n)
         (let ((,size ,n))
           (unwind-protect
                (progn
                  (loop for s in ,ss
                        for ,i from 0
                        do (setf (cffi:mem-aref ,ptr :pointer ,i)
                                 (cffi:foreign-string-alloc s)))
                  ,@body)
             (loop for ,i below ,n
                   for p = (cffi:mem-aref ,ptr :pointer ,i)
                   unless (cffi:null-pointer-p p)
                     do (cffi:foreign-string-free p))))))))

(defmacro with-vector-vector-int ((ptrs sizes nn lists) &body body)
  "Convert a list of lists of ints to a temporary array-of-arrays.
   Binds PTRS to int**, SIZES to size_t*, NN to count."
  (let ((ll (gensym "LISTS"))
        (n (gensym "N"))
        (i (gensym "I")))
    `(let* ((,ll ,lists)
            (,n (length ,ll)))
       (cffi:with-foreign-object (,ptrs :pointer ,n)
         (cffi:with-foreign-object (,sizes :unsigned-long ,n)
           (let ((,nn ,n))
             (loop for sub in ,ll
                   for ,i from 0
                   do (let ((sub-n (length sub)))
                        (setf (cffi:mem-aref ,sizes :unsigned-long ,i) sub-n)
                        (let ((p (cffi:foreign-alloc :int :count sub-n)))
                          (setf (cffi:mem-aref ,ptrs :pointer ,i) p)
                          (loop for val in sub
                                for j from 0
                                do (setf (cffi:mem-aref p :int j) val)))))
             (unwind-protect
                  (progn ,@body)
               (loop for ,i below ,n
                     do (cffi:foreign-free (cffi:mem-aref ,ptrs :pointer ,i))))))))))

(defmacro with-vector-vector-double ((ptrs sizes nn lists) &body body)
  "Convert a list of lists of doubles to a temporary array-of-arrays.
   Binds PTRS to double**, SIZES to size_t*, NN to count."
  (let ((ll (gensym "LISTS"))
        (n (gensym "N"))
        (i (gensym "I")))
    `(let* ((,ll ,lists)
            (,n (length ,ll)))
       (cffi:with-foreign-object (,ptrs :pointer ,n)
         (cffi:with-foreign-object (,sizes :unsigned-long ,n)
           (let ((,nn ,n))
             (loop for sub in ,ll
                   for ,i from 0
                   do (let ((sub-n (length sub)))
                        (setf (cffi:mem-aref ,sizes :unsigned-long ,i) sub-n)
                        (let ((p (cffi:foreign-alloc :double :count sub-n)))
                          (setf (cffi:mem-aref ,ptrs :pointer ,i) p)
                          (loop for val in sub
                                for j from 0
                                do (setf (cffi:mem-aref p :double j)
                                         (to-double val))))))
             (unwind-protect
                  (progn ,@body)
               (loop for ,i below ,n
                     do (cffi:foreign-free (cffi:mem-aref ,ptrs :pointer ,i))))))))))

(defmacro with-vector-vector-size ((ptrs sizes nn lists) &body body)
  "Convert a list of lists of sizes to a temporary array-of-arrays."
  (let ((ll (gensym "LISTS"))
        (n (gensym "N"))
        (i (gensym "I")))
    `(let* ((,ll ,lists)
            (,n (length ,ll)))
       (cffi:with-foreign-object (,ptrs :pointer ,n)
         (cffi:with-foreign-object (,sizes :unsigned-long ,n)
           (let ((,nn ,n))
             (loop for sub in ,ll
                   for ,i from 0
                   do (let ((sub-n (length sub)))
                        (setf (cffi:mem-aref ,sizes :unsigned-long ,i) sub-n)
                        (let ((p (cffi:foreign-alloc :unsigned-long :count sub-n)))
                          (setf (cffi:mem-aref ,ptrs :pointer ,i) p)
                          (loop for val in sub
                                for j from 0
                                do (setf (cffi:mem-aref p :unsigned-long j) val)))))
             (unwind-protect
                  (progn ,@body)
               (loop for ,i below ,n
                     do (cffi:foreign-free (cffi:mem-aref ,ptrs :pointer ,i))))))))))

;;; ---------------------------------------------------------------------------
;;; Output array helpers — all call %free on the gmsh-allocated memory
;;; ---------------------------------------------------------------------------

(defun foreign-array-to-list (ptr n type)
  "Convert a gmsh-allocated C array to a CL list and free the array."
  (unwind-protect
       (loop for i below n collect (cffi:mem-aref ptr type i))
    (%free ptr)))

(defun dim-tags-to-pairs (ptr n)
  "Convert a flat int array [d0 t0 d1 t1 ...] (N total ints) to a list
   of (dim . tag) cons cells. Frees the gmsh-allocated array."
  (unwind-protect
       (loop for i below n by 2
             collect (cons (cffi:mem-aref ptr :int i)
                           (cffi:mem-aref ptr :int (1+ i))))
    (%free ptr)))

(defun foreign-string-result (ptr)
  "Extract a string from a gmsh-allocated char*, free it, return CL string."
  (let ((s (cffi:foreign-string-to-lisp ptr)))
    (%free ptr)
    s))

(defun foreign-string-array-to-list (ptr n)
  "Convert a gmsh-allocated array of n char* pointers to a CL list of strings.
   Frees each string and the array itself."
  (unwind-protect
       (loop for i below n
             for sp = (cffi:mem-aref ptr :pointer i)
             collect (let ((s (cffi:foreign-string-to-lisp sp)))
                       (%free sp)
                       s))
    (%free ptr)))

(defun foreign-vectors-to-list (ptrs sizes nn type)
  "Convert an array-of-arrays (ptrs[nn], sizes[nn]) to a CL list of lists.
   Frees each sub-array and the ptrs/sizes arrays."
  (unwind-protect
       (loop for i below nn
             for sub-ptr = (cffi:mem-aref ptrs :pointer i)
             for sub-n = (cffi:mem-aref sizes :unsigned-long i)
             collect (loop for j below sub-n
                          collect (cffi:mem-aref sub-ptr type j)
                          finally (%free sub-ptr)))
    (%free ptrs)
    (%free sizes)))

(defun foreign-vector-pairs-to-list (ptrs sizes nn)
  "Convert an array-of-pair-arrays to a list of lists of (dim . tag) pairs.
   Each sub-array has sizes[i] ints (= 2 * n-pairs). Frees everything."
  (unwind-protect
       (loop for i below nn
             for sub-ptr = (cffi:mem-aref ptrs :pointer i)
             for sub-n = (cffi:mem-aref sizes :unsigned-long i)
             collect (loop for j below sub-n by 2
                          collect (cons (cffi:mem-aref sub-ptr :int j)
                                        (cffi:mem-aref sub-ptr :int (1+ j)))
                          finally (%free sub-ptr)))
    (%free ptrs)
    (%free sizes)))
