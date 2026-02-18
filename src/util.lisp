(in-package #:cl-hamt)

;; Utility functions for operating on HAMTs.

(defun get-bits (hash depth)
  "Extract bits 5*depth : 5*(depth+1) from the number hash."
  (declare (type integer hash depth))
  (ldb (byte 5 (* 5 depth)) hash))

(defun get-index (bits bitmap)
  "Given the 5-bit int extracted from a hash at the present depth, find
the index in the current array corresponding to this bit sequence."
  (logcount (ldb (byte bits 0) bitmap)))

(defun vec-insert (vec pos item)
  (declare (type simple-vector vec)
           (type fixnum pos))
  (let* ((old-len (length vec))
         (len (1+ old-len))
         (v (make-array len)))
    (declare (type fixnum old-len len)
             (type simple-vector v))
    (replace v vec :end1 pos :end2 pos)
    (setf (svref v pos) item)
    (replace v vec :start1 (1+ pos) :start2 pos)
    v))

(defun vec-remove (vec pos)
  (declare (type simple-vector vec)
           (type fixnum pos))
  (let* ((old-len (length vec))
         (len (1- old-len))
         (v (make-array len)))
    (declare (type fixnum old-len len)
             (type simple-vector v))
    (replace v vec :end1 pos :end2 pos)
    (replace v vec :start1 pos :start2 (1+ pos))
    v))

(defun vec-update (vec pos item)
  (declare (type simple-vector vec)
           (type fixnum pos))
  (let ((v (copy-seq vec)))
    (declare (type simple-vector v))
    (setf (svref v pos) item)
    v))

