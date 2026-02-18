
(in-package #:cl-hamt)

(defclass leaf ()
  ((key
    :reader node-key
    :initarg :key
    :initform nil)))

(defclass conflict ()
  ((hash
    :reader conflict-hash
    :initarg :hash)
   (entries
    :reader conflict-entries
    :initarg :entries
    :initform '())))

(defclass table ()
  ((bitmap
    :reader table-bitmap
    :initarg :bitmap
    :initform 0
    :type (unsigned-byte 32))
   (table
    :reader table-array
    :initarg :table
    :initform (make-array 0 :initial-element nil))))

;; Base HAMT class
(defclass hamt ()
  ((test
    :reader hamt-test
    :initarg :test
    :initform #'equal)
   (hash
    :reader hamt-hash
    :initarg :hash
    :initform #'cl-murmurhash:murmurhash)
   (table
    :reader hamt-table
    :initarg :table)))


(defmacro with-hamt (hamt (&key test hash table) &body body)
  "Accessing HAMT slots"
  `(with-accessors ((,test hamt-test)
                    (,hash hamt-hash)
                    (,table hamt-table))
       ,hamt
     ,@body))

(defmacro with-table (node hash depth
                      (bitmap array bits index hit)
                      &body body)
  "Bitshifting in HAMT tables"
  `(let* ((,bitmap (table-bitmap ,node))
          (,array (table-array ,node))
          (,bits (get-bits ,hash ,depth))
          (,index (get-index ,bits ,bitmap))
          (,hit (logbitp ,bits ,bitmap)))
     ,@body))


;; Getting the size of a HAMT
(defgeneric %hamt-size (node))

(defmethod %hamt-size ((node leaf))
  1)

(defmethod %hamt-size ((node table))
  (loop for node across (table-array node)
     sum (%hamt-size node)))

(defmethod %hamt-size ((node conflict))
  (length (conflict-entries node)))


;; Reducing over a HAMT is the same for table nodes of sets and dicts
(defmethod %hamt-reduce (func node initial-value))

(defmethod %hamt-reduce (func (node table) initial-value)
  (reduce (lambda (r child)
            (%hamt-reduce func child r))
          (table-array node)
          :initial-value initial-value))

;; Helpers for defining equality between hash sets/dictionaries
(define-condition incompatible-tests-error (error)
  ())

(defun array-eq (arr1 arr2 test)
  (let ((n (length arr1)))
    (if (not (= n (length arr2)))
        nil
        (do ((i 0 (+ i 1)))
            ((or (= i n)
                 (not (funcall test (elt arr1 i) (elt arr2 i))))
             (= i n))))))
