
(in-package #:cl-hamt)

;; Internal HAMT nodes are represented as structs for cheaper allocation and
;; access in hot paths. Public wrapper types (hash-set/hash-dict) remain CLOS.
(defstruct (set-leaf (:constructor make-set-leaf (&key key)))
  key)

(defstruct (set-conflict (:constructor make-set-conflict (&key hash entries)))
  hash
  (entries '()))

(defstruct (set-table (:constructor make-set-table (&key (bitmap 0) (table (make-array 0)))))
  (bitmap 0 :type (unsigned-byte 64))
  (table (make-array 0) :type simple-vector))

(defstruct (dict-leaf (:constructor make-dict-leaf (&key key value)))
  key
  value)

(defstruct (dict-conflict (:constructor make-dict-conflict (&key hash entries)))
  hash
  (entries '()))

(defstruct (dict-table (:constructor make-dict-table (&key (bitmap 0) (table (make-array 0)))))
  (bitmap 0 :type (unsigned-byte 64))
  (table (make-array 0) :type simple-vector))

;; Base HAMT class
(defclass hamt ()
  ((test
    :reader hamt-test
    :initarg :test
    :initform #'equal)
   (hash
   :reader hamt-hash
   :initarg :hash
    :initform #'xxhash64-object)
   (table
    :reader hamt-table
    :initarg :table)))


(defmacro with-hamt (hamt (&key test hash table) &body body)
  "Accessing HAMT slots"
  (let ((bindings '()))
    (when test
      (push `(,test hamt-test) bindings))
    (when hash
      (push `(,hash hamt-hash) bindings))
    (when table
      (push `(,table hamt-table) bindings))
    `(with-accessors ,(nreverse bindings)
         ,hamt
       ,@body)))

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

(defmacro with-typed-table (node hash depth
                            (bitmap array bits index hit)
                            &body body)
  `(with-table ,node ,hash ,depth (,bitmap ,array ,bits ,index ,hit)
     (declare (type (unsigned-byte 64) ,bitmap)
              (type simple-vector ,array)
              (type fixnum ,bits ,index))
     ,@body))

(defmacro rewrite-table-update (table-constructor bitmap array index new-node)
  `(,table-constructor
    :bitmap ,bitmap
    :table (vec-update ,array ,index ,new-node)))

(defmacro rewrite-table-insert (table-constructor bitmap bits array index new-node)
  `(,table-constructor
    :bitmap (logior ,bitmap (ash 1 ,bits))
    :table (vec-insert ,array ,index ,new-node)))

(defmacro rewrite-table-remove-or-empty (table-constructor bitmap bits array index)
  `(if (= ,bitmap (ash 1 ,bits))
       nil
       (,table-constructor
        :bitmap (logxor ,bitmap (ash 1 ,bits))
        :table (vec-remove ,array ,index))))

(defmacro with-conflict-removal-scan ((entries entry matches-p)
                                      (kept kept-count removed)
                                      &body body)
  "Scan ENTRIES, removing the first element satisfying MATCHES-P.
Short-circuits after the first match so duplicate entries are preserved."
  `(let ((,kept '())
         (,kept-count 0)
         (,removed nil))
     (dolist (,entry ,entries)
       (if (and (not ,removed) ,matches-p)
           (setf ,removed t)
           (progn
             (incf ,kept-count)
             (push ,entry ,kept))))
     ,@body))

(defmacro with-conflict-equality-check ((node1 node2 conflict-type)
                                        (entries1 entries2)
                                        &body body)
  `(and (typep ,node2 ',conflict-type)
        (equal (conflict-hash ,node1) (conflict-hash ,node2))
        (let ((,entries1 (conflict-entries ,node1))
              (,entries2 (conflict-entries ,node2)))
          (and (= (length ,entries1) (length ,entries2))
               ,@body))))

(defmacro with-table-equality-check ((node1 node2 table-type)
                                     (child1 child2)
                                     &body body)
  `(and (typep ,node2 ',table-type)
        (equal (table-bitmap ,node1) (table-bitmap ,node2))
        (array-eq (table-array ,node1)
                  (table-array ,node2)
                  (lambda (,child1 ,child2)
                    ,@body))))

(declaim (inline node-key node-value conflict-hash conflict-entries table-bitmap table-array))

(defun node-key (node)
  (typecase node
    (set-leaf (set-leaf-key node))
    (dict-leaf (dict-leaf-key node))
    (t nil)))

(defun node-value (node)
  (typecase node
    (dict-leaf (dict-leaf-value node))
    (t nil)))

(defun conflict-hash (node)
  (typecase node
    (set-conflict (set-conflict-hash node))
    (dict-conflict (dict-conflict-hash node))
    (t 0)))

(defun conflict-entries (node)
  (typecase node
    (set-conflict (set-conflict-entries node))
    (dict-conflict (dict-conflict-entries node))
    (t '())))

(defun table-bitmap (node)
  (typecase node
    (set-table (set-table-bitmap node))
    (dict-table (dict-table-bitmap node))
    (t 0)))

(defun table-array (node)
  (typecase node
    (set-table (set-table-table node))
    (dict-table (dict-table-table node))
    (t #())))


;; Getting the size of a HAMT
(defun %hamt-size (node)
  (typecase node
    ((or set-leaf dict-leaf) 1)
    ((or set-conflict dict-conflict)
     (length (conflict-entries node)))
    ((or set-table dict-table)
     (loop for child across (table-array node)
           sum (%hamt-size child)))
    (t 0)))


;; Reducing over a HAMT is the same for table nodes of sets and dicts
(defun %hamt-reduce (func node initial-value)
  (typecase node
    (set-leaf
     (funcall func initial-value (set-leaf-key node)))
    (dict-leaf
     (funcall func initial-value (dict-leaf-key node) (dict-leaf-value node)))
    (set-conflict
     (reduce func (set-conflict-entries node) :initial-value initial-value))
    (dict-conflict
     (labels ((f (alist r)
                (if alist
                    (f (cdr alist)
                       (funcall func r (caar alist) (cdar alist)))
                    r)))
       (f (dict-conflict-entries node) initial-value)))
    ((or set-table dict-table)
     (reduce (lambda (r child)
               (%hamt-reduce func child r))
             (table-array node)
             :initial-value initial-value))
    (t initial-value)))

;; Helpers for defining equality between hash sets/dictionaries
(define-condition incompatible-tests-error (error)
  ())

(defun array-eq (arr1 arr2 test)
  (declare (type simple-vector arr1 arr2))
  (let ((n (length arr1)))
    (declare (type fixnum n))
    (if (not (= n (length arr2)))
        nil
        (loop for i fixnum from 0 below n
              always (funcall test (aref arr1 i) (aref arr2 i))))))
