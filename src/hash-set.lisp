(in-package #:cl-hamt)

(defun %set-lookup-node (node key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (set-leaf
     (funcall test (node-key node) key))
    (set-conflict
     (member key (conflict-entries node) :test test))
    (set-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (when hit
         (%set-lookup-node (aref array index) key hash (1+ depth) test))))
    (t nil)))

;; Adding a new element to a leaf node either returns the leaf node if that
;; item was already present in the set, or creates a conflict node if there
;; was a hash collision.
(defun %set-insert-node (node key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (set-leaf
     (let ((nkey (node-key node)))
       (if (funcall test key nkey)
           node
           (make-set-conflict
            :hash hash
            :entries (list key nkey)))))
    (set-conflict
     (let ((entries (conflict-entries node)))
       (if (member key entries :test test)
           node
           (make-set-conflict
            :hash hash
            :entries (cons key entries)))))
    (set-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (if hit
           (let* ((old-node (aref array index))
                  (new-node (%set-insert-node old-node key hash (1+ depth) test)))
             (if (eq new-node old-node)
                 node
                 (make-set-table
                  :bitmap bitmap
                  :table (vec-update array index new-node))))
           (let ((new-node (if (= depth 6)
                               (make-set-leaf :key key)
                               (%set-insert-node (make-set-table)
                                                 key
                                                 hash
                                                 (1+ depth)
                                                 test))))
             (make-set-table
              :bitmap (logior bitmap (ash 1 bits))
              :table (vec-insert array index new-node))))))
    (t node)))

(defun %set-remove-node (node key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (set-leaf
     (unless (funcall test key (node-key node))
       node))
    (set-conflict
     (let ((kept '())
           (kept-count 0)
           (removed nil))
       (dolist (entry (conflict-entries node))
         (if (funcall test key entry)
             (setf removed t)
             (progn
               (incf kept-count)
               (push entry kept))))
       (cond
         ((not removed) node)
         ((= kept-count 1)
          (make-set-leaf
           :key (car kept)))
         (t (make-set-conflict
             :hash hash
             :entries (nreverse kept))))))
    (set-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (if (not hit)
           node
           (let* ((old-node (aref array index))
                  (new-node (%set-remove-node old-node key hash (1+ depth) test)))
             (cond
               ((eq new-node old-node) node)
               (new-node
                (make-set-table
                 :bitmap bitmap
                 :table (vec-update array index new-node)))
               ((= bitmap 1) nil)
               (t (make-set-table
                   :bitmap (logxor bitmap (ash 1 bits))
                   :table (vec-remove array index))))))))
    (t node)))



;; Wrapper set class
(defclass hash-set (hamt)
  ((table
    :reader hamt-table
    :initarg :table
    :initform (make-set-table))))

(defun empty-set (&key (test #'equal) hash (hash-mode :fast))
  "Return an empty hash-set, in which elements will be compared and hashed
with the supplied test and hash functions. The hash must be a 32-bit hash.
If HASH is not supplied, HASH-MODE chooses:
  :FAST   - xxHash32 over sxhash
  :KEYED  - SipHash-2-4 over sxhash (keyed mixer)
  :SECURE - SipHash-2-4 over serialized object bytes.
If HASH is supplied, it must return an unsigned 32-bit integer."
  (make-instance 'hash-set
                 :test (ctypecase test
                         (function test)
                         (symbol (symbol-function test)))
                 :hash (resolve-hash-function hash hash-mode)))

(defun set-lookup (set x)
  "Return true if the object x is in the set, false otherwise"
  (with-hamt set (:test test :hash hash :table table)
    (%set-lookup-node table x (funcall hash x) 0 test)))

(defun set-size (set)
  "Return the size of the set"
  (%hamt-size (hamt-table set)))

(defun set-insert (set &rest xs)
  "Return a new set with the elements xs added to it. Elements already in
the set are ignored."
  (with-hamt set (:test test :hash hash :table table)
    (flet ((%insert (table x)
             (%set-insert-node table x (funcall hash x) 0 test)))
      (make-instance 'hash-set
                     :test test
                     :hash hash
                     :table (reduce #'%insert xs :initial-value table)))))

(defun set-remove (set &rest xs)
  "Return a new set with the elements xs removed from it. If an item x in
xs is not in the set, it is ignored."
  (with-hamt set (:test test :hash hash :table table)
    (flet ((%remove (table x)
             (%set-remove-node table x (funcall hash x) 0 test)))
      (make-instance 'hash-set
                     :test test
                     :hash hash
                     :table (reduce #'%remove xs :initial-value table)))))

(defun set-reduce (func set initial-value)
  "Successively apply a function to elements of the set. The function is
assumed to have the signature
  `func :: A B -> A`,
where A is the type of `initial-value` and `B` is the type of set elements.
Note that HAMTs do not store items in any order, so the reduction operation
cannot be sensitive to the order in which the items are reduced."
  (%hamt-reduce func (hamt-table set) initial-value))

(defun set-map (func set
                &key
                  (test nil test-supplied-p)
                  (hash nil hash-supplied-p))
  "Return the image of a set under a given function. Optionally use new
comparison and hash functions for the mapped set."
  (let ((mapped-test (if test-supplied-p test (hamt-test set)))
        (mapped-hash (if hash-supplied-p hash (hamt-hash set))))
    (make-instance
     'hash-set
     :test mapped-test
     :hash mapped-hash
     :table (set-reduce (lambda (mapped-table x)
                          (let ((y (funcall func x)))
                            (%set-insert-node mapped-table
                                              y
                                              (funcall mapped-hash y)
                                              0
                                              mapped-test)))
                        set
                        (make-set-table)))))

(defun set-filter (predicate set)
  "Return the elements of the set satisfying a given predicate."
  (with-hamt set (:test test :hash hash :table table)
    (declare (ignore table))
    (make-instance
     'hash-set
     :test test
     :hash hash
     :table (set-reduce (lambda (filtered-table x)
                        (if (funcall predicate x)
                              (%set-insert-node filtered-table
                                                x
                                                (funcall hash x)
                                                0
                                                test)
                              filtered-table))
                        set
                        (make-set-table)))))

(defun set->list (set)
  (set-reduce (lambda (lst x) (cons x lst))
              set
              '()))

(defun %set-merge-into (source target)
  "Insert all elements of SOURCE into TARGET using TARGET's hash/test."
  (with-hamt target (:test target-test :hash target-hash :table target-table)
    (make-instance
     'hash-set
     :test target-test
     :hash target-hash
     :table (set-reduce (lambda (acc x)
                          (%set-insert-node acc
                                            x
                                            (funcall target-hash x)
                                            0
                                            target-test))
                        source
                        target-table))))

(defun %set-remove-all-from (base to-remove)
  "Remove all elements of TO-REMOVE from BASE using BASE's hash/test."
  (with-hamt base (:test base-test :hash base-hash :table base-table)
    (make-instance
     'hash-set
     :test base-test
     :hash base-hash
     :table (set-reduce (lambda (acc x)
                          (%set-remove-node acc
                                            x
                                            (funcall base-hash x)
                                            0
                                            base-test))
                        to-remove
                        base-table))))

(defun %set-intersection-into (left right)
  "Return elements of RIGHT that are present in LEFT.
The returned set uses RIGHT's hash/test semantics."
  (with-hamt left (:test left-test :hash left-hash :table left-table)
    (with-hamt right (:test right-test :hash right-hash :table right-table)
      (declare (ignore right-table))
      (make-instance
       'hash-set
       :test right-test
       :hash right-hash
       :table (set-reduce (lambda (acc x)
                            (if (%set-lookup-node left-table
                                                  x
                                                  (funcall left-hash x)
                                                  0
                                                  left-test)
                                (%set-insert-node acc
                                                  x
                                                  (funcall right-hash x)
                                                  0
                                                  right-test)
                                acc))
                          right
                          (make-set-table))))))

(defun set-union (set &rest args)
  (reduce (lambda (set1 set2)
            (%set-merge-into set1 set2))
          args :initial-value set))

(defun set-intersection (set &rest args)
  (reduce (lambda (set1 set2)
            (%set-intersection-into set1 set2))
          args :initial-value set))

(defun set-diff (set &rest args)
  (reduce (lambda (set1 set2)
            (%set-remove-all-from set1 set2))
          args :initial-value set))

(defun set-symmetric-diff (set1 set2)
  (set-diff (set-union set1 set2)
            (set-intersection set1 set2)))


(defun %hash-set-eq (node1 node2 test)
  (typecase node1
    (set-leaf
     (and (typep node2 'set-leaf)
          (funcall test (node-key node1) (node-key node2))))
    (set-conflict
     (and (typep node2 'set-conflict)
          (equal (conflict-hash node1) (conflict-hash node2))
          (let ((entries1 (conflict-entries node1))
                (entries2 (conflict-entries node2)))
            (and (= (length entries1) (length entries2))
                 (every (lambda (x)
                          (member x entries2 :test test))
                        entries1)))))
    (set-table
     (and (typep node2 'set-table)
          (equal (table-bitmap node1) (table-bitmap node2))
          (array-eq (table-array node1)
                    (table-array node2)
                    (lambda (set1 set2)
                      (%hash-set-eq set1 set2 test)))))
    (t nil)))

(defun set-eq (set1 set2)
  (let ((test1 (hamt-test set1)))
    (if (not (eq test1 (hamt-test set2)))
        (error 'incompatible-tests-error)
        (%hash-set-eq (hamt-table set1) (hamt-table set2) test1))))
