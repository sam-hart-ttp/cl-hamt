(in-package #:cl-hamt)

;; Lookup/insert/remove use explicit node walking to avoid recursive generic
;; dispatch in hot paths.
(defun %dict-lookup-node (node key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (dict-leaf
     (if (funcall test (node-key node) key)
         (values (node-value node) t)
         (values nil nil)))
    (dict-conflict
     (let ((key-val (assoc key (conflict-entries node) :test test)))
       (if key-val
           (values (cdr key-val) t)
           (values nil nil))))
    (dict-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (if hit
           (%dict-lookup-node (aref array index) key hash (1+ depth) test)
           (values nil nil))))
    (t (values nil nil))))

;; Inserting into a leaf either functionally updates the value stored in the
;; current node if the keys match, or creates a conflict node if the keys do
;; not match but their hashes do.
(defun %dict-insert-node (node key value hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (dict-leaf
     (let ((nkey (node-key node)))
       (if (funcall test key nkey)
           (make-dict-leaf
            :key key
            :value value)
           (make-dict-conflict
            :hash hash
            :entries (acons key
                            value
                            (acons nkey
                                   (node-value node)
                                   '()))))))
    (dict-conflict
     (let ((entries (conflict-entries node))
           (updated '())
           (found nil)
           (changed nil))
       (dolist (kv entries)
         (if (funcall test (car kv) key)
             (progn
               (setf found t)
               (if (eq (cdr kv) value)
                   (push kv updated)
                   (progn
                     (setf changed t)
                     (push (cons key value) updated))))
             (push kv updated)))
       (cond
         ((and found (not changed)) node)
         (found
          (make-dict-conflict
           :hash hash
           :entries (nreverse updated)))
         (t (make-dict-conflict
             :hash hash
             :entries (cons (cons key value) entries))))))
    (dict-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (if hit
           (let* ((old-node (aref array index))
                  (new-node (%dict-insert-node old-node key value hash (1+ depth) test)))
             (if (eq new-node old-node)
                 node
                 (make-dict-table
                  :bitmap bitmap
                  :table (vec-update array index new-node))))
           (let ((new-node (if (= depth 6)
                               (make-dict-leaf
                                :key key
                                :value value)
                               (%dict-insert-node (make-dict-table)
                                                  key
                                                  value
                                                  hash
                                                  (1+ depth)
                                                  test))))
             (make-dict-table
              :bitmap (logior bitmap (ash 1 bits))
              :table (vec-insert array index new-node))))))
    (t node)))

(defun %dict-remove-node (node key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (typecase node
    (dict-leaf
     (unless (funcall test key (node-key node))
       node))
    ;; Removing an entry from a conflict node reduces the scope of the hash
    ;; collision. If there is now only 1 key with the given hash, we can
    ;; return a dict-leaf, since there is no longer a collision.
    (dict-conflict
     (let ((kept '())
           (kept-count 0)
           (removed nil))
       (dolist (entry (conflict-entries node))
         (if (funcall test (car entry) key)
             (setf removed t)
             (progn
               (incf kept-count)
               (push entry kept))))
       (cond
         ((not removed) node)
         ((= kept-count 1)
          (make-dict-leaf
           :key (caar kept)
           :value (cdar kept)))
         (t (make-dict-conflict
             :hash hash
             :entries (nreverse kept))))))
    (dict-table
     (with-table node hash depth
         (bitmap array bits index hit)
       (declare (type (unsigned-byte 32) bitmap)
                (type simple-vector array)
                (type fixnum bits index))
       (if (not hit)
           node
           (let* ((old-node (aref array index))
                  (new-node (%dict-remove-node old-node key hash (1+ depth) test)))
             (cond
               ((eq new-node old-node) node)
               (new-node
                (make-dict-table
                 :bitmap bitmap
                 :table (vec-update array index new-node)))
               ((= bitmap 1) nil)
               (t (make-dict-table
                   :bitmap (logxor bitmap (ash 1 bits))
                   :table (vec-remove array index))))))))
    (t node)))



;; Wrapper dictionary class
(defclass hash-dict (hamt)
  ((table
    :reader hamt-table
    :initarg :table
    :initform (make-dict-table))))

(defun empty-dict (&key (test #'equal) (hash #'cl-murmurhash:murmurhash))
  "Return an empty hash-dict, in which keys will be compared and hashed
with the supplied test and hash functions. The hash must be a 32-bit hash."
  (make-instance 'hash-dict
                 :test (ctypecase test
                         (function test)
                         (symbol (symbol-function test)))
                 :hash hash))

(defun dict-lookup (dict key)
  "Multiply-return the value mapped to by the key in the dictionary and
whether or not the value is present in the dictionary.
The multiple return is necessary in case a key is present but maps to nil."
  (with-hamt dict (:test test :hash hash :table table)
    (%dict-lookup-node table key (funcall hash key) 0 test)))

(defun dict-size (dict)
  "Return the number of key/value pairs in the dict"
  (%hamt-size (hamt-table dict)))

(defun dict-insert (dict &rest args)
  "Return a new dictionary with the key/value pairs added. The key/value
pairs are assumed to be alternating in the &rest argument, so to add the
key/value pairs (k1, v1), ..., (kn, vn), one would invoke
  (dict-insert dict k1 v1 ... kn vn).
If any of the keys are already present in the dict passed, they are mapped
to the new values in the returned dict."
  (with-hamt dict (:test test :hash hash :table table)
    (flet ((%insert (table key value)
             (%dict-insert-node table key value (funcall hash key) 0 test)))
      (make-instance
       'hash-dict
       :test test
       :hash hash
       :table (labels ((f (table args)
                         (if args
                             (let ((key (car args))
                                   (value (cadr args)))
                               (f (%insert table key value)
                                  (cddr args)))
                             table)))
                (f table args))))))

(defun dict-remove (dict &rest keys)
  "Return a new dict with the keys removed. Any keys passed that are not
already present in the dict are ignored."
  (with-hamt dict (:test test :hash hash :table table)
    (flet ((%remove (table key)
             (%dict-remove-node table key (funcall hash key) 0 test)))
      (make-instance 'hash-dict
                     :test test
                     :hash hash
                     :table (reduce #'%remove keys :initial-value table)))))

(defun dict-reduce (func dict initial-value)
  "Successively apply a function to key/value pairs of the dict.
The function is assumed to have the signature
   `func :: A K V -> A`,
where `A` is the type of the initial-value, `K` is the type of the dict
keys and `V` is the type of dictionary values.
Note that HAMTs do not store items in any order, so the reduction operation
cannot be sensitive to the order in which the items are reduced."
  (%hamt-reduce func (hamt-table dict) initial-value))

(defun dict-map-values (func dict &key test hash)
  "Return a new dict with the values mapped by the given function.
Optionally use new comparison and hash functions for the mapped dict."
  (let ((mapped-test (if test test (hamt-test dict)))
        (mapped-hash (if hash hash (hamt-hash dict))))
    (make-instance
     'hash-dict
     :test mapped-test
     :hash mapped-hash
     :table (dict-reduce (lambda (mapped-table k v)
                           (%dict-insert-node mapped-table
                                              k
                                              (funcall func v)
                                              (funcall mapped-hash k)
                                              0
                                              mapped-test))
                         dict
                         (make-dict-table)))))

(defun dict-map-keys (func dict &key test hash)
  "Return a new dict with the keys mapped by the given function."
  (let ((mapped-test (if test test (hamt-test dict)))
        (mapped-hash (if hash hash (hamt-hash dict))))
    (make-instance
     'hash-dict
     :test mapped-test
     :hash mapped-hash
     :table (dict-reduce (lambda (mapped-table k v)
                           (let ((key (funcall func k)))
                             (%dict-insert-node mapped-table
                                                key
                                                v
                                                (funcall mapped-hash key)
                                                0
                                                mapped-test)))
                         dict
                         (make-dict-table)))))

(defun dict-filter (predicate dict)
  "Return a new dict consisting of the key/value pairs satisfying the
given predicate."
  (with-hamt dict (:test test :hash hash :table table)
    (declare (ignore table))
    (make-instance
     'hash-dict
     :test test
     :hash hash
     :table (dict-reduce (lambda (filtered-table k v)
                           (if (funcall predicate k v)
                               (%dict-insert-node filtered-table
                                                  k
                                                  v
                                                  (funcall hash k)
                                                  0
                                                  test)
                               filtered-table))
                         dict
                         (make-dict-table)))))

(defun dict-reduce-keys (func dict initial-value)
  "Reducing over dictionary keys, ignoring the values."
  (flet ((f (r k v)
           (declare (ignore v))
           (funcall func r k)))
    (dict-reduce #'f dict initial-value)))

(defun dict-reduce-values (func dict initial-value)
  "Reducing over dictionary values, ignoring the keys."
  (flet ((f (r k v)
           (declare (ignore k))
           (funcall func r v)))
    (dict-reduce #'f dict initial-value)))

(defun dict->alist (dict)
  (dict-reduce (lambda (alist k v)
                 (acons k v alist))
               dict
               '()))

(defun %hash-dict-eq (node1 node2 key-test value-test)
  (typecase node1
    (dict-leaf
     (and (typep node2 'dict-leaf)
          (funcall key-test (node-key node1) (node-key node2))
          (funcall value-test (node-value node1) (node-value node2))))
    (dict-conflict
     (and (typep node2 'dict-conflict)
          (equal (conflict-hash node1) (conflict-hash node2))
          (labels ((alist-eq (alist1 alist2)
                     (if (or (not alist1) (not alist2))
                         (and (not alist1) (not alist2))
                         (let ((key1 (caar alist1))
                               (key2 (caar alist2))
                               (value1 (cdar alist1))
                               (value2 (cdar alist2)))
                           (when (and (funcall key-test key1 key2)
                                      (funcall value-test value1 value2))
                             (alist-eq (cdr alist1) (cdr alist2)))))))
            (alist-eq (conflict-entries node1) (conflict-entries node2)))))
    (dict-table
     (and (typep node2 'dict-table)
          (equal (table-bitmap node1) (table-bitmap node2))
          (array-eq (table-array node1)
                    (table-array node2)
                    (lambda (dict1 dict2)
                      (%hash-dict-eq dict1 dict2 key-test value-test)))))
    (t nil)))

(defun dict-eq (dict1 dict2 &key (value-test #'equal))
  (let ((test1 (hamt-test dict1)))
    (if (not (eq test1 (hamt-test dict2)))
        (error 'incompatible-tests-error)
        (%hash-dict-eq (hamt-table dict1) (hamt-table dict2) test1 value-test))))
