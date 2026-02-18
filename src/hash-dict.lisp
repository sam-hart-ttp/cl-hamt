(in-package #:cl-hamt)

;; Leaves in a dictionary also store the value contained at the node, as well
;; as a key
(defclass dict-leaf (leaf)
  ((value
    :reader node-value
    :initarg :value
    :initform nil)))

;; These classes give extra information to dispatch on, e.g. for looking up
;; entries in conflict nodes
(defclass dict-conflict (conflict) ())
(defclass dict-table (table) ())



;; Methods for looking up key/value pairs in a dict
(defmethod %hamt-lookup ((node dict-leaf) key hash depth test)
  (if (funcall test (node-key node) key)
      (values (node-value node) t)
      (values nil nil)))

(defmethod %hamt-lookup ((node dict-table) key hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (with-table node hash depth
      (bitmap array bits index hit)
    (declare (type (unsigned-byte 32) bitmap)
             (type simple-vector array)
             (type fixnum bits index))
    (if hit
        (%hamt-lookup (aref array index) key hash (1+ depth) test)
        (values nil nil))))

(defmethod %hamt-lookup ((node dict-conflict) key hash depth test)
  (declare (ignore hash depth))
  (let ((key-val (assoc key (conflict-entries node) :test test)))
    (if key-val
        (values (cdr key-val) t)
        (values nil nil))))



;; Methods for inserting key/value pairs into a dict
(defgeneric %dict-insert (node key value hash depth test))

;; Inserting into a leaf either functionally updates the value stored in the
;; current node if the keys match, or creates a conflict node if the keys do
;; not match but their hashes do.
(defmethod %dict-insert ((node dict-leaf) key value hash depth test)
  (declare (ignore depth))
  (let ((nkey (node-key node)))
    (if (funcall test key nkey)
        (make-instance 'dict-leaf
                       :key key
                       :value value)
        (make-instance 'dict-conflict
                       :hash hash
                       :entries (acons key
                                       value
                                       (acons nkey
                                              (node-value node)
                                              '()))))))

;; Inserting into a conflict node either updates the value associated to an
;; existing key, or expands the scope of the conflict
(defmethod %dict-insert ((node dict-conflict) key value hash depth test)
  (declare (ignore depth))
  (let ((entries (conflict-entries node)))
    (let ((updated '())
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
         (make-instance 'dict-conflict
                        :hash hash
                        :entries (nreverse updated)))
        (t (make-instance 'dict-conflict
                          :hash hash
                          :entries (cons (cons key value) entries)))))))

(defmethod %dict-insert ((node dict-table) key value hash depth test)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) hash)
           (type fixnum depth))
  (with-table node hash depth
      (bitmap array bits index hit)
    (declare (type (unsigned-byte 32) bitmap)
             (type simple-vector array)
             (type fixnum bits index))
    (flet ((%insert (table)
             (%dict-insert table key value hash (1+ depth) test)))
      (if hit
          (let* ((old-node (aref array index))
                 (new-node (%insert old-node)))
            (if (eq new-node old-node)
                node
                (make-instance 'dict-table
                               :bitmap bitmap
                               :table (vec-update array index new-node))))
          (let ((new-node
                  (if (= depth 6)
                      (make-instance 'dict-leaf
                                     :key key
                                     :value value)
                      (%insert (make-instance 'dict-table)))))
            (make-instance 'dict-table
                           :bitmap (logior bitmap (ash 1 bits))
                           :table (vec-insert array index new-node)))))))



;; Removing entries from dictionaries.
;; Most of the functionality is contained in the file hamt.lisp.

;; Removing an entry from a conflict node reduces the scope of the hash
;; collision. If there is now only 1 key with the given hash, we can
;; return a dict-leaf, since there is no longer a collision.
(defmethod %hamt-remove ((node dict-conflict) key hash depth test)
  (declare (ignore depth))
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
       (make-instance 'dict-leaf
                      :key (caar kept)
                      :value (cdar kept)))
      (t (make-instance 'dict-conflict
                        :hash hash
                        :entries (nreverse kept))))))



;; Methods for reducing over elements of HAMTs
(defmethod %hamt-reduce (func (node dict-leaf) initial-value)
  (funcall func initial-value (node-key node) (node-value node)))

(defmethod %hamt-reduce (func (node dict-conflict) initial-value)
  (labels ((f (alist r)
             (if alist
                 (f (cdr alist)
                    (funcall func r (caar alist) (cdar alist)))
                 r)))
    (f (conflict-entries node) initial-value)))



;; Wrapper dictionary class
(defclass hash-dict (hamt)
  ((table
    :reader hamt-table
    :initarg :table
    :initform (make-instance 'dict-table
                             :bitmap 0
                             :table (make-array 0)))))

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
    (%hamt-lookup table key (funcall hash key) 0 test)))

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
             (%dict-insert table key value (funcall hash key) 0 test)))
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
             (%hamt-remove table key (funcall hash key) 0 test)))
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
                           (%dict-insert mapped-table
                                         k
                                         (funcall func v)
                                         (funcall mapped-hash k)
                                         0
                                         mapped-test))
                         dict
                         (make-instance 'dict-table
                                        :bitmap 0
                                        :table (make-array 0))))))

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
                             (%dict-insert mapped-table
                                           key
                                           v
                                           (funcall mapped-hash key)
                                           0
                                           mapped-test)))
                         dict
                         (make-instance 'dict-table
                                        :bitmap 0
                                        :table (make-array 0))))))

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
                               (%dict-insert filtered-table
                                             k
                                             v
                                             (funcall hash k)
                                             0
                                             test)
                               filtered-table))
                         dict
                         (make-instance 'dict-table
                                        :bitmap 0
                                        :table (make-array 0))))))

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

;; Methods for deciding if two dictionaries are equal
(defgeneric %hash-dict-eq (dict1 dict2 key-test value-test))

(defmethod %hash-dict-eq (dict1 dict2 key-test value-test)
  (declare (ignore dict1 dict2 key-test value-test))
  nil)

(defmethod %hash-dict-eq ((node1 dict-leaf)
                          (node2 dict-leaf)
                          key-test
                          value-test)
  (and (funcall key-test (node-key node1) (node-key node2))
       (funcall value-test (node-value node1) (node-value node2))))


(defmethod %hash-dict-eq ((node1 dict-conflict)
                          (node2 dict-conflict)
                          key-test
                          value-test)
  (and (equal (conflict-hash node1) (conflict-hash node2))
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

(defmethod %hash-dict-eq ((node1 dict-table)
                          (node2 dict-table)
                          key-test
                          value-test)
  (and (equal (table-bitmap node1) (table-bitmap node2))
       (array-eq (table-array node1)
                 (table-array node2)
                 (lambda (dict1 dict2)
                   (%hash-dict-eq dict1 dict2 key-test value-test)))))

(defun dict-eq (dict1 dict2 &key (value-test #'equal))
  (let ((test1 (hamt-test dict1)))
    (if (not (eq test1 (hamt-test dict2)))
        (error 'incompatible-tests-error)
        (%hash-dict-eq (hamt-table dict1) (hamt-table dict2) test1 value-test))))
