#!/usr/bin/env sbcl --script

(require :asdf)

(let* ((this (or *load-pathname* *compile-file-pathname*))
       (script-dir (make-pathname :name nil :type nil :defaults this))
       (root (truename (merge-pathnames "../" script-dir))))
  (pushnew root asdf:*central-registry* :test #'equal))

(asdf:load-system :cl-hamt)

(defun parse-arg (args idx default)
  (if (< idx (length args))
      (nth idx args)
      default))

(defun parse-numeric-args (argv)
  (let ((nums (remove nil
                      (mapcar (lambda (s)
                                (ignore-errors (parse-integer s)))
                              argv))))
    (values (parse-arg nums 0 50000)
            (parse-arg nums 1 4096))))

(defun hamming-weight32 (x)
  (logcount (ldb (byte 32 0) x)))

(defun generate-integer-samples (n)
  (loop for i below n collect i))

(defun generate-string-samples (n)
  (loop for i below n collect (format nil "item-~D" i)))

(defun hash-stats (name hash-fn samples buckets)
  (let* ((n (length samples))
         (counts (make-array buckets :element-type 'fixnum :initial-element 0))
         (seen (make-hash-table :test #'eql))
         (bit-ones 0))
    (dolist (x samples)
      (let* ((h (funcall hash-fn x))
             (bucket (mod h buckets)))
        (incf (aref counts bucket))
        (incf bit-ones (hamming-weight32 h))
        (setf (gethash h seen) t)))
    (let* ((unique (hash-table-count seen))
           (collisions (- n unique))
           (expected (/ (float n) buckets))
           (chi-square
             (loop for c across counts
                   for d = (- c expected)
                   sum (/ (* d d) expected)))
           (max-bucket (loop for c across counts maximize c))
           (min-bucket (loop for c across counts minimize c))
           (bit-one-ratio (/ (float bit-ones) (* 32.0 n))))
      (format t "~&~A~%" name)
      (format t "  samples: ~D~%" n)
      (format t "  unique hashes: ~D~%" unique)
      (format t "  collisions: ~D (~,2F%)~%" collisions (* 100.0 (/ collisions n)))
      (format t "  buckets: ~D~%" buckets)
      (format t "  bucket min/max: ~D / ~D~%" min-bucket max-bucket)
      (format t "  chi-square: ~,3F~%" chi-square)
      (format t "  bit-1 ratio: ~,4F (ideal ~,4F)~%~%" bit-one-ratio 0.5))))

(defun run-analysis (n buckets)
  (let ((int-samples (generate-integer-samples n))
        (str-samples (generate-string-samples n)))
    (format t "~&=== Integer Samples ===~%")
    (hash-stats "xxhash32-object"
                #'cl-hamt::xxhash32-object
                int-samples
                buckets)
    (hash-stats "siphash32-object"
                #'cl-hamt::siphash32-object
                int-samples
                buckets)
    (format t "~&=== String Samples ===~%")
    (hash-stats "xxhash32-object"
                #'cl-hamt::xxhash32-object
                str-samples
                buckets)
    (hash-stats "siphash32-object"
                #'cl-hamt::siphash32-object
                str-samples
                buckets)))

(multiple-value-bind (n buckets)
    (parse-numeric-args (cdr sb-ext:*posix-argv*))
  (format t "~&Running hash analysis with N=~D, buckets=~D~%~%" n buckets)
  (run-analysis n buckets))
