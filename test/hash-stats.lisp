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
            (parse-arg nums 1 4096)
            (parse-arg nums 2 20))))

(defun hamming-weight64 (x)
  (logcount (ldb (byte 64 0) x)))

(defun generate-integer-samples (n state)
  (loop for i below n
        collect (random (ash 1 31) state)))

(defun generate-string-samples (n state)
  (loop for i below n
        collect (format nil "item-~D" (random (ash 1 30) state))))

(defun hash-stats-single (hash-fn samples buckets)
  (let* ((n (length samples))
         (counts (make-array buckets :element-type 'fixnum :initial-element 0))
         (seen (make-hash-table :test #'eql))
         (bit-ones 0))
    (dolist (x samples)
      (let* ((h (funcall hash-fn x))
             (bucket (mod h buckets)))
        (incf (aref counts bucket))
        (incf bit-ones (hamming-weight64 h))
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
           (bit-one-ratio (/ (float bit-ones) (* 64.0 n))))
      (list :unique unique
            :collisions collisions
            :collision-rate (/ (float collisions) n)
            :chi-square chi-square
            :min-bucket min-bucket
            :max-bucket max-bucket
            :bit-one-ratio bit-one-ratio))))

(defun mean (xs)
  (/ (reduce #'+ xs) (length xs)))

(defun stddev (xs)
  (let* ((n (length xs)))
    (if (<= n 1)
        0.0
        (let* ((m (mean xs))
               (var (/ (reduce #'+ xs
                               :key (lambda (x)
                                      (let ((d (- x m)))
                                        (* d d))))
                       (1- n))))
          (sqrt var)))))

(defun ci95-half-width (xs)
  (if (<= (length xs) 1)
      0.0
      (* 1.96 (/ (stddev xs) (sqrt (length xs))))))

(defun summarize-metric (xs)
  (list :mean (mean xs)
        :stddev (stddev xs)
        :ci95 (ci95-half-width xs)))

(defun summarize-trials (trial-results key)
  (summarize-metric (mapcar (lambda (r) (getf r key)) trial-results)))

(defun print-summary-line (label summary &key (percent nil) (digits 4))
  (let ((m (getf summary :mean))
        (ci (getf summary :ci95)))
    (if percent
        (format t "  ~A: ~,2F% +/- ~,2F% (95% CI)~%" label (* 100.0 m) (* 100.0 ci))
        (format t "  ~A: ~,vf +/- ~,vf (95% CI)~%" label digits m digits ci))))

(defun run-dataset-analysis (dataset-name sample-generator hash-name hash-fn n buckets trials state)
  (let ((trial-results
          (loop repeat trials
                for samples = (funcall sample-generator n state)
                collect (hash-stats-single hash-fn samples buckets))))
    (format t "~&~A / ~A~%" dataset-name hash-name)
    (format t "  samples/trial: ~D, buckets: ~D, trials: ~D~%" n buckets trials)
    (print-summary-line "unique hashes"
                        (summarize-trials trial-results :unique)
                        :digits 2)
    (print-summary-line "collisions"
                        (summarize-trials trial-results :collisions)
                        :digits 2)
    (print-summary-line "collision rate"
                        (summarize-trials trial-results :collision-rate)
                        :percent t)
    (print-summary-line "chi-square"
                        (summarize-trials trial-results :chi-square)
                        :digits 3)
    (print-summary-line "bit-1 ratio"
                        (summarize-trials trial-results :bit-one-ratio)
                        :digits 5)
    (format t "  bucket min/max across trials: ~D / ~D~%~%"
            (reduce #'min trial-results :key (lambda (r) (getf r :min-bucket)))
            (reduce #'max trial-results :key (lambda (r) (getf r :max-bucket))))))

(defun run-analysis (n buckets trials)
  (let ((state (make-random-state t)))
    (run-dataset-analysis "Integer samples"
                          #'generate-integer-samples
                          "xxhash64-object"
                          #'cl-hamt::xxhash64-object
                          n buckets trials state)
    (run-dataset-analysis "Integer samples"
                          #'generate-integer-samples
                          "siphash64-object"
                          #'cl-hamt::siphash64-object
                          n buckets trials state)
    (run-dataset-analysis "String samples"
                          #'generate-string-samples
                          "xxhash64-object"
                          #'cl-hamt::xxhash64-object
                          n buckets trials state)
    (run-dataset-analysis "String samples"
                          #'generate-string-samples
                          "siphash64-object"
                          #'cl-hamt::siphash64-object
                          n buckets trials state)))

(multiple-value-bind (n buckets trials)
    (parse-numeric-args (cdr sb-ext:*posix-argv*))
  (format t "~&Running hash analysis with N=~D, buckets=~D, trials=~D~%~%"
          n buckets trials)
  (run-analysis n buckets trials))
