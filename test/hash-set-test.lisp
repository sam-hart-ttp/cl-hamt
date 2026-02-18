(in-package #:cl-hamt-test)

(def-suite hash-set-tests)
(in-suite hash-set-tests)

(test empty
      (is (= 0 (set-size (empty-set)))))

(test custom-hash-validation
  (signals error
    (set-insert (empty-set :hash (lambda (x)
                                   (declare (ignore x))
                                   -1))
                "bad-hash"))
  (signals error
    (set-insert (empty-set :hash (lambda (x)
                                   (declare (ignore x))
                                   (ash 1 70)))
                "bad-hash")))

(test hash-mode-validation
  (signals error
    (empty-set :hash-mode :unknown))
  (is (typep (empty-set :test 'equal :hash-mode :fast) 'hash-set))
  (signals error
    (empty-set :test #'equalp :hash-mode :fast))
  (is (typep (empty-set :test #'equalp
                        :hash (lambda (x)
                                (ldb (byte 64 0) (sxhash (if (stringp x)
                                                             (string-downcase x)
                                                             x)))))
             'hash-set)))

(test hash-output-range-and-spread
  (let* ((samples (loop for i below 128 collect (format nil "item-~D" i)))
         (fast-hashes (mapcar #'cl-hamt::xxhash64-object samples))
         (keyed-hashes (mapcar #'cl-hamt::siphash64-sxhash-object samples))
         (secure-hashes (mapcar #'cl-hamt::siphash64-object samples)))
    (is-true (every (lambda (h) (typep h '(unsigned-byte 64))) fast-hashes))
    (is-true (every (lambda (h) (typep h '(unsigned-byte 64))) keyed-hashes))
    (is-true (every (lambda (h) (typep h '(unsigned-byte 64))) secure-hashes))
    ;; Not a strict statistical test, just a guard against degenerate hashing.
    (is-true (> (length (remove-duplicates fast-hashes)) 96))
    (is-true (> (length (remove-duplicates keyed-hashes)) 96))
    (is-true (> (length (remove-duplicates secure-hashes)) 96))))

(defvar *evil-print-called* nil)

(defclass evil-print-object () ())

(defmethod print-object ((obj evil-print-object) stream)
  (declare (ignore obj))
  (setf *evil-print-called* t)
  (write-string "#<evil-print-object>" stream))

(test secure-mode-does-not-dispatch-print-object
  (setf *evil-print-called* nil)
  (signals error
    (cl-hamt::siphash64-object (make-instance 'evil-print-object)))
  (is-false *evil-print-called*))

(test hash-mode-smoke
  (let* ((items (loop for i below 64 collect (format nil "name-~D" i)))
         (fast (apply #'set-insert (cons (empty-set :hash-mode :fast) items)))
         (keyed (apply #'set-insert (cons (empty-set :hash-mode :keyed) items)))
         (secure (apply #'set-insert (cons (empty-set :hash-mode :secure) items))))
    (is (= (length items) (set-size fast)))
    (is (= (length items) (set-size keyed)))
    (is (= (length items) (set-size secure)))
    (is-true (every (lambda (x)
                      (and (set-lookup fast x)
                           (set-lookup keyed x)
                           (set-lookup secure x)))
                    items))))

(defvar swinging-hepcats
  (set-insert (empty-set)
              "Louis Armstrong"
              "Earl Hines"
              "Artie Shaw"
              "Count Basie"
              "Duke Ellington"
              "Coleman Hawkins"))

(defvar beboppers
  (set-insert (empty-set)
              "Coleman Hawkins"
              "Charlie Parker"
              "Dizzy Gillespie"
              "Bud Powell"
              "Miles Davis"))

(test inserting
  (is (= 6 (set-size swinging-hepcats)))
  (is-true (set-lookup swinging-hepcats "Earl Hines"))
  (is-false (set-lookup swinging-hepcats "Kenny G")))

(test removing
  (is-false (set-lookup (set-remove swinging-hepcats
                                    "Coleman Hawkins")
                        "Coleman Hawkins"))
  (is (= 5 (set-size (set-remove swinging-hepcats "Coleman Hawkins")))))


(defun integer-set (n)
  (labels ((f (s k)
             (if (= k n)
                 s
                 (f (set-insert s k) (1+ k)))))
    (f (empty-set) 0)))

(test reducing
  (is (= 45 (set-reduce #'+ (integer-set 10) 0))))

(defvar hepcats-and-beboppers
  (set-filter (lambda (person)
                (set-lookup beboppers person))
              swinging-hepcats))

(test filtering
  (is-true (set-lookup hepcats-and-beboppers "Coleman Hawkins"))
  (is (= 1 (set-size hepcats-and-beboppers))))

(test mapping
  (is-true (set-lookup (set-map (lambda (k) (* k k))
                                (integer-set 10))
                       81)))


;; We force collisions with a constant hash function to test conflict handling.
(defun collision-hash (x)
  (declare (ignore x))
  0)

(defvar some-word-collisions
  '(("PSYCHOANALYZE" . "BEDUCKS")
    ("PANSPERMIES" . "NONSELF")
    ("UNSIGHING" . "TURBITS")))

(defvar set-with-collisions
  (reduce (lambda (s p)
            (set-insert s (car p) (cdr p)))
          some-word-collisions
          :initial-value (empty-set :test #'equal
                                    :hash #'collision-hash)))

(test collisions
  (is (equal 6 (set-size set-with-collisions)))
  (is-true (reduce (lambda (correct word)
                         (and correct
                              (set-lookup set-with-collisions
                                          word)))
                       '("PSYCHOANALYZE"
                         "BEDUCKS"
                         "PANSPERMIES"
                         "NONSELF"
                         "UNSIGHING"
                         "TURBITS")
                       :initial-value t))
  (is-true (set-lookup (set-remove set-with-collisions "PSYCHOANALYZE")
                       "BEDUCKS"))
  (is (= 5 (set-size (set-remove set-with-collisions "BEDUCKS")))))

(defvar max-number-value 1000)
(defvar some-numbers
  (loop for i from 0 to 100 collecting (random max-number-value)))

(defvar set-without-collisions
  (reduce (lambda (s p)
            (set-insert s (car p)))
          some-word-collisions
          :initial-value (empty-set :test #'equal
                                    :hash #'collision-hash)))

(test set-equality
  (is-false (set-eq swinging-hepcats beboppers))
  (is-true (set-eq (set-union swinging-hepcats beboppers)
                   (set-union beboppers swinging-hepcats)))
  (is-true (set-eq set-with-collisions set-with-collisions))
  (is-true (let ((set1 (apply #'set-insert (cons (empty-set) some-numbers)))
                 (set2 (apply #'set-insert (cons (empty-set) some-numbers))))
             (and (not (eq set1 set2))
                  (set-eq set1 set2))))
  (is-false (set-eq (apply 'set-insert (cons (empty-set) some-numbers))
                    (apply 'set-insert
                           (cons (empty-set)
                                 (cons (+ 1 max-number-value) some-numbers)))))
  (is-false (set-eq set-with-collisions set-without-collisions))
  (is-true (set-eq (empty-set :test 'equal) (empty-set :test #'equal))))
