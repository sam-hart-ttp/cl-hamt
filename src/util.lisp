(in-package #:cl-hamt)

;; Utility functions for operating on HAMTs.
;; Compile-time slicing config:
;; - set +slice-bits+ to 5 for 32-way nodes
;; - set +slice-bits+ to 6 for 64-way nodes
(defconstant +slice-bits+ 5)
(defconstant +hash-bits+ 64)
(defconstant +bitmap-bits+ (ash 1 +slice-bits+))
;; Note: depth +max-hash-depth+ uses only (- +hash-bits+ (* +slice-bits+ +max-hash-depth+))
;; effective bits (e.g. 4 bits → 16-way) since (1+ +max-hash-depth+) × +slice-bits+ > +hash-bits+.
(defconstant +max-hash-depth+ (floor (1- +hash-bits+) +slice-bits+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member +slice-bits+ '(5 6))
    (error "Invalid +SLICE-BITS+ value ~S. Expected 5 or 6." +slice-bits+))
  (unless (<= +bitmap-bits+ 64)
    (error "Invalid +SLICE-BITS+ value ~S; bitmap width would exceed 64 bits."
           +slice-bits+)))

(declaim (inline get-bits get-index vec-insert vec-remove vec-update))

(declaim (inline %u32 %u64 %rotl64))

(defun %u32 (x)
  (ldb (byte 32 0) x))

(defun %u64 (x)
  (ldb (byte 64 0) x))

(defun %random-u64 (state)
  (%u64 (random (ash 1 64) state)))

(defparameter *siphash-random-state* (make-random-state t))
(defparameter *siphash-k0* (%random-u64 *siphash-random-state*))
(defparameter *siphash-k1* (%random-u64 *siphash-random-state*))
(declaim (type (unsigned-byte 64) *siphash-k0* *siphash-k1*))

(defun %rotl64 (x r)
  (declare (type (unsigned-byte 64) x)
           (type fixnum r))
  (%u64 (logior (ash x r)
                (ash x (- r 64)))))

(defun %string->utf8-octets (string)
  "Encode STRING as UTF-8 octets."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string string))
  (let ((octets (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (declare (type (vector (unsigned-byte 8)) octets))
    (labels ((emit (byte)
               (vector-push-extend (%u32 byte) octets)))
      (loop for ch across string
            for cp = (char-code ch) do
              (cond
                ((<= cp #x7F)
                 (emit cp))
                ((<= cp #x7FF)
                 (emit (logior #xC0 (ash cp -6)))
                 (emit (logior #x80 (logand cp #x3F))))
                ((<= cp #xFFFF)
                 (emit (logior #xE0 (ash cp -12)))
                 (emit (logior #x80 (logand (ash cp -6) #x3F)))
                 (emit (logior #x80 (logand cp #x3F))))
                ((<= cp #x10FFFF)
                 (emit (logior #xF0 (ash cp -18)))
                 (emit (logior #x80 (logand (ash cp -12) #x3F)))
                 (emit (logior #x80 (logand (ash cp -6) #x3F)))
                 (emit (logior #x80 (logand cp #x3F))))
                (t
                 (emit #x3F)))))
    octets))

(defun %octet-buffer ()
  (make-array 32
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun %emit-u8 (buffer byte)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type (unsigned-byte 8) byte))
  (vector-push-extend byte buffer)
  buffer)

(defun %emit-varuint (buffer n)
  "Encode N as unsigned LEB128."
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type (integer 0 *) n))
  (loop
    (let ((byte (logand n #x7f)))
      (setf n (ash n -7))
      (if (zerop n)
          (progn
            (%emit-u8 buffer byte)
            (return buffer))
          (%emit-u8 buffer (logior byte #x80))))))

(defun %emit-string (buffer string)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type string string))
  (let* ((octets (%string->utf8-octets string))
         (len (length octets)))
    (declare (type fixnum len)
             (type (vector (unsigned-byte 8)) octets))
    (%emit-varuint buffer len)
    (loop for i fixnum from 0 below len do
      (%emit-u8 buffer (aref octets i)))
    buffer))

(defun %emit-integer (buffer n)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type integer n))
  ;; Encode sign and magnitude bytes (little endian).
  (let ((m (if (minusp n) (- n) n))
        (bytes (%octet-buffer)))
    (declare (type (integer 0 *) m)
             (type (vector (unsigned-byte 8)) bytes))
    (%emit-u8 buffer (if (minusp n) 1 0))
    (loop while (> m 0) do
      (%emit-u8 bytes (logand m #xff))
      (setf m (ash m -8)))
    (when (= (length bytes) 0)
      (%emit-u8 bytes 0))
    (%emit-varuint buffer (length bytes))
    (loop for i fixnum from 0 below (length bytes) do
      (%emit-u8 buffer (aref bytes i)))
    buffer))

(defun %emit-symbol (buffer sym)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type symbol sym))
  (cond
    ((keywordp sym)
     (%emit-u8 buffer 1)
     (%emit-string buffer (symbol-name sym)))
    ((symbol-package sym)
     (%emit-u8 buffer 2)
     (%emit-string buffer (package-name (symbol-package sym)))
     (%emit-string buffer (symbol-name sym)))
    (t
     ;; Uninterned symbols are hashed by their print-name only.
     (%emit-u8 buffer 3)
     (%emit-string buffer (symbol-name sym)))))

(defun %emit-float (buffer fl)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type float fl))
  ;; Use write-to-string for float primitives only (no generic print-object dispatch).
  (%emit-string buffer
                (with-standard-io-syntax
                  (let ((*print-readably* t)
                        (*print-escape* t))
                    (write-to-string fl)))))

(defun %object->octets (obj)
  "Serialize OBJ into tagged canonical octets without invoking print-object."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((buffer (%octet-buffer))
        (seen nil))
    (declare (type (vector (unsigned-byte 8)) buffer))
    (labels ((track (x)
               (unless seen
                 (setf seen (make-hash-table :test #'eq)))
               (when (gethash x seen)
                 (error "SECURE hash does not support circular structures (type ~S)."
                        (type-of x)))
               (setf (gethash x seen) t))
             (untrack (x)
               (remhash x seen))
             (visit (x)
               (typecase x
                 (null
                  (%emit-u8 buffer 0))
                 (cons
                  (track x)
                  (%emit-u8 buffer 1)
                  (visit (car x))
                  (visit (cdr x))
                  (untrack x))
                 (integer
                  (%emit-u8 buffer 2)
                  (%emit-integer buffer x))
                 (ratio
                  (%emit-u8 buffer 3)
                  (visit (numerator x))
                  (visit (denominator x)))
                 (single-float
                  (%emit-u8 buffer 4)
                  (%emit-float buffer x))
                 (double-float
                  (%emit-u8 buffer 5)
                  (%emit-float buffer x))
                 (short-float
                  (%emit-u8 buffer 6)
                  (%emit-float buffer x))
                 (long-float
                  (%emit-u8 buffer 7)
                  (%emit-float buffer x))
                 (complex
                  (%emit-u8 buffer 8)
                  (visit (realpart x))
                  (visit (imagpart x)))
                 (character
                  (%emit-u8 buffer 9)
                  (%emit-varuint buffer (char-code x)))
                 (string
                  (%emit-u8 buffer 10)
                  (%emit-string buffer x))
                 (symbol
                  (%emit-u8 buffer 11)
                  (%emit-symbol buffer x))
                 (bit-vector
                  (track x)
                  (%emit-u8 buffer 12)
                  (%emit-varuint buffer (length x))
                  (loop for i fixnum from 0 below (length x) do
                    (%emit-u8 buffer (if (zerop (aref x i)) 0 1)))
                  (untrack x))
                 (vector
                  (track x)
                  (%emit-u8 buffer 13)
                  (%emit-varuint buffer (length x))
                  (loop for i fixnum from 0 below (length x) do
                    (visit (aref x i)))
                  (untrack x))
                 (array
                  (track x)
                  (%emit-u8 buffer 14)
                  (%emit-varuint buffer (array-rank x))
                  (loop for d fixnum below (array-rank x) do
                    (%emit-varuint buffer (array-dimension x d)))
                  (loop for i fixnum below (array-total-size x) do
                    (visit (row-major-aref x i)))
                  (untrack x))
                 (t
                  (error "SECURE hash does not support objects of type ~S."
                         (type-of x))))))
      (visit obj))
    buffer))

(defmacro %sipround (v0 v1 v2 v3)
  "One round of SipHash mixing over lexical variables V0..V3."
  `(progn
     (setf ,v0 (%u64 (+ ,v0 ,v1)))
     (setf ,v1 (%rotl64 ,v1 13))
     (setf ,v1 (logxor ,v1 ,v0))
     (setf ,v0 (%rotl64 ,v0 32))
     (setf ,v2 (%u64 (+ ,v2 ,v3)))
     (setf ,v3 (%rotl64 ,v3 16))
     (setf ,v3 (logxor ,v3 ,v2))
     (setf ,v0 (%u64 (+ ,v0 ,v3)))
     (setf ,v3 (%rotl64 ,v3 21))
     (setf ,v3 (logxor ,v3 ,v0))
     (setf ,v2 (%u64 (+ ,v2 ,v1)))
     (setf ,v1 (%rotl64 ,v1 17))
     (setf ,v1 (logxor ,v1 ,v2))
     (setf ,v2 (%rotl64 ,v2 32))))

(defmacro %sipcompress (v0 v1 v2 v3 m)
  "SipHash-2-4 compression: XOR message block, two rounds, XOR back."
  `(progn
     (setf ,v3 (logxor ,v3 ,m))
     (%sipround ,v0 ,v1 ,v2 ,v3)
     (%sipround ,v0 ,v1 ,v2 ,v3)
     (setf ,v0 (logxor ,v0 ,m))))

(defun %siphash64-octets (octets)
  "SipHash-2-4 over OCTETS using process-local random keys."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (vector (unsigned-byte 8)) octets))
  (let* ((k0 *siphash-k0*)
         (k1 *siphash-k1*)
         (v0 (logxor #x736f6d6570736575 k0))
         (v1 (logxor #x646f72616e646f6d k1))
         (v2 (logxor #x6c7967656e657261 k0))
         (v3 (logxor #x7465646279746573 k1))
         (len (length octets))
         (full-end (* 8 (floor len 8))))
    (declare (type (unsigned-byte 64) k0 k1 v0 v1 v2 v3)
             (type fixnum len full-end))
    (loop for i fixnum from 0 below full-end by 8 do
      (let ((m 0))
        (declare (type (unsigned-byte 64) m))
        (setf m (%u64
                 (logior (ash (aref octets (+ i 0)) 0)
                         (ash (aref octets (+ i 1)) 8)
                         (ash (aref octets (+ i 2)) 16)
                         (ash (aref octets (+ i 3)) 24)
                         (ash (aref octets (+ i 4)) 32)
                         (ash (aref octets (+ i 5)) 40)
                         (ash (aref octets (+ i 6)) 48)
                         (ash (aref octets (+ i 7)) 56))))
        (%sipcompress v0 v1 v2 v3 m)))
    (let ((b (%u64 (ash len 56))))
      (declare (type (unsigned-byte 64) b))
      (loop for i fixnum from full-end below len do
        (setf b (%u64
                 (logior b
                         (ash (aref octets i)
                              (* 8 (- i full-end)))))))
      (%sipcompress v0 v1 v2 v3 b))
    (setf v2 (logxor v2 #xff))
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%u64 (logxor v0 v1 v2 v3))))

(defun xxhash64-object (obj &optional (seed 0))
  "xxHash64 over the 64-bit sxhash representation of OBJ."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 64) seed))
  (let* ((prime1 #x9E3779B185EBCA87)
         (prime2 #xC2B2AE3D27D4EB4F)
         (prime3 #x165667B19E3779F9)
         (prime5 #x27D4EB2F165667C5)
         (x (%u64 (sxhash obj)))
         (h (%u64 (+ seed prime5 8))))
    (setf h (%u64 (+ h (%u64 (* x prime2)))))
    (setf h (%u64 (* (%rotl64 h 31) prime1)))
    (setf h (logxor h (ash h -33)))
    (setf h (%u64 (* h prime2)))
    (setf h (logxor h (ash h -29)))
    (setf h (%u64 (* h prime3)))
    (setf h (logxor h (ash h -32)))
    h))

(defun siphash64-sxhash-object (obj)
  "SipHash-2-4 over the 64-bit sxhash representation of OBJ.
This is a keyed mixer over sxhash."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((k0 *siphash-k0*)
         (k1 *siphash-k1*)
         (v0 (logxor #x736f6d6570736575 k0))
         (v1 (logxor #x646f72616e646f6d k1))
         (v2 (logxor #x6c7967656e657261 k0))
         (v3 (logxor #x7465646279746573 k1))
         (m (%u64 (sxhash obj))))
    (%sipcompress v0 v1 v2 v3 m)
    (%sipcompress v0 v1 v2 v3 (ash 8 56))
    (setf v2 (logxor v2 #xff))
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%sipround v0 v1 v2 v3)
    (%u64 (logxor v0 v1 v2 v3))))

(defun siphash64-object (obj)
  "SipHash-2-4 over canonical object bytes.
SECURE mode only supports a safe subset of object types and rejects unsupported
or circular structures."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%siphash64-octets (%object->octets obj)))

(defun coerce-test-function (test)
  (ctypecase test
    (function test)
    (symbol (symbol-function test))))

(defun default-hash-test-compatible-p (test test-fn)
  "Default hash modes are intentionally restricted to EQ/EQL/EQUAL semantics."
  (or (and (symbolp test)
           (member test '(eq eql equal)))
      (eq test-fn #'eq)
      (eq test-fn #'eql)
      (eq test-fn #'equal)))

(defun validate-hash-test-compatibility (test test-fn hash)
  (when (and (null hash)
             (not (default-hash-test-compatible-p test test-fn)))
    (error "Default hash modes are intentionally restricted to TEST EQ, EQL, or EQUAL. Supply explicit :HASH to override (TEST=~S)."
           test)))

(defun default-hash-function-p (hash-fn)
  (or (eq hash-fn #'xxhash64-object)
      (eq hash-fn #'siphash64-sxhash-object)
      (eq hash-fn #'siphash64-object)))

(defun resolve-hash-function (hash hash-mode)
  "Resolve a hash function from explicit HASH or HASH-MODE.
HASH-MODE values:
  :FAST   - xxHash64 over sxhash (default)
  :KEYED  - SipHash-2-4 over sxhash (legacy secure-mixer behavior)
  :SECURE - SipHash-2-4 over canonical safe object bytes."
  (cond
    (hash (let ((hash-fn (ctypecase hash
                           (function hash)
                           (symbol (symbol-function hash)))))
            (lambda (obj)
              (let ((value (funcall hash-fn obj)))
                (if (typep value '(unsigned-byte 64))
                    value
                    (error "Custom hash function ~S returned ~S for ~S; expected an unsigned 64-bit integer."
                           hash value obj))))))
    ((or (null hash-mode) (eq hash-mode :fast))
     #'xxhash64-object)
    ((eq hash-mode :keyed)
     #'siphash64-sxhash-object)
    ((eq hash-mode :secure)
     #'siphash64-object)
    (t (error "Unknown hash mode ~S (expected :FAST, :KEYED, or :SECURE)." hash-mode))))

(defun get-bits (hash depth)
  "Extract +SLICE-BITS+ chunk for DEPTH from HASH."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 64) hash)
           (type fixnum depth))
  (ldb (byte +slice-bits+ (* +slice-bits+ depth)) hash))

(defun get-index (bits bitmap)
  "Given the slice extracted from a hash at the present depth, find
the index in the current array corresponding to this bit sequence."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum bits)
           (type (unsigned-byte 64) bitmap))
  (logcount (ldb (byte bits 0) bitmap)))

(defun vec-insert (vec pos item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector vec)
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
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector vec)
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
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector vec)
           (type fixnum pos))
  (let ((v (copy-seq vec)))
    (declare (type simple-vector v))
    (setf (svref v pos) item)
    v))
