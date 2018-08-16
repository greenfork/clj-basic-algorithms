(ns clj-basic-algorithms.index-hashing.core
  (:import [clojure.lang Murmur3]))

(defn ->int32 [x] (int (cond
                         (> x Integer/MAX_VALUE)
                         (+ Integer/MIN_VALUE (mod x (inc Integer/MAX_VALUE)))
                         (< x Integer/MIN_VALUE)
                         (dec (+ Integer/MAX_VALUE (mod x (Integer/MIN_VALUE))))
                         :else x)))
(defn u32+  [^Integer x ^Integer y] (unchecked-add-int x y))
(defn u32*  [^Integer x ^Integer y] (unchecked-multiply-int x y))
(defn <<32  [x y] (->int32 (bit-shift-left x y)))
(defn >>32  [^Integer x ^Integer y] (->int32 (bit-shift-right x y)))
(defn xor32 [^Integer x ^Integer y] (->int32 (bit-xor x y)))
(defn rand32 [] (->int32 (long (rand (* 2 Integer/MAX_VALUE)))))

;;; "Bad" hashing algorithms

(defn additive-hash [s]
  (reduce #(u32+ %1 (int %2)) 0 (str s)))

(defn xor-hash [s]
  (reduce #(xor32 %1 (int %2)) 0 (str s)))

(defn rotating-hash [s]
  (reduce #(xor32 (xor32 (<<32 %1 4) (>>32 %1 28)) (int %2)) 0 (str s)))

;;; "Good" hashing algorithms

(defn bernstein
  "Confusingly good, works best for small `s`."
  [s]
  (reduce #(u32+ (u32* 33 %1) (int %2)) 0 (str s)))

(defn bernstein-plus
  "Modified version of `bernstein-hash`, presumably a better one."
  [s]
  (reduce #(xor32 (u32* 33 %1) (int %2)) 0 (str s)))

(defn arx-hash
  "Generic function for add-rotate-xor technique."
  [s]
  (reduce #(xor32 %1 (u32+ (u32+ (<<32 %1 5) (>>32 %1 2)) (int %2))) 0 (str s)))

(defn FNV
  "FNV-1a version for 32 bits."
  [s]
  (let [h         2166136261
        FNV-prime 16777619]
    (reduce #(u32* (xor32 %1 (int %2)) FNV-prime) h (str s))))

(defn one-at-a-time [s]
  (let [h (reduce #(let [h (u32+ %1 (int %2))
                         h (u32+ h (<<32 h 10))
                         h (xor32 h (>>32 h 6))]
                     h)
                  0
                  (str s))
        h (u32+ h (<<32 h 3))
        h (xor32 h (>>32 h 11))
        h (u32+ h (<<32 h 15))]
    h))

(defn random-bitstring [] (rand32))

(defn zobrist-table
  ([i] (zobrist-table i 256))
  ([i j]
   (loop [i i
          outer-tab []]
     (if (zero? i)
       outer-tab
       (recur (dec i) (conj outer-tab
                            (loop [j j
                                   inner-tab []]
                              (if (zero? j)
                                inner-tab
                                (recur (dec j) (conj inner-tab
                                                     (random-bitstring)))))))))))

(defn zobrist
  "Note that `table` variable should be consistent throughout calling this
  function in order to get same hash for same value."
  ([s] (zobrist s 0x7fffffff (zobrist-table 100)))
  ([s mask table]
   (let [s (str s), length (count s)]
     (bit-and mask (reduce #(xor32 %1 ((table %2) (int ((vec s) %2))))
                           length
                           (range length))))))

(defn murmur3
  "Straight from the `clojure.lang` namespace."
  [x]
  (condp = (type x)
    java.lang.Long    (Murmur3/hashLong x)
    java.lang.Integer (Murmur3/hashInt x)
    java.lang.String  (Murmur3/hashUnencodedChars x)))
