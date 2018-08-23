(ns clj-basic-algorithms.hash-table.core
  "A Hash Table using Murmur3 hashing function, separate chaining with linked
  lists as a collision resolution strategy and a Java array as the main data
  structure allowing for duplication keys.

  Please, do not use this in production. Use Clojure maps instead like so:
  `{:a 1 :b 2 :c 3}`."
  (:import [clojure.lang Murmur3]))

;;; Implementation
;;;
;;; Implementation is by no means complete and lacks a lot of checks and
;;; doesn't address corner cases.

(defn murmur3
  "Straight from the `clojure.lang` namespace."
  [x]
  (condp = (type x)
    java.lang.Long       (Murmur3/hashLong x)
    java.lang.Integer    (Murmur3/hashInt x)
    java.lang.String     (Murmur3/hashUnencodedChars x)
    clojure.lang.Keyword (+ (Murmur3/hashUnencodedChars (str x)) 0x9e3779b9)))

(def initial-size     50)
(def load-factor      0.7)
(def hashing-function murmur3)

(declare insert-multiple list-values)

(defrecord HashTable [array size load-factor hashing-function])

(defn initialize-array
  "Fill `array` with empty lists."
  [array]
  (let [limit (dec (count array))]
    (loop [index 0]
      (aset array index ())
      (if (== index limit)
        array
        (recur (inc index))))))

(defn create
  "Create new `HashTable` record."
  ([] (create initial-size load-factor hashing-function))
  ([initial-size load-factor hashing-function]
   (HashTable. (initialize-array (make-array Object initial-size))
               initial-size
               load-factor
               hashing-function)))

(defn resize
  "Move `hashtable`s key-value pairs to a larger new `HashTable`."
  [^HashTable hashtable ^Integer new-size]
  (insert-multiple (create new-size
                           (:load-factor hashtable)
                           (:hashing-function hashtable))
                   (list-values hashtable)))

;; Lots of tedious checks are omitted.
(defn insert
  "Insert new `value` with specified `key` into `hashtable`."
  [hashtable [key value]]
  (let [hashtable   (if (nil? hashtable) (create) hashtable)
        real-size   (count (list-values hashtable))
        resize?     (> (/ (inc real-size) (:size hashtable)) (:load-factor hashtable))
        hash        (:hashing-function hashtable)
        index       (mod (hash key) (:size hashtable))
        array-value (aget (:array hashtable) index)]
    (aset (:array hashtable) index (conj array-value [key value]))
    (if resize?
      (resize hashtable (* 2 (:size hashtable)))
      hashtable)))

(defn insert-multiple
  "Insert into `hashtable` a collection of key-value pairs."
  [hashtable key-values]
  ;; OPTIMIZE: `insert` does not need to compute `resize?` every time
  (reduce insert hashtable key-values))

(defn list-values
  "List all keys and values as a single collection."
  [hashtable]
  (mapcat identity (remove empty? (into [] (:array hashtable)))))

(defn hfind
  "Find value in a `hashtable` by `key`."
  [hashtable key]
  (let [index (mod ((:hashing-function hashtable) key) (inc (:size hashtable)))
        cell  (aget (:array hashtable) index)]
    (loop [f (first cell)
           r (rest cell)]
      (cond
        (= key (first f)) (second f)
        (empty? r)        nil
        :else             (recur (first r) (rest r))))))
