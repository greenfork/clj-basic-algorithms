(ns clj-basic-algorithms.binary-search-tree.search
  (:require [clj-basic-algorithms.binary-search-tree.core :refer :all]
            [clojure.test :refer :all]
            [criterium.core :refer [bench]])
  (:import [clj_basic_algorithms.binary_search_tree.core BST]))

;;; This file compares different implementations of a binary search tree and
;;; different implementations of a searching algorithm according to the time
;;; it takes to execute them.
;;;
;;; First the tree implementation is stated, then different searching
;;; algorithms follow.
;;;
;;; At the very end there's a test suite for searching algorithms and it is
;;; followed by a commented benchmark.

;;; `bstl-int` `bstv-int` IMPLEMENTATION

(defn value-int [tree]
  (if (integer? tree)
    tree
    (first tree)))

(defn left-child-int [tree]
  (if (integer? tree)
    nil
    (first (second tree))))

(defn right-child-int [tree]
  (if (integer? tree)
    nil
    (second (second tree))))

;; Composite literate searching algorithm
(defn literate-search-int [tree key]
  (if (= nil tree)
    nil
    (if (= key (value-int tree))
      tree
      (if (< key (value-int tree))
        (recur (left-child-int tree) key)
        (recur (right-child-int tree) key)))))

;; Fast complex cryptic searching algorithm
(defn cryptic-search-int [tree key]
  (if (integer? tree)
    (if (= tree key)
      tree
      nil)
    (if (= (first tree) key)
      tree
      (if (> (first tree) key)
        (recur (first (second tree)) key)
        (recur (second (second tree)) key)))))

;;; `bstl-arr` `bstv-arr` IMPLEMENTATION

(defn value-arr [tree] (first tree))

(defn left-child-arr [tree] (second tree))

(defn right-child-arr [tree] (last tree))

;; Composite literate searching algorithm
(defn literate-search-arr [tree key]
  (cond
    (nil? tree) nil
    (= key (value-arr tree)) tree
    (< key (value-arr tree)) (recur (left-child-arr tree) key)
    (> key (value-arr tree)) (recur (right-child-arr tree) key)))

;; Fast complex cryptic searching algorithm
(defn cryptic-search-arr [tree key]
  (cond
    (nil? tree) nil
    (= key (first tree)) tree
    (< key (first tree)) (recur (second tree) key)
    (> key (first tree)) (recur (last tree) key)))

;;; `bst-rec` IMPLEMENTATION

(defn literate-search-rec [tree key]
  (if (= key (:key tree))
    tree
    (if (nil? (:key tree))
      nil
      (if (< key (:key tree))
        (recur (:left tree) key)
        (recur (:right tree) key)))))

;;; Exhaustive testing of all flavors of search

(deftest search-int
  (doseq [f [(partial cryptic-search-int bstl-int)
             (partial literate-search-int bstl-int)]]
    (is (= (f 4) bstl-int))
    (is (= (f 2) '(2 (1 3))))
    (is (= (f 3) 3))
    (is (= (f 1) 1))
    (is (= (f 5) 5))
    (is (= (f 8) nil))))

(deftest search-arr
  (doseq [f [(partial cryptic-search-arr bstl-arr)
             (partial literate-search-arr bstl-arr)]]
    (is (= (f 4) bstl-arr))
    (is (= (f 2) '(2 (1 nil nil) (3 nil nil))))
    (is (= (f 3) '(3 nil nil)))
    (is (= (f 1) '(1 nil nil)))
    (is (= (f 5) '(5 nil nil)))
    (is (= (f 8) nil))))

(deftest search-rec
  (let [f (partial literate-search-rec bst-rec)]
    (is (= (f 4) bst-rec))
    (is (= (f 2) (BST. 2 (BST. 1 nil nil) (BST. 3 nil nil))))
    (is (= (f 3) (BST. 3 nil nil)))
    (is (= (f 1) (BST. 1 nil nil)))
    (is (= (f 5) (BST. 5 nil nil)))
    (is (= (f 8) nil))))

;;; Benchmarking

(comment
  (doseq [[message f] [["Literate search with bstl-int"
                        (partial literate-search-int bstl-int)]
                       ["Literate search with bstv-int"
                        (partial literate-search-int bstv-int)]
                       ["Cryptic search with bstl-int"
                        (partial cryptic-search-int bstl-int)]
                       ["Cryptic search with bstv-int"
                        (partial cryptic-search-int bstv-int)]
                       ["Literate search with bstl-arr"
                        (partial literate-search-arr bstl-arr)]
                       ["Literate search with bstv-arr"
                        (partial literate-search-arr bstv-arr)]
                       ["Cryptic search with bstl-arr"
                        (partial cryptic-search-arr bstl-arr)]
                       ["Cryptic search with bstv-arr"
                        (partial cryptic-search-arr bstv-arr)]
                       ["Literate search with bst-rec"
                        (partial literate-search-rec bst-rec)]]]
    (println (str "\n\n" message "\n"))
    (bench (do
             (f 4)
             (f 2)
             (f 3)
             (f 1)
             (f 5)
             (f 8)
             (f 8)
             (f -1)
             (f 2.5)
             (f 1.5)))))

;;; Short summary of Time Means

;; Literate search with bstl-int    4.389093 µs
;; Literate search with bstv-int    9.074148 µs
;; Cryptic  search with bstl-int    3.494030 µs
;; Cryptic  search with bstv-int    7.784723 µs
;; Literate search with bstl-arr    5.224145 µs
;; Literate search with bstv-arr    10.76883 µs
;; Cryptic  search with bstl-arr    4.430814 µs
;; Cryptic  search with bstv-arr    9.735281 µs
;; Literate search with bst-rec     3.476523 µs

;;; Literate search with bstl-int

;; Evaluation count : 13834440 in 60 samples of 230574 calls.
;;              Execution time mean : 4.389093 µs
;;     Execution time std-deviation : 60.186099 ns
;;    Execution time lower quantile : 4.332404 µs ( 2.5%)
;;    Execution time upper quantile : 4.524832 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 7 outliers in 60 samples (11.6667 %)
;; 	low-severe	 6 (10.0000 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers


;;; Literate search with bstv-int

;; Evaluation count : 6728760 in 60 samples of 112146 calls.
;;              Execution time mean : 9.074148 µs
;;     Execution time std-deviation : 257.590422 ns
;;    Execution time lower quantile : 8.925354 µs ( 2.5%)
;;    Execution time upper quantile : 9.554482 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 6 outliers in 60 samples (10.0000 %)
;; 	low-severe	 3 (5.0000 %)
;; 	low-mild	 3 (5.0000 %)
;;  Variance from outliers : 15.7690 % Variance is moderately inflated by outliers


;;; Cryptic search with bstl-int

;; Evaluation count : 18836580 in 60 samples of 313943 calls.
;;              Execution time mean : 3.494030 µs
;;     Execution time std-deviation : 490.973064 ns
;;    Execution time lower quantile : 3.133811 µs ( 2.5%)
;;    Execution time upper quantile : 4.601162 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 4 outliers in 60 samples (6.6667 %)
;; 	low-severe	 3 (5.0000 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 82.4174 % Variance is severely inflated by outliers


;;; Cryptic search with bstv-int

;; Evaluation count : 7751220 in 60 samples of 129187 calls.
;;              Execution time mean : 7.784723 µs
;;     Execution time std-deviation : 133.723365 ns
;;    Execution time lower quantile : 7.704948 µs ( 2.5%)
;;    Execution time upper quantile : 8.009776 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 3 (5.0000 %)
;; 	low-mild	 2 (3.3333 %)
;;  Variance from outliers : 6.2744 % Variance is slightly inflated by outliers


;;; Literate search with bstl-arr

;; Evaluation count : 11476020 in 60 samples of 191267 calls.
;;              Execution time mean : 5.224145 µs
;;     Execution time std-deviation : 32.999862 ns
;;    Execution time lower quantile : 5.193584 µs ( 2.5%)
;;    Execution time upper quantile : 5.267534 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers


;;; Literate search with bstv-arr

;; Evaluation count : 5588100 in 60 samples of 93135 calls.
;;              Execution time mean : 10.768826 µs
;;     Execution time std-deviation : 45.582692 ns
;;    Execution time lower quantile : 10.712305 µs ( 2.5%)
;;    Execution time upper quantile : 10.887720 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 5 (8.3333 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers


;;; Cryptic search with bstl-arr

;; Evaluation count : 13609020 in 60 samples of 226817 calls.
;;              Execution time mean : 4.430814 µs
;;     Execution time std-deviation : 35.830046 ns
;;    Execution time lower quantile : 4.399529 µs ( 2.5%)
;;    Execution time upper quantile : 4.525004 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 2 (3.3333 %)
;; 	low-mild	 3 (5.0000 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers


;;; Cryptic search with bstv-arr

;; Evaluation count : 6214200 in 60 samples of 103570 calls.
;;              Execution time mean : 9.735281 µs
;;     Execution time std-deviation : 265.816388 ns
;;    Execution time lower quantile : 9.605656 µs ( 2.5%)
;;    Execution time upper quantile : 10.495659 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 4 (6.6667 %)
;;  Variance from outliers : 14.2141 % Variance is moderately inflated by outliers


;;; Literate search with bst-rec

;; Evaluation count : 17404260 in 60 samples of 290071 calls.
;;              Execution time mean : 3.476523 µs
;;     Execution time std-deviation : 81.495793 ns
;;    Execution time lower quantile : 3.438332 µs ( 2.5%)
;;    Execution time upper quantile : 3.557834 µs (97.5%)
;;                    Overhead used : 11.764317 ns

;; Found 6 outliers in 60 samples (10.0000 %)
;; 	low-severe	 2 (3.3333 %)
;; 	low-mild	 4 (6.6667 %)
;;  Variance from outliers : 11.0216 % Variance is moderately inflated by outliers
