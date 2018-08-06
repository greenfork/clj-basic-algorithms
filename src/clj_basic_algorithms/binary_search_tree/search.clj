(ns clj-basic-algorithms.binary-search-tree.search
  (:require [clojure.test :refer :all]))

;; get key of the node
(defn k [node] (first node))
(defn left [node] (second node))
(defn right [node] (last node))

(defn depth-first-search [tree value]
  (cond
    (nil? tree) nil
    (= value (k tree)) tree
    (< value (k tree)) (recur (left tree) value)
    (> value (k tree)) (recur (right tree) value)))

(defn breadth-first-search [tree value]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY tree)]
    (let [elem (peek queue)]
      (cond
        (zero? (count queue)) nil
        (nil? elem) (recur (pop queue))
        (= (k elem) value) elem
        :else (recur (conj (pop queue) (left elem) (right elem)))))))

(defn dls
  "Depth-limited depth-first search. Used in `iddfs`.
  It has 3 possible returns: `:please-stop`, `:please-continue` and an actual
  node if found."
  [node depth value]
  (cond
    (nil? node) :please-stop
    (zero? depth) (if (== value (k node)) node :please-continue)
    :else (let [l (dls (left node) (dec depth) value)
                r (dls (right node) (dec depth) value)
                responses #{:please-continue :please-stop}]
            (cond
              (= :please-stop l r) :please-stop
              (not (responses l)) l ;; found l
              (not (responses r)) r ;; found r
              :else :please-continue))))

(defn iddfs
  "Iterative deepening depth-first search or iterative deepening search."
  [tree value]
  (loop [depth 0]
    (let [rs (dls tree depth value)]
      (cond
        (= rs :please-stop) nil
        (= rs :please-continue) (recur (inc depth))
        :else rs))))

(def bstl '(4 (2 (1 nil nil) (3 nil nil)) (5 nil nil)))

(deftest search
  (doseq [f [(partial depth-first-search bstl)
             (partial breadth-first-search bstl)
             (partial iddfs bstl)]]
    (is (= (f 4) bstl))
    (is (= (f 2) '(2 (1 nil nil) (3 nil nil))))
    (is (= (f 3) '(3 nil nil)))
    (is (= (f 1) '(1 nil nil)))
    (is (= (f 5) '(5 nil nil)))
    (is (= (f 8) nil))))
