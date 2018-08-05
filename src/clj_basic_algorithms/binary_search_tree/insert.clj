(ns clj-basic-algorithms.binary-search-tree.insert)

;; get key of the node
(defn k [node] (first node))

(defn left-child [node] (second node))

(defn right-child [node] (last node))

(defn insert-node [tree value]
  (cond
    (nil? tree) (list value nil nil)
    (< value (k tree)) (list (k tree)
                             (insert-node (left-child tree) value)
                             (right-child tree))
    (> value (k tree)) (list (k tree)
                             (left-child tree)
                             (insert-node (right-child tree) value))
    :else tree))

(defn insert-multiple-nodes [tree values]
  (reduce insert-node tree values))

(defn balanced-sequence
  "Create a sequence of `n` integers that builds a balanced BST when inserted.
  No need to balance the tree if you can balance the insertion sequence haha."
  [n]
  (loop [rs []
         queue (conj clojure.lang.PersistentQueue/EMPTY (range n))]
    (let [elem (peek queue)]
      (cond
        (nil? elem) rs ;; queue is empty
        (empty? elem) (recur rs (pop queue))
        (= 1 (count elem)) (recur (conj rs (first elem)) (pop queue))
        :else (let [middle (Math/floorDiv (cast Long (long (count elem))) 2)]
                (recur (conj rs (nth elem middle))
                       (conj (pop queue)
                             (take middle elem)
                             (nthrest elem (inc middle)))))))))

(comment
  (insert-multiple-nodes nil (balanced-sequence 1000)))
