(ns clj-basic-algorithms.binary-search-tree.walk)

;; get key of the node
(defn k [node] (first node))
(defn left [node] (second node))
(defn right [node] (last node))

;;; Depth-first fashion

(defn pre-order [tree f]
  (if (nil? tree)
    nil
    (do (f (k tree))
        (pre-order (left tree) f)
        (pre-order (right tree) f))))

(defn in-order [tree f]
  (if (nil? tree)
    nil
    (do
      (in-order (left tree) f)
      (f (k tree))
      (in-order (right tree) f))))

(defn post-order [tree f]
  (if (nil? tree)
    nil
    (do
      (post-order (left tree) f)
      (post-order (right tree) f)
      (f (k tree)))))

;;; Breadth-first fashion

(defn level-order [tree f]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY tree)]
    (let [elem (peek queue)]
      (cond
        (zero? (count queue)) nil
        (nil? elem) (recur (pop queue))
        :else (do
                (f (k elem))
                (recur (conj (pop queue) (left elem) (right elem))))))))
