(ns clj-basic-algorithms.binary-search-tree.delete)

;; get key of the node
(defn k [node] (first node))
(defn left [node] (second node))
(defn right [node] (last node))

(defn leftmost-node [tree]
  (if (nil? (left tree))
    tree
    (recur (left tree))))

(defn rightmost-node [tree]
  (if (nil? (right tree))
    tree
    (recur (right tree))))

(defn delete [tree value]
  (let [nkey (k tree)
        lchild (left tree)
        rchild (right tree)]
    (cond
      (nil? nkey) nil
      (< value nkey) (list nkey (delete lchild value) rchild)
      (> value nkey) (list nkey lchild (delete rchild value))
      (== value nkey) (cond
                        (= nil lchild rchild) nil
                        (nil? lchild) rchild
                        (nil? rchild) lchild
                        :else (if (zero? (rand-int 2))
                                ;; in-order predecessor
                                (let [pred (k (rightmost-node lchild))]
                                  (list pred (delete lchild pred) rchild))
                                ;; in-order successor
                                (let [succ (k (leftmost-node rchild))]
                                  (list succ lchild (delete rchild succ))))))))
