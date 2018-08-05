(ns clj-basic-algorithms.binary-search-tree.delete)

(defn leftmost-node [tree]
  (if (nil? (second tree))
    tree
    (recur (second tree))))

(defn rightmost-node [tree]
  (if (nil? (last tree))
    tree
    (recur (last tree))))

(defn delete [tree value]
  (let [nkey (first tree)
        lchild (second tree)
        rchild (last tree)]
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
                                (let [pred (first (rightmost-node lchild))]
                                  (list pred (delete lchild pred) rchild))
                                ;; in-order successor
                                (let [succ (first (leftmost-node rchild))]
                                  (list succ lchild (delete rchild succ))))))))
