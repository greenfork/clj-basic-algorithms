(ns clj-basic-algorithms.AVL-tree.core
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

;; bf -- balance factor, height factor
(defrecord AVL [key left right bf])

;;; Rotation
;;;
;;; Every rotation function takes a `root` which is a node (and tree) with a
;;; +2/-2 balance factor and returns the tree with stabilized balance
;;; factors (the proper rotation should be chosen).
;;;
;;; Though rotations can be ideally compressed into two functions, it becomes
;;; very cryptic and hard to understand.

(defn rotate-left [root]
  (let [new-root     (:right root)
        balance-diff (if (zero? (:bf new-root)) 1 0)]
    (AVL. (:key new-root)
          (AVL. (:key root) (:left root) (:left new-root) balance-diff)
          (:right new-root)
          (- balance-diff))))

(defn rotate-right [root]
  (let [new-root     (:left root)
        balance-diff (if (zero? (:bf new-root)) 1 0)]
    (AVL. (:key new-root)
          (:left new-root)
          (AVL. (:key root) (:right new-root) (:right root) (- balance-diff))
          balance-diff)))

(defn rotate-right-left [root]
  (let [right-tree    (:right root)
        new-root      (:left right-tree)
        left-tree-bf  (if (== 1 (:bf new-root)) -1 0)
        right-tree-bf (if (== -1 (:bf new-root)) 1 0)]
    (AVL. (:key new-root)
          (AVL. (:key root) (:left root) (:left new-root) left-tree-bf)
          (AVL. (:key right-tree) (:right new-root) (:right right-tree) right-tree-bf)
          0)))

(defn rotate-left-right [root]
  (let [left-tree     (:left root)
        new-root      (:right left-tree)
        left-tree-bf  (if (== 1 (:bf new-root)) -1 0)
        right-tree-bf (if (== -1 (:bf new-root)) 1 0)]
    (AVL. (:key new-root)
          (AVL. (:key left-tree) (:left left-tree) (:left new-root) left-tree-bf)
          (AVL. (:key root) (:right new-root) (:right root) right-tree-bf)
          0)))

;;; Insertion
;;;
;;; The main idea is to create nodes in a recursive manner on the branch where
;;; it should belong and then, upon unwinding the recursion back, correct the
;;; balance factors. Numbers -2...2 and different rotations follow from the
;;; algorithm for insertion.
;;;
;;; Both branches of insertion whether it's `<` or `>` can be compressed into
;;; one branch but it becomes very cryptic and hard to understand.

(defn- insert-r
  "Return a tree as the first argument and a flag as the second argument.
  The flag `:update-bf` or `:dont-update-bf` tells whether changes to a
  balance factor should propagate up to the root."
  [node value]
  (cond
    (nil? node) [(AVL. value nil nil 0) :update-bf]
    (< value (:key node))
    (let [[inserted flag] (insert-r (:left node) value)
          dont-update-bf? (= flag :dont-update-bf)
          bf-upd          (dec (:bf node))
          new-tree        (partial ->AVL (:key node) inserted (:right node))]
      (cond
        dont-update-bf?  [(new-tree (:bf node)) :dont-update-bf]
        (== -2 bf-upd)   (if (== -1 (:bf inserted))
                           [(rotate-right (new-tree bf-upd)) :dont-update-bf]
                           [(rotate-left-right (new-tree bf-upd)) :dont-update-bf])
        (== -1 bf-upd)   [(new-tree bf-upd) :update-bf]
        (==  0 bf-upd)   [(new-tree bf-upd) :dont-update-bf]))
    (> value (:key node))
    (let [[inserted flag] (insert-r (:right node) value)
          dont-update-bf? (= flag :dont-update-bf)
          bf-upd          (inc (:bf node))
          new-tree        (partial ->AVL (:key node) (:left node) inserted)]
      (cond
        dont-update-bf?  [(new-tree (:bf node)) :dont-update-bf]
        (== 2 bf-upd)    (if (== 1 (:bf inserted))
                           [(rotate-left (new-tree bf-upd)) :dont-update-bf]
                           [(rotate-right-left (new-tree bf-upd)) :dont-update-bf])
        (== 1 bf-upd)    [(new-tree bf-upd) :update-bf]
        (== 0 bf-upd)    [(new-tree bf-upd) :dont-update-bf]))
    :else [node :dont-update-bf]))

(defn insert [node value]
  (first (insert-r node value)))

(defn insert-multiple
  ([values]      (insert-multiple nil values))
  ([node values] (reduce insert node values)))

;;; Deletion
;;;
;;; The main idea is to delete the node in a recursive manner applying
;;; deletion on the each following node down the tree depending whether the
;;; value is higher or lower.
;;;
;;; The function is split up into 4 parts for clarity. It follows the same
;;; idea as the insertion, it may be beneficial to look at insertion first.
;;;
;;; 2 branches can be compressed into 1 but the choice was made for better
;;; clarity and further possible modifications.

(declare leftmost rightmost) ;; can be found in the Utility functions section

(defmulti ^:private delete-r
  "Dispatch deletion in 4 possible ways:

  - `:nil`      -- when there is no node to delete
  - `:equal`    -- when the node was found and it is time to delete it
  - `:to-left`  -- when the node is in the left subtree of the `node`
  - `:to-right` -- when the node is in the right subtree of the `node`.

  The return value consists of the node with an applied deletion (or the same
  node if the value for deletion is not found) and a sentinel value
  `:update-bf` or `:dont-update-bf` which signals whether changes to the
  balance factor should be propagated further to the root."
  (fn [node value]
    (let [k (:key node)]
      (cond
        (nil? k)     :nil
        (== value k) :equal
        (< value k)  :to-left
        (> value k)  :to-right))))

(defmethod delete-r :nil [node _] [node :dont-update-bf])

(defmethod delete-r :equal equal [node value]
  (let [lchild (:left node)
        rchild (:right node)
        k (:key node)]
    (cond
      (= nil lchild rchild) [nil :update-bf]
      (nil? lchild) [rchild :update-bf]
      (nil? rchild) [lchild :update-bf]
      :else (let [sub (if (zero? (rand-int 2))    ;; value to substitute with
                        (:key (rightmost lchild)) ;; in-order predecessor
                        (:key (leftmost rchild))) ;; in-order successor
                  ;; First, it is necessary to delete the substitution node.
                  [deleted flag] (delete-r node sub)
                  l-deleted (:left deleted)
                  r-deleted (:right deleted)
                  ;; Then we can apply the substitution. Here it is necessary
                  ;; to check whether there was a rotation and initial node
                  ;; which we want to delete will be substituted correctly.
                  ;; After the rotation the only 3 places where initial node
                  ;; could be are: top, left and right nodes.
                  left-tree (if (= (:key l-deleted) k)
                              (AVL. sub (:left l-deleted) (:right l-deleted)
                                    (:bf l-deleted))
                              l-deleted)
                  right-tree (if (= (:key r-deleted) k)
                               (AVL. sub (:left r-deleted) (:right r-deleted)
                                     (:bf r-deleted))
                               r-deleted)
                  middle-tree (if (= (:key deleted) k)
                                (AVL. sub left-tree right-tree
                                      (:bf deleted))
                                (AVL. (:key deleted) left-tree right-tree
                                      (:bf deleted)))]
              [middle-tree flag]))))

(defmethod delete-r :to-left left-branch [node value]
  (let [lchild (:left node)
        rchild (:right node)
        k (:key node)
        [deleted flag]  (delete-r lchild value)
        bf-upd          (inc (:bf node))
        dont-update-bf? (= flag :dont-update-bf)
        new-tree        (partial ->AVL k deleted rchild)]
    (cond
      dont-update-bf? [(new-tree (:bf node)) :dont-update-bf]
      (== bf-upd 2)   (if (== -1 (:bf rchild))
                        [(rotate-right-left (new-tree (:bf node))) :update-bf]
                        (let [rotated (rotate-left (new-tree (:bf node)))
                              flag (if (zero? (:bf rotated))
                                     :update-bf
                                     :dont-update-bf)]
                          [rotated flag]))
      (== bf-upd 1)   [(new-tree bf-upd) :dont-update-bf]
      (== bf-upd 0)   [(new-tree bf-upd) :update-bf])))

(defmethod delete-r :to-right right-branch [node value]
  (let [lchild (:left node)
        rchild (:right node)
        k (:key node)
        [deleted flag]  (delete-r rchild value)
        bf-upd          (dec (:bf node))
        dont-update-bf? (= flag :dont-update-bf)
        new-tree        (partial ->AVL k lchild deleted)]
    (cond
      dont-update-bf? [(new-tree (:bf node)) :dont-update-bf]
      (== bf-upd -2)  (if (== 1 (:bf lchild))
                        [(rotate-left-right (new-tree (:bf node))) :update-bf]
                        (let [rotated (rotate-right (new-tree (:bf node)))
                              flag (if (zero? (:bf rotated))
                                     :update-bf
                                     :dont-update-bf)]
                          [rotated flag]))
      (== bf-upd -1)  [(new-tree bf-upd) :dont-update-bf]
      (== bf-upd 0)   [(new-tree bf-upd) :update-bf])))

(defn delete [node value]
  (first (delete-r node value)))

(defn delete-multiple [node values]
  (reduce delete node values))

;;; Searching
;;;
;;; Common search algorithms for all binary search trees.

(defn dfs
  "Depth-first search."
  [node value]
  (cond
    (nil? node)             nil
    (nil? value)            nil
    (== value (:key node))  node
    (<  value (:key node))  (recur (:left node) value)
    (>  value (:key node))  (recur (:right node) value)))

(defn bfs
  "Breadth-first search."
  [node value]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY node)]
    (let [node (peek queue)]
      (cond
        (zero? (count queue))  nil
        (nil? value)           nil
        (nil? node)            (recur (pop queue))
        (== (:key node) value) node
        :else (recur (conj (pop queue) (:left node) (:right node)))))))

(defn- dls
  "Depth-limited depth-first search. Used in `iddfs`.
  It has 3 possible returns: `:please-stop`, `:please-continue` and an actual
  node if found."
  [node depth value]
  (cond
    (nil?  node)  :please-stop
    (zero? depth) (if (== value (:key node)) node :please-continue)
    :else         (let [l (dls (:left  node) (dec depth) value)
                        r (dls (:right node) (dec depth) value)
                        responses #{:please-continue :please-stop}]
                    (cond
                      (= :please-stop l r) :please-stop
                      (not (responses l))  l ;; found l
                      (not (responses r))  r ;; found r
                      :else                :please-continue))))

(defn iddfs
  "Iterative deepening depth-first search or iterative deepening search."
  [node value]
  (loop [depth 0]
    (let [result (dls node depth value)]
      (cond
        (nil? value)                nil
        (= result :please-stop)     nil
        (= result :please-continue) (recur (inc depth))
        :else                       result))))

;;; Utility functions

(defn AVL->map [avl]
  (if (nil? avl)
    nil
    {:key   (:key avl)
     :left  (AVL->map (:left avl))
     :right (AVL->map (:right avl))
     :bf    (:bf avl)}))

(defn AVL->keys [avl]
  (if (nil? avl)
    [nil]
    (sort (map :key (filter some? (tree-seq map?
                                            #(list (:left %) (:right %))
                                            (AVL->map avl)))))))

(defn map->AVL [m]
  (if (nil? m)
    nil
    (AVL. (:key m)
          (map->AVL (:left m))
          (map->AVL (:right m))
          (:bf m))))

(defn leftmost  [node] (if (nil? (:left  node)) node (recur (:left  node))))
(defn rightmost [node] (if (nil? (:right node)) node (recur (:right node))))

(defn height [node]
  (if (nil? node)
    0
    (max (inc (height (:left node)))
         (inc (height (:right node))))))

(defn balance-factor [node]
  (- (height (:right node)) (height (:left node))))

(defn throw-error [node msg]
  (println "Tree:")
  (prn (AVL->map node))
  (println "Balance factor:")
  (println (balance-factor node))
  (throw (ex-info msg
                  {:tree (AVL->map node)
                   :balance-factor (balance-factor node)})))

;;; Verification
;;;
;;; In case spec-based verification is not working.

(defn verify
  "Verify whether `node` is a true AVL tree.
  Optional `del-value` is for testing the `delete` function.
  Good ol' function style checking."
  ([node]
   (verify node nil))
  ([node del-value]
   (cond
     (nil? node) nil
     (some? (dfs node del-value))
     (throw-error node "Deleted value is present in the tree")
     (> (Math/abs (:bf node)) 1)
     (throw-error node "Balance factor is more than 1")
     (not= (balance-factor node) (:bf node))
     (throw-error node "Balance factor is incorrect")
     (and (some? (:left node)) (< (:key node) (:key (:left node))))
     (throw-error node "Left child is bigger than its parent")
     (and (some? (:right node)) (> (:key node) (:key (:right node))))
     (throw-error node "Right child is smaller than its parent")
     :else (do (verify (:left node))
               (verify (:right node))))))

;;; Specs
;;;
;;; For more about specs see clojure.spec.

(s/def ::avl-type        #(= clj_basic_algorithms.AVL_tree.core.AVL (type %)))
(s/def ::left-is-smaller #(or (nil? (:left  %)) (< (:key (:left  %)) (:key %))))
(s/def ::right-is-bigger #(or (nil? (:right %)) (> (:key (:right %)) (:key %))))
(s/def ::truly-balanced  #(== (:bf %) (balance-factor %)))
;; In case of incorrectly working `insert` function this generator will fail
;; to produce data for input to specs. In this case the function `verify`
;; should be used to check the correctness of functions.
(def gen-avl-tree #(gen/fmap insert-multiple (gen/vector (gen/int))))
(s/def ::avl-tree (s/with-gen
                    (s/nilable
                     (s/and ::avl-type
                            (s/keys :req-un [::key ::left ::right ::bf])
                            ::left-is-smaller
                            ::right-is-bigger
                            ::truly-balanced))
                    gen-avl-tree))

(s/def ::key   int?)
(s/def ::left  ::avl-tree)
(s/def ::right ::avl-tree)
(s/def ::bf    #{-1 0 1})

(s/def ::search
  #(let [tree-values (into #{} (AVL->keys (-> % :args :node)))
         tree        (-> % :args :node)
         value       (-> % :args :value)
         ret-key     (-> % :ret  :key)]
     (or (and (not (contains? tree-values value))
              (nil? ret-key))
         (and (contains? tree-values value)
              (== value ret-key)))))

(s/fdef dfs
  :args (s/cat :node ::avl-tree :value int?)
  :ret  ::avl-tree
  :fn   ::search)

(s/fdef bfs
  :args (s/cat :node ::avl-tree :value int?)
  :ret  ::avl-tree
  :fn   ::search)

(s/fdef iddfs
  :args (s/cat :node ::avl-tree :value int?)
  :ret  ::avl-tree
  :fn   ::search)

(s/fdef insert
  :args (s/cat :node ::avl-tree :value int?)
  :ret  ::avl-tree
  :fn   #(some? (dfs (:ret %) (-> % :args :value))))

(s/fdef delete
  :args (s/cat :node ::avl-tree :value int?)
  :ret  ::avl-tree
  :fn   #(nil? (dfs (:ret %) (-> % :args :value))))

;;; Testing

(defn test-it [sym]
  ((comp not #(contains? % :failure) stest/abbrev-result first stest/check) sym))

(t/deftest avl-tree
  (t/testing "insertion"
    (t/is (test-it `insert)))
  (t/testing "deletion"
    (t/is (test-it `delete)))
  (t/testing "depth-first search"
    (t/is (test-it `dfs)))
  (t/testing "breadth-first search"
    (t/is (test-it `bfs)))
  (t/testing "iterative deepening depth-first search"
    (t/is (test-it `iddfs))))
