(ns clj-basic-algorithms.redblack-tree.core
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defrecord RB [key left right color])

;;; Rotation
;;;
;;; Rotations are applied to fix the red violation (a red node can't have red
;;; children) when the uncle (a sibling of a parent) of the inserted node is
;;; _black_.

(defn rotate-left [root]
  (let [new-root (:right root)]
    (RB. (:key new-root)
         (RB. (:key root) (:left root) (:left new-root) :red)
         (:right new-root)
         :black)))

(defn rotate-right [root]
  (let [new-root (:left root)]
    (RB. (:key new-root)
         (:left new-root)
         (RB. (:key root) (:right new-root) (:right root) :red)
         :black)))

(defn rotate-right-left [root]
  (let [right-tree (:right root)
        new-root   (:left right-tree)]
    (RB. (:key new-root)
         (RB. (:key root) (:left root) (:left new-root) :red)
         (RB. (:key right-tree) (:right new-root) (:right right-tree) :red)
         :black)))

(defn rotate-left-right [root]
  (let [left-tree (:left root)
        new-root  (:right left-tree)]
    (RB. (:key new-root)
         (RB. (:key left-tree) (:left left-tree) (:left new-root) :red)
         (RB. (:key root) (:right new-root) (:right root) :red)
         :black)))

;;; Color flip
;;;
;;; Color flip is applied to fix the red violation (a red node can't have red
;;; children) when the uncle (a sibling of a parent) of the inserted node is
;;; _red_.

(defn color-flip
  "Flip the color of the `root` and its children."
  [root]
  (let [lchild (:left  root)
        rchild (:right root)]
    (RB. (:key root)
         (RB. (:key lchild) (:left lchild) (:right lchild) :black)
         (RB. (:key rchild) (:left rchild) (:right rchild) :black)
         :red)))

;;; Insertion
;;;
;;; Insert the node as in a normal binary search tree and apply balancing
;;; while the recursion is unwinding.
;;;
;;; Upon unwinding the recursion there is no data left about its history,
;;; namely grandchildren which were visited. This is why the function
;;; `balance` has to consider all the cases.

(declare create-leaf)

(defn balance [root]
  (let [lchild (:left root)
        rchild (:right root)]
    (if (= :red (:color lchild) (:color rchild))
      [(color-flip root) :please-check]
      (cond
        (= :red (:color lchild) (:color (:left lchild)))
        [(rotate-right root) :please-stop]
        (= :red (:color lchild) (:color (:right lchild)))
        [(rotate-left-right root) :please-stop]
        (= :red (:color rchild) (:color (:right rchild)))
        [(rotate-left root) :please-stop]
        (= :red (:color rchild) (:color (:left rchild)))
        [(rotate-right-left root) :please-stop]))))

(defn- insert-r
  "Return the found tree with `value` in the `node` and a sentinel value.
  Sentinel value can be one of the following:

  `:please-check` -- nothing should be done, process as usual;
  `:please-stop`  -- tree is balanced, stop checking it;
  `:code-red`     -- there is a red violation, balance the tree."
  [node value]
  (cond
    (nil? (:key node)) [(RB. value (create-leaf) (create-leaf) :red)
                        :please-check]
    (< value (:key node))
    (let [[inserted flag] (insert-r (:left node) value)
          new-tree        (RB. (:key node) inserted (:right node) (:color node))
          red-violation?  (= :red (:color node) (:color inserted))]
      (cond
        (= :please-stop flag) [new-tree :please-stop]
        red-violation?        [new-tree :code-red]
        (= :code-red flag)    (balance new-tree)
        :else                 [new-tree :please-check]))
    (> value (:key node))
    (let [[inserted flag] (insert-r (:right node) value)
          new-tree        (RB. (:key node) (:left node) inserted (:color node))
          red-violation?  (= :red (:color node) (:color inserted))]
      (cond
        (= :please-stop flag) [new-tree :please-stop]
        red-violation?        [new-tree :code-red]
        (= :code-red flag)    (balance new-tree)
        :else                 [new-tree :please-check]))
    :else [node :please-stop]))

(defn insert [node value]
  (let [tree (first (insert-r node value))]
    (if (= :red (:color tree))
      (RB. (:key tree) (:left tree) (:right tree) :black)
      tree)))

(defn insert-multiple
  ([values]      (insert-multiple nil values))
  ([node values] (reduce insert node values)))

;;; Deletion

(declare leaf? red? black? color create-leaf leftmost rightmost)

(defn- balance-code-black [root]
  (let [del-dir (if (leaf? (:left root)) :left :right)
        opp-dir (if (= :left del-dir) :right :left)
        deleted (del-dir root)
        sibling (opp-dir root)
        sib-outer-child (opp-dir sibling)
        sib-inner-child (del-dir sibling)
        rotate-single (if (= :left del-dir) rotate-left rotate-right)
        rotate-double (if (= :left del-dir) rotate-right-left rotate-left-right)
        rotate-sibling (if (= :left del-dir) rotate-right rotate-left)]
    (cond
      (every? #(= :black (:color %)) [root sibling sib-outer-child sib-inner-child])
      [(map->RB {:key (:key root)
                 del-dir deleted
                 opp-dir (color sibling :red)
                 :color :black})
       :code-black]
      (and (every? #(= :black (:color %)) [sibling sib-outer-child sib-inner-child])
           (= :red (:color root)))
      [(map->RB {:key (:key root)
                 del-dir deleted
                 opp-dir (color sibling :red)
                 :color :black})
       :please-stop]
      (and (= :black (:color sibling))
           (= :red (:color sib-outer-child)))
      (let [root-color (:color root)
            rotated (rotate-single root)]
        [(map->RB {:key (:key rotated)
                   del-dir (color (del-dir rotated) :black)
                   opp-dir (color (opp-dir rotated) :black)
                   :color root-color})
         :please-stop])
      (and (= :black (:color sibling) (:color sib-outer-child))
           (= :red (:color sib-inner-child)))
      (let [root-color (:color root)
            rotated (rotate-double root)]
        [(map->RB {:key (:key rotated)
                   del-dir (color (del-dir rotated) :black)
                   opp-dir (color (opp-dir rotated) :black)
                   :color root-color})
         :please-stop])
      (and (= :red (:color sibling) (:color (opp-dir sib-inner-child)))
           (= :black (:color root) (:color sib-inner-child)))
      (let [rotated (rotate-double root)]
        [(map->RB {:key (:key rotated)
                   del-dir (color (del-dir rotated) :black)
                   opp-dir (map->RB {:key (:key (opp-dir rotated))
                                     del-dir (color (del-dir (opp-dir rotated)) :black)
                                     opp-dir (opp-dir (opp-dir rotated))
                                     :color :red})
                   :color :black})
         :please-stop])
      (and (= :red (:color sibling) (:color (del-dir sib-inner-child)))
           (= :black (:color root) (:color sib-inner-child)))
      (let [rotated (rotate-double
                     (map->RB {:key (:key root)
                               del-dir deleted
                               opp-dir (map->RB {:key (:key sibling)
                                                 del-dir (rotate-sibling sib-inner-child)
                                                 opp-dir sib-outer-child
                                                 :color (:color sibling)})
                               :color (:color root)}))]
        [(map->RB {:key (:key rotated)
                   del-dir (color (del-dir rotated) :black)
                   opp-dir (map->RB {:key (:key (opp-dir rotated))
                                     del-dir (color (del-dir (opp-dir rotated)) :black)
                                     opp-dir (opp-dir (opp-dir rotated))
                                     :color :red})
                   :color :black})
         :please-stop])
      (and (= :red (:color sibling))
           (every? #(= :black (:color %)) [root sib-inner-child sib-outer-child]))
      (let [rotated (rotate-single root)]
        [(map->RB {:key (:key rotated)
                   del-dir (map->RB {:key (:key (del-dir rotated))
                                     del-dir deleted
                                     opp-dir (color (opp-dir (del-dir rotated)) :red)
                                     :color :black})
                   opp-dir (color (opp-dir rotated) :black)
                   :color :black})
         :please-continue]))))

(defn- delete-r
  [node value]
  (let [lchild (:left node)
        rchild (:right node)
        k      (:key node)]
    (cond
      (nil? k) [(create-leaf) :please-stop]
      (== value k)
      (cond
        (red? node) [lchild :please-stop] ;; lchild and rchild are equal leaves
        (and (leaf? lchild) (red?  rchild)) [(color rchild :black) :please-stop]
        (and (red?  lchild) (leaf? rchild)) [(color lchild :black) :please-stop]
        (and (leaf? lchild) (leaf? rchild)) [(create-leaf) :code-black]
        ;; `node` is black and both children are not leaves
        :else (let [sub (if (zero? (rand-int 2))    ;; value to substitute with
                          (:key (rightmost lchild)) ;; in-order predecessor
                          (:key (leftmost rchild))) ;; in-order successor
                    [deleted flag] (delete-r node sub)
                    l-deleted (:left deleted)
                    r-deleted (:right deleted)
                    ;; Then we can apply the substitution. Here it is necessary
                    ;; to check whether there was a rotation and initial node
                    ;; which we want to delete will be substituted correctly.
                    ;; After the rotation the only 3 places where initial node
                    ;; could be are: top, left and right nodes.
                    left-tree (if (= (:key l-deleted) k)
                                (RB. sub (:left l-deleted) (:right l-deleted)
                                     (:color l-deleted))
                                l-deleted)
                    right-tree (if (= (:key r-deleted) k)
                                 (RB. sub (:left r-deleted) (:right r-deleted)
                                      (:color r-deleted))
                                 r-deleted)
                    middle-tree (if (= (:key deleted) k)
                                  (RB. sub left-tree right-tree
                                       (:color deleted))
                                  (RB. (:key deleted) left-tree right-tree
                                       (:color deleted)))]
                [middle-tree flag]))
      (<  value k)
      (let [[deleted flag] (delete-r lchild value)
            new-tree       (RB. k deleted rchild (:color node))]
        (cond
          (= flag :please-stop) [new-tree :please-stop]
          (= flag :code-black)  (balance-code-black new-tree)
          :else                 [new-tree :please-continue]))
      (>  value k)
      (let [[deleted flag] (delete-r rchild value)
            new-tree       (RB. k lchild deleted (:color node))]
        (cond
          (= flag :please-stop) [new-tree :please-stop]
          (= flag :code-black)  (balance-code-black new-tree)
          :else                 [new-tree :please-continue])))))

(defn delete
  [node value]
  (if (or (nil? node)
          (nil? value)
          (nil? (:key node))
          (and (leaf? (:left node)) (leaf? (:right node))))
    (create-leaf)
    (first (delete-r node value))))

;;; Searching
;;;
;;; Common search algorithms for all binary search trees.

(defn dfs
  "Depth-first search."
  [node value]
  (cond
    (or (nil? node)
        (nil? value)
        (nil? (:key node))) nil
    (== value (:key node))  node
    (<  value (:key node))  (recur (:left node) value)
    (>  value (:key node))  (recur (:right node) value)))

;;; Utility functions

(defn RB->map [rb]
  (if (nil? rb)
    nil
    {:key   (:key rb)
     :left  (RB->map (:left rb))
     :right (RB->map (:right rb))
     :color (:color rb)}))

(defn RB->keys [rb]
  (if (nil? rb)
    [nil]
    (->> (tree-seq map? #(list (:left %) (:right %)) (RB->map rb))
         (filter some?)
         (map :key)
         (filter some?))))

(defn map->RB [m]
  (if (nil? m)
    nil
    (RB. (:key m)
         (map->RB (:left m))
         (map->RB (:right m))
         (:color m))))

(defn leaf? [node] (= nil (:left node) (:right node)))

(defn- every-path-r
  [node path]
  (if (leaf? node)
    path
    (let [lchild (:left  node)
          rchild (:right node)
          lpath (every-path-r lchild (conj path [(:key lchild) (:color lchild)]))
          rpath (every-path-r rchild (conj path [(:key rchild) (:color rchild)]))]
      (cond
        (and (list? lpath) (list? rpath)) (hash-set lpath rpath)
        (and (set?  lpath) (list? rpath)) (conj lpath rpath)
        (and (list? lpath) (set?  rpath)) (conj rpath lpath)
        (and (set?  lpath) (set?  rpath)) (clojure.set/union rpath lpath)))))

(defn every-path
  "Return a collection of all possible paths consisting of keys and colors."
  [node]
  (if (or (nil? node) (leaf? node))
    #{'([nil :black])}
    (every-path-r node (list [(:key node) (:color node)]))))

(defn count-black-nodes [path]
  (reduce #(if (= :black (second %2)) (inc %1) %1) 0 path))

(defn black-depth [node]
  (loop [node node, acc 0]
    (cond
      (nil? node)              acc
      (= :red (:color node))   (recur (:left node) acc)
      (= :black (:color node)) (recur (:left node) (inc acc)))))

(defn red? [node] (= :red (:color node)))
(defn black? [node] (= :black (:color node)))

(defn color
  "Color the node with a specified... ahhh, `coloUr`."
  [node colour]
  (RB. (:key node) (:left node) (:right node) colour))

(defn create-leaf [] (RB. nil nil nil :black))

(defn leftmost  [node] (if (leaf? (:left  node)) node (recur (:left  node))))
(defn rightmost [node] (if (leaf? (:right node)) node (recur (:right node))))

;;; Specs
;;;
;;; Binary search tree specific:
;;;
;;; 1. Left child is of a smaller value than its parent.
;;;
;;; 2. Right child is of a bigger value than its parent.
;;;
;;; Red-Black tree specific:
;;;
;;; 1. Each node is either red or black.
;;;
;;; 2. The root is black.
;;;
;;; 3. All leaves (NIL) are black.
;;;
;;; 4. If a node is red, then both its children are black.
;;;
;;; 5. Every path from a given node to any of its descendant NIL nodes
;;;    contains the same number of black nodes.
;;;
;;; Miscellaneous:
;;;
;;; 1. Tree is of type/class `RB`.

(s/def ::rb-type #(= clj_basic_algorithms.redblack_tree.core.RB (type %)))

(s/def ::left-is-smaller #(or (nil? (:left  %))
                              (nil? (:key (:left  %)))
                              (<    (:key (:left  %)) (:key %))))
(s/def ::right-is-bigger #(or (nil? (:right %))
                              (nil? (:key (:right %)))
                              (>    (:key (:right %)) (:key %))))

(defn red-node? [node]
  (and (= :red (:color node))
       (= :black (:color (:left node)) (:color (:right node)))))

;; Because `s/or` destructures, it is unsuitable for combinations with `s/and`.
(s/def ::node #(or (and (leaf? %)
                        (= :black (:color %))
                        (= nil (:key %)))
                   (and (not (leaf? %))
                        (or (= :black (:color %))
                            (red-node? %)))))

(s/def ::constant-black-depth #(every? (fn [path] (= (black-depth %)
                                                     (count-black-nodes path)))
                                       (every-path %)))

(def gen-rb-tree #(gen/fmap insert-multiple (gen/vector (gen/int))))

(s/def ::rb-tree (s/nilable
                  (s/and ::rb-type
                         (s/keys :req-un [::key ::left ::right ::color])
                         ::node
                         ::left-is-smaller
                         ::right-is-bigger)))
(s/def ::rb-tree-root (s/with-gen
                        (s/nilable
                         (s/and ::rb-tree
                                #(= :black (:color %))
                                ::constant-black-depth))
                        gen-rb-tree))

(s/def ::key   (s/nilable int?))
(s/def ::left  ::rb-tree)
(s/def ::right ::rb-tree)
(s/def ::color #{:red :black})

(s/def ::search
  #(let [tree-values (->> % :args :node RB->keys (filter some?) (into #{}))
         value       (-> % :args :value)
         ret-key     (-> % :ret  :key)]
     (or (and (not (contains? tree-values value))
              (nil? ret-key))
         (and (contains? tree-values value)
              (== value ret-key)))))

(s/fdef dfs
  :args (s/cat :node ::rb-tree-root :value int?)
  :ret  ::rb-tree
  :fn   ::search)

(s/fdef insert
  :args (s/cat :node ::rb-tree-root :value int?)
  :ret  ::rb-tree-root
  :fn   #(some? (dfs (:ret %) (-> % :args :value))))

(s/fdef delete
  :args (s/cat :node ::rb-tree-root :value int?)
  :ret  ::rb-tree-root
  :fn   #(nil? (dfs (:ret %) (-> % :args :value))))

;;; Testing

(defn test-it [sym]
  ((comp not #(contains? % :failure) stest/abbrev-result first stest/check) sym))

(t/deftest rb-tree-test
  (t/testing "insertion"
    (t/is (test-it `insert)))
  (t/testing "deletion"
    (t/is (test-it `delete)))
  (t/testing "depth-first search"
    (t/is (test-it `dfs))))

;;; Example

(def rb-tree (RB. 13
                  (RB. 8
                       (RB. 1
                            (create-leaf)
                            (RB. 6 (create-leaf) (create-leaf) :red)
                            :black)
                       (RB. 11 (create-leaf) (create-leaf) :black)
                       :red)
                  (RB. 17
                       (RB. 15 (create-leaf) (create-leaf) :black)
                       (RB. 25
                            (RB. 22 (create-leaf) (create-leaf) :red)
                            (RB. 27 (create-leaf) (create-leaf) :red)
                            :black)
                       :red)
                  :black))

;;; `rb-tree` scheme
;;;
;;; N -- nil node, black.

;;                       13,B
;;                     /      \
;;                   /          \
;;                 /              \
;;               8,R              17,R
;;             /     \          /      \
;;           /        \        /         \
;;         1,B       11,B    15,B       25,B
;;        /   \      /  \    /  \     /      \
;;       N    6,R   N    N  N    N  22,R     27,R
;;           /   \                 /    \   /    \
;;          N     N               N      N N      N
