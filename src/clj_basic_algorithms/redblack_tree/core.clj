(ns clj-basic-algorithms.redblack-tree.core
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defrecord RB [key left right color])

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
  (if (nil? node)
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

(s/def ::constant-black-depth #(every? (fn [path] (== (black-depth %)
                                                      (count-black-nodes path)))
                                       (every-path %)))
;; (def gen-rb-tree #(gen/fmap insert-multiple (gen/vector (gen/int))))

(s/def ::rb-tree (s/nilable
                  (s/and ::rb-type
                         (s/keys :req-un [::key ::left ::right ::color])
                         ::node
                         ::left-is-smaller
                         ::right-is-bigger)))
(s/def ::rb-tree-root (s/nilable (s/and ::rb-tree
                                        #(= :black (:color %))
                                        ::constant-black-depth)))

(s/def ::key   (s/nilable int?))
(s/def ::left  ::rb-tree)
(s/def ::right ::rb-tree)
(s/def ::color #{:red :black})

;;; Testing

(def rb-tree (RB. 13
                  (RB. 8
                       (RB. 1
                            (RB. nil nil nil :black)
                            (RB. 6
                                 (RB. nil nil nil :black)
                                 (RB. nil nil nil :black)
                                 :red)
                            :black)
                       (RB. 11
                            (RB. nil nil nil :black)
                            (RB. nil nil nil :black)
                            :black)
                       :red)
                  (RB. 17
                       (RB. 15
                            (RB. nil nil nil :black)
                            (RB. nil nil nil :black)
                            :black)
                       (RB. 25
                            (RB. 22
                                 (RB. nil nil nil :black)
                                 (RB. nil nil nil :black)
                                 :red)
                            (RB. 27
                                 (RB. nil nil nil :black)
                                 (RB. nil nil nil :black)
                                 :red)
                            :black)
                       :red)
                  :black))

(def small-rb-tree (RB. 13
                        (RB. 8
                             (RB. nil nil nil :black)
                             (RB. nil nil nil :black)
                             :red)
                        (RB. 17
                             (RB. nil nil nil :black)
                             (RB. nil nil nil :black)
                             :red)
                        :black))
