(ns clj-basic-algorithms.symbolic-regression.core
  (:require [clojure.zip :as zip]
            [clojure.set :as set])
  (:import [clojure.lang PersistentList]))

;;; Data to model

(defn f [x] (+ (* x x) x 1))

(def dataset (for [x (range -5 5 0.1)] [x (f x)]))

;;; Defining parameters

(def terminal-set #{'x 'R})

(def function-arity {'+ 2, '- 2, '* 2, '% 2})
(def function-set (into #{} (keys function-arity)))

;; (def function-set #{['unchecked-add 2]
;;                     ['unchecked-subtract 2]
;;                     ['unchecked-multiply 2]
;;                     ['% 2]})

(def symbol-set (set/union terminal-set function-set))

(def terminal-vec (into [] terminal-set))
(def function-vec (into [] function-set))
(def symbol-vec (into [] symbol-set))

(defn fitness-function
  "Sum of absolute errors. Lower value corresponds to better candidate."
  [candidate]
  (let [fun (eval (list 'fn '[x] candidate))]
    (reduce (fn [acc [in out]] (+ acc (Math/abs (- (fun in) out)))) 0 dataset)))

(def crossover-rate    90)
(def reproduction-rate 8)
(def mutation-rate     2)

;; When to terminate the evolution
(def minimum-error 0.01)

;;; Utility functions

(defn % [^Number x ^Number y]
  (if (zero? y)
    0
    (unchecked-divide-int x y)))

(defn R [] (- (rand 20) 10))

(defn treesize [tree] (count (flatten tree)))

(defn- locations [tree]
  (iterate #(let [subtree (zip/next %)]
              (if (function-set (zip/node subtree))
                (zip/next subtree)
                subtree))
           (zip/seq-zip tree)))

(defn all-locations [tree] (take (treesize tree) (locations tree)))

(defn random-location [tree]
  (if (seq? tree)
    (nth (locations tree) (rand-int (treesize tree)))
    tree))

;;; Random tree generation

(defn full-tree
  "Generate a random tree fully filled up to `max-depth`."
  [max-depth]
  (if (== max-depth 1)
    (rand-nth terminal-vec)
    (let [f     (rand-nth function-vec)
          arity (function-arity f)]
      (cons f (repeatedly arity #(full-tree (dec max-depth)))))))

(defn grow-tree
  "Generate a random tree which can be shorter than its `max-depth`."
  [max-depth]
  (if (== max-depth 1)
    (rand-nth terminal-vec)
    (let [sym   (rand-nth symbol-vec)
          arity (function-arity sym)]
      (if (nil? arity)
        ;; `sym` is a terminal
        sym
        ;; `sym` is a function
        (cons sym (repeatedly arity #(grow-tree (dec max-depth))))))))

(defn ramped-half-and-half
  "Generate a random tree using the `grow-tree` and `full-tree` combination.
  The depth is chosen randomly between `min-depth-limit` and `max-depth-limit`."
  [min-depth-limit max-depth-limit]
  (let [max-depth (rand-nth (range min-depth-limit (inc max-depth-limit)))]
    (if (> (rand) 0.5)
      (full-tree max-depth)
      (grow-tree max-depth))))

;; https://stackoverflow.com/a/52127548/8598954
(defn- ptc2
  "Generate a random tree with the `target-size`.
  Note: `target-size` is the number of nodes, not the same as its depth."
  [target-size]
  (if (== 1 target-size)
    (rand-nth terminal-vec)
    (let [f (rand-nth function-vec)
          arity (function-arity f)]
      ;; Generate a tree like `[+ nil nil]` and iterate upon it
      (loop [tree (into [f] (repeat arity nil))
             ;; `paths-to-nil` will be something like ([1] [2]), being a list of
             ;; paths to the nil elements in the `tree`
             paths-to-nil (for [i (range arity)] [(inc i)])
             size 1]
        (if (< (+ size (count paths-to-nil)) target-size)
          ;; Replace one of the nils in the `tree` with a new node
          (let [path (rand-nth paths-to-nil)
                f (rand-nth function-vec)
                arity (function-arity f)]
            (recur (assoc-in tree path (into [f] (repeat arity nil)))
                   (into (remove #{path} paths-to-nil)
                         (for [i (range arity)] (conj path (inc i))))
                   (inc size)))
          ;; In the end, fill all remaining slots with terminals
          (reduce (fn [tree path] (assoc-in tree path (rand-nth terminal-vec)))
                  tree paths-to-nil))))))

(defn- sequentiate [v] (map #(if (sequential? %) (sequentiate %) %) (seq v)))

(defn ptc2-tree [target-size] (sequentiate (ptc2 target-size)))

;;; Crossover

(defn random-crossover [tree1 tree2]
  (let [loc1 (random-location tree1)
        loc2 (random-location tree2)]
    [(zip/root (zip/replace loc1 (zip/node loc2)))
     (zip/root (zip/replace loc2 (zip/node loc1)))]))

(def test-tree1 '(+ (* x (+ y z)) w))
(def test-tree2 '(+ (* x (+ x x)) x))
;; (def test-tree1 '(unchecked-add (unchecked-multiply x (unchecked-add y z)) w))
;; (def test-tree2 '(unchecked-add (unchecked-multiply x (unchecked-add x x)) x))
