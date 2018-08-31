(ns clj-basic-algorithms.symbolic-regression.core
  (:require [clojure.zip :as zip])
  (:import [clojure.lang PersistentList]))

;;; Data to model

(defn f [x] (+ (* x x) x 1))

(def dataset (for [x (range -5 5 0.1)] [x (f x)]))

;;; Defining parameters

(def terminal-set #{'x 'R})

(def function-set #{'unchecked-add-int
                    'unchecked-subtract-int
                    'unchecked-multiply-int
                    '%
                    ;; FIXME: remove these after testing is complete
                    '+
                    '*})

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

(defn % [^Integer x ^Integer y]
  (if (zero? y)
    0
    (unchecked-divide-int x y)))

(defn R [] (- (rand 20) 10))

(defn treesize [tree] (count (flatten tree)))

(defn all-locations [tree]
  (take (treesize tree)
        (iterate #(let [subtree (zip/next %)]
                    (if (function-set (zip/node subtree))
                      (zip/next subtree)
                      subtree))
                 (zip/seq-zip tree))))

(defn random-location [tree]
  (if (seq? tree)
    (rand-nth (all-locations tree))
    tree))

;;; Evolving functions

;;; Crossover

(defn random-crossover [tree1 tree2]
  (let [loc1 (random-location tree1)
        loc2 (random-location tree2)]
    [(zip/root (zip/replace loc1 (zip/node loc2)))
     (zip/root (zip/replace loc2 (zip/node loc1)))]))

(def test-tree1 '(+ (* x (+ y z)) w))
(def test-tree2 '(+ (* x (+ x x)) x))
