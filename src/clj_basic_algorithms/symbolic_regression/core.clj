(ns clj-basic-algorithms.symbolic-regression.core
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest])
  (:import [clojure.lang PersistentList]))

;;; Data to model

(defn f [x] (+ (* x x) x 1))
(defn k [x] (+ (Math/pow x 4) (Math/pow x 3) (Math/pow x 2) x))
(defn g [x] (+ (Math/pow x 5) (- (* 2 (Math/pow x 3))) x))
(defn h [x] (+ (Math/pow x 6) (- (* 2 (Math/pow x 4))) (Math/pow x 2)))

(def dataset-f (for [x (range -5 5 0.1)] [x (f x)]))
(def dataset-k (for [x (range -5 5 0.1)] [x (k x)]))
(def dataset-g (for [x (range -5 5 0.1)] [x (g x)]))
(def dataset-h (for [x (range -5 5 0.1)] [x (h x)]))

;;; Defining parameters

(def terminal-set #{'x 'R})

(def function-arity {'+ 2, '- 2, '* 2, 'pd 2})
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
(def replication-rate  8)
(def mutation-rate     2)

;; When to terminate the evolution
(def minimum-error 0.01)

;;; Utility functions

(defn random-symbol [] (rand-nth symbol-vec))
(defn random-terminal [] (rand-nth terminal-vec))
(defn random-function [] (rand-nth function-vec))

(defn random-function-same-arity [f]
  (rand-nth (mapv first
                  (filter #(and (#{(function-arity f)} (second %))
                                (not= f (first %)))
                          function-arity))))

(defn pd
  "Divide `x` over `y` and in case `y` is zero return 0."
  [^Number x ^Number y]
  (if (zero? y)
    0
    (unchecked-divide-int x y)))

(defn R [] (- (rand 20) 10))

(defn treesize [tree] (if (seq? tree)(count (flatten tree)) 1))

(defn- locations [tree]
  (iterate #(let [subtree (zip/next %)]
              (if (function-set (zip/node subtree))
                (zip/next subtree)
                subtree))
           (zip/seq-zip tree)))

(defn all-locations [tree] (take (treesize tree) (locations tree)))

(defn random-location
  "Choose a node in the `tree` at random.

  Note: there is roughly 50% of functions and terminals in the tree."
  [tree]
  (if (seq? tree)
    (nth (locations tree) (rand-int (treesize tree)))
    tree))

(defn biased-random-location
  "Choose nodes `p` % of the time and leaves 1 - `p` % of the time."
  ([tree] (biased-random-location tree 0.9))
  ([tree p]
   {:pre [(< 0 p 1)]}
   (loop [loc (random-location tree)]
     (if (seq? (zip/node loc))
       (if (> p (rand))
         loc
         (recur (random-location tree)))
       (if (< p (rand))
         loc
         (recur (random-location tree)))))))

;;; Random tree generation

(defn full-tree
  "Generate a random tree fully filled up to `max-depth`."
  [max-depth]
  (if (== max-depth 1)
    (random-terminal)
    (let [f     (random-function)
          arity (function-arity f)]
      (cons f (repeatedly arity #(full-tree (dec max-depth)))))))

(defn grow-tree
  "Generate a random tree which can be shorter than its `max-depth`."
  [max-depth]
  (if (== max-depth 1)
    (random-terminal)
    (let [sym   (random-symbol)
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
  (let [max-depth (int (+ min-depth-limit
                          (rand-int (- max-depth-limit min-depth-limit))))]
    (if (> (rand) 0.5)
      (full-tree max-depth)
      (grow-tree max-depth))))

;; https://stackoverflow.com/a/52127548/8598954
(defn- ptc2
  "Generate a random tree with the `target-size`.
  The actual size will be increased by some small value if it is impossible to
  construct the tree with the exact specified `target-size`.

  Note: `target-size` is the number of nodes, not the same as its depth."
  [target-size]
  (if (== 1 target-size)
    (random-terminal)
    (let [f (random-function)
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
                f (random-function)
                arity (function-arity f)]
            (recur (assoc-in tree path (into [f] (repeat arity nil)))
                   (into (remove #{path} paths-to-nil)
                         (for [i (range arity)] (conj path (inc i))))
                   (inc size)))
          ;; In the end, fill all remaining slots with terminals
          (reduce (fn [tree path] (assoc-in tree path (random-terminal)))
                  tree paths-to-nil))))))

(defn- sequentiate [v] (map #(if (seqable? %) (sequentiate %) %) (seq v)))

(defn ptc2-tree [target-size]
  (sequentiate (ptc2 target-size)))

;;; Crossover

(defn random-crossover [tree1 tree2]
  (let [loc1 (random-location tree1)
        loc2 (random-location tree2)]
    [(zip/root (zip/replace loc1 (zip/node loc2)))
     (zip/root (zip/replace loc2 (zip/node loc1)))]))

(defn biased-crossover
  "Pick non-terminals `p` % of the time and terminals 1 - `p` % of the time.
  According to John Koza 90% is good, it is default."
  ([tree1 tree2] (biased-crossover tree1 tree2 0.9))
  ([tree1 tree2 p]
   {:pre (< 0 p 1)}
   (let [loc1 (biased-random-location tree1 p)
         loc2 (biased-random-location tree2 p)]
     [(zip/root (zip/replace loc1 (zip/node loc2)))
      (zip/root (zip/replace loc2 (zip/node loc1)))])))

(defn- similar-nodes
  "Return the number of similar nodes in 2 trees.
  The counting stops for the subtree as soon as 1 element does not match."
  [tree1 tree2]
  (let [tree1 (zip/seq-zip tree1)
        tree2 (zip/seq-zip tree2)]
    (if (and (zip/branch? tree1) (zip/branch? tree2))
      (let [f1 (zip/down tree1)
            f2 (zip/down tree2)]
        (if (= (zip/node f1) (zip/node f2))
          (reduce (fn [acc [st1 st2]] (+ acc (similar-nodes st1 st2)))
                  1
                  (map vector (zip/rights f1) (zip/rights f2)))
          0))
      ;; if one of the trees is a terminal
      (if (= tree1 tree2)
        1
        0))))

(defn homologous-crossover
  "Crossover trees at similar common points.

  `size-coeff` -- how similar should be the sizes of the trees; more is more
  similar; `size-coeff` > 0; 1 by default.

  `homology-coeff` -- how similar should be the nodes in the trees; more is
  less similar; `homology-coeff` > 0; 1 by default.

  `tries` -- number of tries to select the right subtrees; after `tries`
  number of failures to successfully select the trees, the algorithm falls
  back on random subtree selection; 0 is equal to infinity (very
  philosophical); 50 by default.

  The algorithm is based on

  R. M. MacCallum. Introducing a perl genetic programming system: and can
  meta-evolution solve the bloat problem? In C. Ryan, et al., editors, Genetic
  Programming, Proceedings of EuroGP’2003, volume 2610 of LNCS, pages
  364–373, Essex, 14-16 April 2003. Springer-Verlag. ISBN 3-540-00971-X."
  ([tree1 tree2] (homologous-crossover tree1 tree2 1 1 50))
  ([tree1 tree2 size-coeff homology-coeff tries]
   (let [[loc1 loc2] (loop [n (if (zero? tries) Integer/MAX_VALUE tries)]
                       (let [loc1 (random-location tree1)
                             loc2 (random-location tree2)
                             t1 (zip/node loc1)
                             t2 (zip/node loc2)
                             size1 (treesize t1)
                             size2 (treesize t2)
                             size-mult     (/ (Math/abs (- size1 size2))
                                              (max size1 size2))
                             homology-mult (/ (similar-nodes t1 t2)
                                              (min size1 size2))
                             probability (* (- 1 (Math/pow size-mult size-coeff))
                                            (Math/pow homology-mult homology-coeff))]
                         (cond
                           (zero? n) [loc1 loc2]
                           ;; Same node values occur more often, this check
                           ;; should alleviate this disadvantage.
                           (= t1 t2) (recur (dec n))
                           (> probability (rand)) [loc1 loc2]
                           :else (recur (dec n)))))]
     [(zip/root (zip/replace loc1 (zip/node loc2)))
      (zip/root (zip/replace loc2 (zip/node loc1)))])))

(defn size-fair-crossover
  "Crossover saving the size within close boundaries between 2 subtrees."
  [tree1 tree2]
  (let [loc1 (random-location tree1)
        size1 (treesize (zip/node loc1))
        bound1 (* (/ 2 3) size1)
        bound2 (* (/ 4 3) size1)
        loc2 (loop [loc (random-location tree2)]
               (if (<= bound1 (treesize (zip/node loc)) bound2)
                 loc
                 (recur (random-location tree2))))]
    [(zip/root (zip/replace loc1 (zip/node loc2)))
     (zip/root (zip/replace loc2 (zip/node loc1)))]))

;;; Mutation

(defn subtree-mutation
  "Replace a randomly selected subtree with another random subtree.

  The max depth should be no more than 15% larger than the initial tree
  according to:

  K. E. Kinnear, Jr. Evolving a sort: Lessons in genetic programming. In
  Proceedings of the 1993 International Conference on Neural Networks, volume
  2, pages 881–888, San Francisco, USA, 28 March-1 April 1993. IEEE Press.
  ISBN 0-7803-0999-5."
  [tree]
  (if (seq? tree)
   (let [loc (random-location tree)
         max-depth (inc (rand-int (dec (* 1.15 (treesize (zip/node loc))))))
         subtree (ramped-half-and-half 1 max-depth)]
     (zip/root (zip/replace loc subtree)))
   (random-terminal)))

(defn size-fair-subtree-mutation
  "Replace a randomly selected subtree with a similar-sized random subtree.
  The final size is in the range [l/2, 3l/2] where l is the size of the
  subtree."
  [tree]
  (if (seq? tree)
    (let [loc (random-location tree)
          loc-size (treesize (zip/node loc))
          min-depth (/ loc-size 2)
          max-depth (/ (* 3 loc-size) 2)
          subtree (if (not= 1 loc-size)
                    (ramped-half-and-half min-depth max-depth)
                    (ramped-half-and-half 1 1))]
      (zip/root (zip/replace loc subtree)))
    (random-terminal)))

(defn point-mutation
  "Replace a random node with another random node of the same type."
  [tree]
  (if (seq? tree)
    (let [loc (random-location tree)]
      (if (seq? (zip/node loc)) ;; if loc is a branch
        (zip/root (zip/replace (zip/next loc)
                               (random-function-same-arity (zip/node (zip/next loc)))))
        (zip/root (zip/replace loc (random-terminal)))))
    (random-terminal)))

;;; Specs

(s/def ::function function-set)
(s/def ::terminal terminal-set)
(def min-depth 2)
(def max-depth 8)
(s/def ::max-depth (s/int-in min-depth max-depth))
(s/def ::max-size  (s/int-in (int (Math/pow 2 min-depth)) (int (Math/pow 2 max-depth))))
(def gen-tree (fn [] (gen/fmap #(ramped-half-and-half 1 %)
                               (s/gen ::max-depth))))
(s/def ::tree (s/with-gen (s/or :tree (s/cat :function ::function
                                             :args     (s/* (s/or :branch ::tree
                                                                  :leaf   ::terminal)))
                                :element ::terminal)
                gen-tree))

;; Random tree generation specs

(s/fdef full-tree
  :args (s/cat :max-depth ::max-depth)
  :ret  ::tree)

(s/fdef grow-tree
  :args (s/cat :max-depth ::max-depth)
  :ret  ::tree)

(s/fdef ramped-half-and-half
  :args (s/and (s/cat :min-depth ::max-depth :max-depth ::max-depth)
               #(< (:min-depth %) (:max-depth %)))
  :ret  ::tree)

(s/fdef ptc2-tree
  :args (s/cat :target-size ::max-size)
  :ret  ::tree)

;; Mutation specs

(s/fdef subtree-mutation
  :args (s/cat :tree ::tree)
  :ret  ::tree)

(s/fdef size-fair-subtree-mutation
  :args (s/cat :tree ::tree)
  :ret  ::tree)

(s/fdef point-mutation
  :args (s/cat :tree ::tree)
  :ret  ::tree)

;;; Testing

(def test-tree1 '(+ (* x (+ y z)) w))
(def test-tree2 '(+ (* x (+ x x)) x))
(def test-tree3 '(- (* x x) (+ x (* (pd R x) R))))

(def test-tree50 '(pd (pd (pd (+ R (pd R x)) R) (pd (pd R x) (+ x (- R x))))
                     (- (- (- (* (+ x x) (* x x)) (pd (pd R R) (- (+ R x) x)))
                           (* x (pd (* R (pd x x)) R))) (+ (- R x) R))))

(deftest biased-random-location-test
  (let [probability 0.9
        epsilon 0.01
        loops 10000
        functions (loop [n loops function-counter 0]
                    (if (zero? n)
                      function-counter
                      (recur (dec n)
                             (if (seq? (zip/node (biased-random-location test-tree50
                                                                         probability)))
                               (inc function-counter)
                               function-counter))))]
    (is (< (- probability epsilon) (/ functions loops) (+ probability epsilon)))))

(defn test-it [sym]
  ((comp not #(contains? % :failure) stest/abbrev-result first stest/check) sym))

(deftest specs
  (testing "full-tree"
    (is (test-it `full-tree)))
  (testing "grow-tree"
    (is (test-it `grow-tree)))
  (testing "ramped-half-and-half"
    (is (test-it `ramped-half-and-half)))
  (testing "ptc2-tree"
    (is (test-it `ptc2-tree)))
  (testing "subtree-mutation"
    (is (test-it `subtree-mutation)))
  (testing "fair-size-subtree-mutation"
    (is (test-it `size-fair-subtree-mutation)))
  (testing "point-mutation"
    (is (test-it `point-mutation))))
