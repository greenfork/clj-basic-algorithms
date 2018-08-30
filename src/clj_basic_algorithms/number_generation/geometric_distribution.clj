(ns clj-basic-algorithms.number-generation.geometric-distribution)

(defn generate
  "Take a sample from the geometric distribution.
  `min` is the minimally allowed number. `p` is the probability that `min`
  value should be increased."
  [min p]
  {:pre [(< 0 p 1)]}
  (loop [number min]
    (if (> p (rand))
      (recur (inc number))
      number)))
