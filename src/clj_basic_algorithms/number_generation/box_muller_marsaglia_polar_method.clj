(ns clj-basic-algorithms.number-generation.box-muller-marsaglia-polar-method)

;;; Box-Muller-Marsaglia polar method
;;;
;;; Generate 2 random numbers from the specified Normal (Gaussian, bell)
;;; distribution.

(defn- rand+-1 [] (- (rand 2) 1))

(defn generate
  "`mu` is the mean, `sigma` is the standard deviation of the distribution."
  [mu sigma]
  (let [[x y s] (loop [x (rand+-1) y (rand+-1)]
                  (let [s (+ (* x x) (* y y))]
                    (if (< 0 s 1)
                      [x y s]
                      (recur (rand+-1) (rand+-1)))))
        variable (Math/sqrt (- (/ (* 2 (Math/log s)) s)))]
    [(+ mu (* x sigma variable))
     (+ mu (* y sigma variable))]))
