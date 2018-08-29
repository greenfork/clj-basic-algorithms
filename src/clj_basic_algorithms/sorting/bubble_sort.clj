(ns clj-basic-algorithms.sorting.bubble-sort
  (:require [clojure.test :refer :all]))

(def array (int-array (repeatedly 100 #(rand-int 1000))))

(defn bubble-sort! [array]
  (let [swapped (atom true)
        limit   (dec (count array))]
    (while @swapped
      (reset! swapped false)
      (dotimes [i limit]
        (let [i+1  (inc i)
              ai   (aget array i)
              ai+1 (aget array i+1)]
          (when (< ai+1 ai)
            (aset-int array i ai+1)
            (aset-int array i+1 ai)
            (reset! swapped true)))))))

(deftest sorting
  (let [array      (int-array (repeatedly 100 #(rand-int 1000)))
        array->vec (fn [array] (into [] array))]
    (bubble-sort! array)
    (is (= (array->vec array) (sort (array->vec array))))))
