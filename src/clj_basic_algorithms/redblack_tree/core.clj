(ns clj-basic-algorithms.redblack-tree.core
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defrecord RB [key left right color])
