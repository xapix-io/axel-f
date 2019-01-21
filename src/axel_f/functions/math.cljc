(ns axel-f.functions.math
  (:require [axel-f.functions.coercion :as coercion]
            [axel-f.functions.core :refer [def-excel-fn]]))

(defn round
  ^{:desc "Rounds a number to a certain number of decimal places according to standard rules."
    :args [{:desc "The value to round to places number of places."}
           {:desc "The number of decimal places to which to round."
            :opt true}]}
  [d & [precision]]
  (let [precision (coercion/excel-number (or precision 0))
        d (coercion/excel-number d)]
    (let [factor (Math/pow 10 precision)
          res (/ (Math/round (* d factor)) factor)]
      (if (> precision 0)
        res
        (int res)))))

(defn sum-fn [& items]
  (reduce + (map coercion/excel-number (flatten items))))

(defn sum
  ^{:desc "Returns the sum of a series of numbers and/or references."
    :args [{:desc "The first number or range to add together."}
           {:desc "Additional numbers or ranges to add to arg1."
            :opt true
            :repeatable true}]}
  [& items]
  (apply sum-fn items))

(def-excel-fn
  "SUM" sum
  "ROUND" round)
