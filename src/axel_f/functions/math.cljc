(ns axel-f.functions.math
  (:require [axel-f.error :as error]
            [axel-f.functions.coercion :as coercion]
            [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn]]))

(def-excel-fn round
  "Rounds a number to a certain number of decimal places according to standard rules."
  {:args [{:desc "The value to round to places number of places."}
          {:desc "The number of decimal places to which to round."
           :opt true}]}
  [d & [precision]]
  (if d
    (let [precision (or precision 0)]
      (if-let [d (coercion/excel-number d)]
        (if-let [precision (coercion/excel-number precision)]
          (let [factor (Math/pow 10 precision)
                res (/ (Math/round (* d factor)) factor)]
            (if (> precision 0)
              res
              (int res)))
          (throw (error/error "#VALUE!" (error/format-not-a-number-error "ROUND" 2 precision))))
        (throw (error/error "#VALUE!" (error/format-not-a-number-error "ROUND" 1 d)))))
    (throw (error/error "#N/A" "Wrong number of args (0) passed to: ROUND"))))

(defn sum-fn [& items]
  (reduce #?(:clj +' :cljs +)
          (map (fn [n]
                 (if-let [n (coercion/excel-number n)]
                   n
                   (throw (error/error "#VALUE!" (error/format-not-a-number-error "SUM" nil n)))))
               (flatten items))))

(def-excel-fn sum
  "Returns the sum of a series of numbers and/or references."
  {:args [{:desc "The first number or range to add together."}
          {:desc "Additional numbers or ranges to add to arg1."
           :opt true
           :repeatable true}]}
  [& items]
  (apply sum-fn items))
