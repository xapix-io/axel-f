(ns axel-f.functions.stat
  (:require [axel-f.functions.math :as math]
            [axel-f.functions.coercion :as coercion]
            [axel-f.error :as error]
            [axel-f.macros :refer [def-excel-fn]]))

(defn- flatten-numbers [tr-coercer]
  (comp (mapcat #(cond
                   (string? %)     [(not-empty %)]
                   (sequential? %) (not-empty %)
                   (boolean? %)    [%]
                   (number? %)     [%]
                   :otherwise      nil))
        (filter identity)
        tr-coercer
        (filter number?)))

(def average
  ^{:desc "Returns the numerical average value in a dataset, ignoring text."
    :args [{:desc "The first value or range to consider when calculating the average value."}
           {:desc "Additional values or ranges to consider when calculating the average value."
            :opt true
            :repeatable true}]}
  (fn [& items]
    (let [tr-flatten-numbers (flatten-numbers (map coercion/excel-number))
          items (sequence tr-flatten-numbers items)
          len (count items)]
      (when-not (zero? len)
        (/ (apply math/sum-fn items)
           len)))))

(def count
  ^{:desc "Returns a count of the number of numeric values in a dataset."
    :args [{:desc "The first value or range to consider when counting."}
           {:desc "Additional values or ranges to consider when counting."
            :repeatable true
            :opt true}]}
  (fn [& items]
    (let [tr-flatten-numbers (flatten-numbers (map coercion/excel-number))]
      (clojure.core/count (sequence tr-flatten-numbers items)))))

(def max
  ^{:desc "Returns the maximum value in a numeric dataset."
    :args [{:desc "The first value or range to consider when calculating the maximum value."}
           {:desc "Additional values or ranges to consider when calculating the maximum value."
            :opt true
            :repeatable true}]}
  (fn [& items]
    (let [tr-coercer (map (fn [n]
                            (if-let [n (coercion/excel-number n)]
                              n
                              (throw (error/error "#VALUE!" (error/format-not-a-number-error "MAX" nil n))))))
          tr-flatten-numbers (flatten-numbers tr-coercer)]
      (apply clojure.core/max (into [] tr-flatten-numbers items)))))

(def min
  ^{:desc "Returns the minimum value in a numeric dataset."
    :args [{:desc "The first value or range to consider when calculating the minimum value."}
           {:desc "Additional values or ranges to consider when calculating the minimum value."
            :opt true
            :repeatable true}]}
  (fn [& items]
    (let [tr-coercer (map (fn [n]
                            (if-let [n (coercion/excel-number n)]
                              n
                              (throw (error/error "#VALUE!" (error/format-not-a-number-error "MIN" nil n))))))
          tr-flatten-numbers (flatten-numbers tr-coercer)]
      (apply clojure.core/min (into [] tr-flatten-numbers items)))))

(def-excel-fn
  "MIN" min
  "MAX" max
  "COUNT" count
  "AVERAGE" average)
