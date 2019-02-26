(ns axel-f.functions.stat
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            [axel-f.functions.math :as math]
            [axel-f.functions.coercion :as coercion]))

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

(defn average
  [& items]
  (let [tr-flatten-numbers (flatten-numbers (map coercion/excel-number))
        items (sequence tr-flatten-numbers items)
        len (count items)]
    (when-not (zero? len)
      (/ (apply math/sum-fn items)
         len))))

(def average-meta
  {:desc "Returns the numerical average value in a dataset, ignoring text."
   :args [{:desc "The first value or range to consider when calculating the average value."}
          {:desc "Additional values or ranges to consider when calculating the average value."
           :opt true
           :repeatable true}]})

(defn count*
  [& items]
  (let [tr-flatten-numbers (flatten-numbers (map (fn [x]
                                                   (try
                                                     (coercion/excel-number x)
                                                     (catch #?(:clj Throwable
                                                               :cljs js/Error) e
                                                       nil)))))]
    (count (sequence tr-flatten-numbers items))))

(def count*-meta
  {:desc "Returns a count of the number of numeric values in a dataset."
   :args [{:desc "The first value or range to consider when counting."}
          {:desc "Additional values or ranges to consider when counting."
           :repeatable true
           :opt true}]})

(defn max*
  [& items]
  (let [tr-coercer (map coercion/excel-number)
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply max (into [] tr-flatten-numbers items))))

(def max*-meta
  {:desc "Returns the maximum value in a numeric dataset."
   :args [{:desc "The first value or range to consider when calculating the maximum value."}
          {:desc "Additional values or ranges to consider when calculating the maximum value."
           :opt true
           :repeatable true}]})

(defn min*
  [& items]
  (let [tr-coercer (map coercion/excel-number)
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply min (into [] tr-flatten-numbers items))))

(def min*-meta
  {:desc "Returns the minimum value in a numeric dataset."
   :args [{:desc "The first value or range to consider when calculating the minimum value."}
          {:desc "Additional values or ranges to consider when calculating the minimum value."
           :opt true
           :repeatable true}]})

(def-excel-fn
  "MIN"
  min*
  min*-meta

  "MAX"
  max*
  max*-meta

  "COUNT"
  count*
  count*-meta

  "AVERAGE"
  average
  average-meta)
