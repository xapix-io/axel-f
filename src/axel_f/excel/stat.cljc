(ns axel-f.excel.stat
  (:require [axel-f.excel.math :as math]
            [axel-f.excel.coerce :as coerce]))

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

(defn AVERAGE*
  "Returns the numerical average value in a dataset, ignoring text."
  [& ^{:doc "Dataset item"} items]
  (let [tr-flatten-numbers (flatten-numbers (map coerce/excel-number))
        items (sequence tr-flatten-numbers items)
        len (count items)]
    (when-not (zero? len)
      (/ (apply math/sum-fn items)
         len))))

(def AVERAGE #'AVERAGE*)

(defn COUNT*
  "Returns a count of the number of numeric values in a dataset."
  [& ^{:doc "Dataset item"} items]
  (let [tr-flatten-numbers (flatten-numbers (map (fn [x]
                                                   (try
                                                     (coerce/excel-number x)
                                                     (catch #?(:clj Throwable
                                                               :cljs js/Error) e
                                                       nil)))))]
    (count (sequence tr-flatten-numbers items))))

(def COUNT #'COUNT*)

(defn LENGTH*
  "Returns the number of items in the collection."
  [^{:doc "Collection"} coll]
  (count coll))

(def LENGTH #'LENGTH*)

(defn MAX*
  "Returns the maximum value in a numeric dataset."
  [& ^{:doc "Dataset item"} items]
  (let [tr-coercer (map coerce/excel-number)
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply max (into [] tr-flatten-numbers items))))

(def MAX #'MAX*)

(defn MIN*
  "Returns the minimum value in a numeric dataset."
  [& ^{:doc "Dataset item"} items]
  (let [tr-coercer (map coerce/excel-number)
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply min (into [] tr-flatten-numbers items))))

(def MIN #'MIN*)

(def env
  {"MIN" MIN
   "MAX" MAX
   "AVERAGE" AVERAGE
   "COUNT" COUNT
   "LENGTH" LENGTH})
