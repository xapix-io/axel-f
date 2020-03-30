(ns axel-f.excel.collections
  (:require [axel-f.excel.utils :as ut]))

(defn MAP*
  "Applies partialy defined formula to every element in a collection and returns an array."
  [^{:doc "Partialy defined formula with free variables to apply to the collection"} f
   & ^{:doc "Collection of elements"} colls]
  (apply map f colls))

(def MAP #'MAP*)

(defn FILTER*
  "Returns an array of elements that have been filtered based on a condition."
  [^{:doc "Condition predicate which will be applied to members of collection"} pred
   ^{:doc "Collection of elements"} coll]
  (filter pred coll))

(def FILTER #'FILTER*)

(defn SORT*
  "Sorts a collection by the values returned from applying a sorting function to each element in said collection."
  [^{:doc "Sorting function that will be applied to each element of the collection"} keyfn
   ^{:doc "Collection of elements"} coll]
  (sort-by keyfn coll))

(def SORT #'SORT*)

(defn CONCAT*
  "Concatenates arrays"
  [& ^{:doc "Arrays to concatenate"} colls]
  (apply concat colls))

(def CONCAT #'CONCAT*)

(defn WALK*
  ([f data] (WALK* f -1 data))
  ([f level data]
   (cond
     (= 0 level) (f data)
     (sequential? data) (map (partial WALK* f (dec level)) data)
     (map? data) (ut/map-vals (partial WALK* f (dec level)) data)
     :else (f data))))

(def WALK #'WALK*)

(def env
  {"MAP"    MAP
   "KEEP"   FILTER
   "CONCAT" CONCAT
   "SORT"   SORT
   "walk"   WALK})
