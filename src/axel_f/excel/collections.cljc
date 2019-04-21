(ns axel-f.excel.collections)

(defn MAP*
  "Applies partialy defined formula to every element in a collection and returns an array."
  [f coll]
  (map f coll))

(def MAP #'MAP*)

(defn FILTER*
  "Returns an array of elements that have been filtered based on a condition."
  [pred coll]
  (filter pred coll))

(def FILTER #'FILTER*)

(defn SORT*
  "Sorts a collection by the values returned from applying a sorting function to each element in said collection."
  [keyfn coll]
  (sort-by keyfn coll))

(def SORT #'SORT*)

(def env
  {"MAP"    MAP
   "FILTER" FILTER
   "SORT"   SORT})
