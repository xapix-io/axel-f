(ns axel-f.excel.object)

(defn OBJECT*
  "Construct an object from collection of key/value pairs"
  [^{:doc "Collection of key/value pairs"} kvs]
  (into {} (map #(into [] %) kvs)))

(def OBJECT #'OBJECT*)

(defn MERGE*
  "Merge multiple objects together. If a key occurs in more than one object, the mapping from latter (left-to-right) will be the mapping in the result."
  [& ^{:doc "Collection of objects to merge"} objs]
  (or (apply merge
             (map #(if (map? %) % (OBJECT %)) objs))
      {}))

(def MERGE #'MERGE*)

(def env
  {"OBJECT" {"NEW" OBJECT
             "MERGE" MERGE}})
