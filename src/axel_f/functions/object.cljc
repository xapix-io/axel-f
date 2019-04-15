(ns axel-f.functions.object
  (:require [axel-f.functions.core :refer [def-excel-fn]]))

(defn obj [kvs]
  (into {} (map #(into [] %) kvs)))

(defn obj-merge [& objs]
  (or (apply merge
             (map #(if (map? %) % (obj %)) objs))
      {}))

(def-excel-fn
  "OBJECT.NEW"
  obj
  {:desc "Construct an object from collection of key/value pairs"
   :args [{:desc "Collection of key/value pairs"}]}

  "OBJECT.MERGE"
  obj-merge
  {:desc "Merge multiple objects together. If a key occurs in more than one object, the mapping from latter (left-to-right) will be the mapping in the result."
   :args [{:desc "Object to merge"
           :repeatable true
           :optional true}]})
