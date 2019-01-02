(ns axel-f.v2.types
  (:require [clojure.set :refer [intersection union]]))

(defn coll-of? [type]
  (fn [& ts]
    (vec
     (map-indexed (fn [i t]
                    (if-not (contains? t type)
                      (ex-info "Type missmatch" {:expected type
                                                 :actual t
                                                 :argument i})
                      true))
                  ts))))

(defn mix-of [types]
  (apply union types))

(defn is-a? [child parent]
  (boolean (not-empty (intersection child parent))))

(defn extract-type [v]
  (cond
    (nil? v) nil
    (set? v) (union (set (mapcat #(extract-type %) v)) v)
    (keyword? v) #{v}
    (= (type v) java.lang.Class) (extract-type (parents v))
    (not= v (type v)) (extract-type #{(type v)})))
