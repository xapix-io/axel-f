(ns axel-f.excel.search
  (:require [matchete.data-form :as df]
            [matchete.core :as m]
            [#?(:clj clojure.tools.reader.edn
                :cljs cljs.tools.reader.edn) :as edn]))

(defn edn->pattern [edn-string]
  (df/make-pattern (edn/read-string edn-string)))

(defn json-search*
  ""
  [data edn-pattern]
  (let [pattern (edn->pattern edn-pattern)]
    (into []
          (map (fn [matches]
                 (into {}
                       (map (fn [[k v]]
                              [(subs (name k) 1) v]))
                       matches)))
          (m/matches pattern data))))

(def json-search #'json-search*)

(def env
  {"JSON" {"SEARCH" json-search}})
