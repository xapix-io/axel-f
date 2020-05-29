(ns axel-f.excel.search
  (:require [matchete.core :as m]
            [axel-f.excel.json :as json]))

(defn- json->pattern [jo]
  (cond
    (map? jo)
    (into {}
          (map (fn [[k v]]
                 [(json->pattern k)
                  (json->pattern v)]))
          jo)

    (and (vector? jo)
         (#{"@cat" "@alt" "@scan" "@scan-indexed" "@def-rule"} (first jo)))
    (list* (symbol (subs (first jo) 1)) (map json->pattern (rest jo)))

    (vector? jo)
    (into []
          (map json->pattern)
          jo)

    (and (string? jo)
         (#{\? \! \$} (first jo)))
    (symbol jo)

    :else jo))

(defn json-search*
  ""
  [data json-pattern]
  (let [pattern (json->pattern (json/decode* json-pattern))]
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
