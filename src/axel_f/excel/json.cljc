(ns axel-f.excel.json
  (:require #?(:clj [cheshire.core :as json])))

(defn json-encode*
  "Returns a JSON-encoding String for the given object."
  [^{:doc "Object to encode"} to-encode]
  #?(:clj
     (json/generate-string to-encode)
     :cljs
     (js/JSON.stringify (clj->js to-encode))))

(def json-encode #'json-encode*)

(defn json-decode*
  "Returns an object corresponding to the given JSON-encoded string."
  [^{:doc "JSON-encoded string to decode"} to-decode]
  #?(:clj
     (json/parse-string to-decode)
     :cljs
     (js->clj (js/JSON.parse to-decode))))

(def json-decode #'json-decode*)

(def env
  {"JSONENCODE" (with-meta json-encode* (merge {:deprecated true} (meta #'json-encode*)))
   "JSONDECODE" (with-meta json-decode* (merge {:deprecated true} (meta #'json-decode*)))
   "JSON" {"ENCODE" json-encode
           "DECODE" json-decode}})
