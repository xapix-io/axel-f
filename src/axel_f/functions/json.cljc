(ns axel-f.functions.json
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            #?(:clj [cheshire.core :as json])))

(defn json-encode
  ^{:args [{:desc "Object to be encoded"}]
    :desc "Returns a JSON-encoding String for the given object."}
  [to-encode]
  #?(:clj
     (json/generate-string to-encode)
     :cljs
     (js/JSON.stringify (clj->js to-encode))))

(defn json-decode
  ^{:args [{:desc "JSON-encoded string to be decoded"}]
    :desc "Returns an object corresponding to the given JSON-encoded string."}
  [to-decode]
  #?(:clj
     (json/parse-string to-decode)
     :cljs
     (js->clj (js/JSON.parse to-decode))))

(def-excel-fn
  "JSONENCODE" json-encode
  "JSON.ENCODE" json-encode
  "JSONDECODE" json-decode
  "JSON.DECODE" json-decode)
