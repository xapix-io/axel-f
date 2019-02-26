(ns axel-f.functions.json
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            #?(:clj [cheshire.core :as json])))

(defn json-encode
  [to-encode]
  #?(:clj
     (json/generate-string to-encode)
     :cljs
     (js/JSON.stringify (clj->js to-encode))))

(def json-encode-meta
  {:args [{:desc "Object to be encoded"}]
   :desc "Returns a JSON-encoding String for the given object."})

(defn json-decode
  [to-decode]
  #?(:clj
     (json/parse-string to-decode)
     :cljs
     (js->clj (js/JSON.parse to-decode))))

(def json-decode-meta
  {:args [{:desc "JSON-encoded string to be decoded"}]
   :desc "Returns an object corresponding to the given JSON-encoded string."})

(def-excel-fn
  "JSONENCODE"
  json-encode
  json-encode-meta

  "JSON.ENCODE"
  json-encode
  json-encode-meta

  "JSONDECODE"
  json-decode
  json-decode-meta

  "JSON.DECODE"
  json-decode
  json-decode-meta)
