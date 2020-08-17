(ns axel-f.excel.json
  (:require [axel-f.buddy.codecs.json :as json]))

(defn encode*
  "Returns a JSON-encoding String for the given object."
  [^{:doc "Object to encode"} to-encode]
  (json/generate-string to-encode))

(def encode #'encode*)

(defn decode*
  "Returns an object corresponding to the given JSON-encoded string."
  [^{:doc "JSON-encoded string to decode"} to-decode]
  (json/parse-string to-decode))

(def decode #'decode*)

(def env
  {"JSONENCODE" (with-meta encode* (merge {:deprecated true} (meta #'encode*)))
   "JSONDECODE" (with-meta decode* (merge {:deprecated true} (meta #'decode*)))
   "JSON" {"ENCODE" encode
           "DECODE" decode}})
