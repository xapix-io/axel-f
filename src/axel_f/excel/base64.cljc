(ns axel-f.excel.base64
  (:require [axel-f.buddy.codecs :as codecs]
            [axel-f.buddy.codecs.base64 :as base64]))

(defn base64-encode*
  "Creates a base-64 encoded ASCII string from a String"
  [^{:doc "String to encode"} to-encode]
  (codecs/to-string (base64/encode to-encode)))

(def base64-encode #'base64-encode*)

(defn base64-decode*
  "Decodes a string of data which has been encoded using base-64 encoding"
  [^{:doc "String to decode"} to-decode]
  (codecs/to-string (base64/decode to-decode)))

(def base64-decode #'base64-decode*)

(def env
  {"BASE64ENCODE" (with-meta base64-encode* (merge {:deprecated true} (meta #'base64-encode*)))
   "BASE64DECODE" (with-meta base64-decode* (merge {:deprecated true} (meta #'base64-decode*)))
   "BASE64" {"ENCODE" base64-encode
             "DECODE" base64-decode}})
