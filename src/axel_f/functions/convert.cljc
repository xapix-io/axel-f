(ns axel-f.functions.convert
  (:require [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn]]
            #?(:clj [cheshire.core :as json]
               :cljs [goog.crypt.base64 :as b64]))
  #?(:clj (:import java.util.Base64)))

(defn base64-encode [to-encode]
  #?(:clj
     (.encodeToString (Base64/getEncoder) (.getBytes to-encode))
     :cljs
     (b64/encodeString to-encode)))

(defn base64-decode [to-decode]
  #?(:clj
     (String. (.decode (Base64/getDecoder) to-decode))
     :cljs
     (b64/decodeString to-decode)))

(defn json-encode [to-encode]
  #?(:clj
     (json/generate-string to-encode)
     :cljs
     (js/JSON.stringify (clj->js to-encode))))

(defn json-decode [to-decode]
  #?(:clj
     (json/parse-string to-decode)
     :cljs
     (js->clj (js/JSON.parse to-decode))))

(def-excel-fn base64encode
  "Creates a base-64 encoded ASCII string from a String"
  {:args [{:desc "String to encode"}]}
  [s]
  (base64-encode s))

(def-excel-fn base64decode
  "Decodes a string of data which has been encoded using base-64 encoding"
  {:args [{:desc "String to decode"}]}
  [s]
  (base64-decode s))

(def-excel-fn jsonencode
  "Returns a JSON-encoding String for the given object."
  {:args [{:desc "Object to be encoded"}]}
  [obj]
  (json-encode obj))

(def-excel-fn jsondecode
  "Returns an object corresponding to the given JSON-encoded string."
  {:args [{:desc "JSON-encoded string to be decoded"}]}
  [s]
  (json-decode s))
