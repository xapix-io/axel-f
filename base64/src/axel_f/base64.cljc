(ns axel-f.base64
  (:require [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn] :as m]
            #?(:cljs [goog.crypt.base64 :as b64]))
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

(def-excel-fn base64.encode
  "Creates a base-64 encoded ASCII string from a String"
  {:args [{:desc "String to encode"}]}
  [s]
  (base64-encode s))

(def-excel-fn base64encode
  "Creates a base-64 encoded ASCII string from a String"
  {:args [{:desc "String to encode"}]
   :deprecated true}
  [s]
  ((m/find-impl "BASE64.ENCODE") s))

(def-excel-fn base64.decode
  "Decodes a string of data which has been encoded using base-64 encoding"
  {:args [{:desc "String to decode"}]}
  [s]
  (base64-decode s))

(def-excel-fn base64decode
  "Decodes a string of data which has been encoded using base-64 encoding"
  {:args [{:desc "String to decode"}]
   :deprecated true}
  [s]
  ((m/find-impl "BASE64.DECODE") s))
