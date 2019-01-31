(ns axel-f.functions.base64
  (:require [axel-f.functions.core :refer [def-excel-fn]])
  #?(:clj (:import java.util.Base64)))

#?(:cljs
   (cond
     (exists? js/atob)
     (def atob js/atob)

     (exists? js/Buffer)
     (defn atob [s]
       (.toString (js/Buffer.from s "base64") "binary"))))

#?(:cljs
   (cond
     (exists? js/btoa)
     (def btoa js/btoa)

     (exists? js/Buffer)
     (defn btoa [s]
       (.toString (js/Buffer.from s "binary") "base64"))))

(def base64-encode
  ^{:args [{:desc "String to encode"}]
    :desc "Creates a base-64 encoded ASCII string from a String"}
  (fn [to-encode]
    #?(:clj
       (.encodeToString (Base64/getEncoder) (.getBytes to-encode))
       :cljs
       (btoa to-encode))))

(def base64-decode
  ^{:args [{:desc "String to decode"}]
    :desc "Decodes a string of data which has been encoded using base-64 encoding"}
  (fn [to-decode]
    #?(:clj
       (String. (.decode (Base64/getDecoder) to-decode))
       :cljs
       (atob to-decode))))

(def-excel-fn
  "BASE64ENCODE" base64-encode
  "BASE64.ENCODE" base64-encode
  "BASE64DECODE" base64-decode
  "BASE64.DECODE" base64-decode)
