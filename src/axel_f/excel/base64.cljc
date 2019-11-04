(ns axel-f.excel.base64
  #?(:clj (:import java.util.Base64)
     :cljs (:require [goog.crypt.base64])))

(defn base64-encode*
  "Creates a base-64 encoded ASCII string from a String"
  [^{:doc "String to encode"} to-encode]
  #?(:clj
     (.encodeToString (Base64/getEncoder) (.getBytes to-encode))
     :cljs
     (goog.crypt.base64/encodeString to-encode)))

(defn base64-encode-websafe*
  "Creates a url-safe base-64 encoded ASCII string from a String"
  [^{:doc "String to encode"} to-encode]
  #?(:clj
     (.encodeToString (Base64/getUrlEncoder) (.getBytes to-encode))
     :cljs
     (goog.crypt.base64/encodeString to-encode true)))

(def base64-encode #'base64-encode*)

(def base64-encode-websafe #'base64-encode-websafe*)

(defn base64-decode*
  "Decodes a string of data which has been encoded using base-64 encoding"
  [^{:doc "String to decode"} to-decode]
  #?(:clj
     (String. (.decode (Base64/getDecoder) to-decode))
     :cljs
     (goog.crypt.base64/decodeString to-decode)))

(defn base64-decode-websafe*
  "Decodes a string of data which has been encoded using base-64 url-safe encoding"
  [^{:doc "String to decode"} to-decode]
  #?(:clj
     (String. (.decode (Base64/getUrlDecoder) to-decode))
     :cljs
     (goog.crypt.base64/decodeString to-decode true)))

(def base64-decode #'base64-decode*)

(def base64-decode-websafe #'base64-decode-websafe*)

(def env
  {"BASE64ENCODE" (with-meta base64-encode* (merge {:deprecated true} (meta #'base64-encode*)))
   "BASE64DECODE" (with-meta base64-decode* (merge {:deprecated true} (meta #'base64-decode*)))
   "BASE64" {"ENCODE" base64-encode
             "DECODE" base64-decode
             "URLENCODE" base64-encode-websafe
             "URLDECODE" base64-decode-websafe}})
