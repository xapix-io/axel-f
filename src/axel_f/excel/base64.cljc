(ns axel-f.excel.base64
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

(defn base64-encode*
  "Creates a base-64 encoded ASCII string from a String"
  [^{:doc "String to encode"} to-encode]
  #?(:clj
     (.encodeToString (Base64/getEncoder) (.getBytes to-encode))
     :cljs
     (btoa to-encode)))

(def base64-encode #'base64-encode*)

(defn base64-decode*
  "Decodes a string of data which has been encoded using base-64 encoding"
  [^{:doc "String to decode"} to-decode]
  #?(:clj
     (String. (.decode (Base64/getDecoder) to-decode))
     :cljs
     (atob to-decode)))

(def base64-decode #'base64-decode*)

(def env
  {"BASE64ENCODE" base64-encode
   "BASE64DECODE" base64-decode
   "BASE64" {"ENCODE" base64-encode
             "DECODE" base64-decode}})
