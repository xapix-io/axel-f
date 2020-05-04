(ns axel-f.buddy.codecs.base64
  "Util functions for make conversion between string, bytes
  and encode them to base64 hex format."
  (:require [axel-f.buddy.codecs :as codecs]
            #?(:cljs [goog.crypt.base64 :as base64]))
  #?(:clj (:import org.apache.commons.codec.binary.Base64)))

#?(:cljs
   (defn ^String trim-padding
     "Removes padding from the right side of base64-encoded string."
     [s]
     (loop [index (.-length s)]
       (if (zero? index)
         ""
         (if (= "." (.charAt s (unchecked-dec index)))
           (recur (unchecked-dec index))
           (subs s 0 index))))))

(defn encode
  "Encode data to byte array base64.
  Accepts String and byte array as argument."
  ([data]
   (encode data false))
  ([data urlsafe?]
   (let [data (codecs/to-bytes data)]
     #?(:clj (if urlsafe?
               (Base64/encodeBase64URLSafe ^bytes data)
               (Base64/encodeBase64 ^bytes data))
        :cljs (codecs/to-bytes
               (base64/encodeByteArray data (if urlsafe? base64/Alphabet.WEBSAFE_NO_PADDING base64/Alphabet.DEFAULT)))))))

(defn decode
  "Decode base64 data into byte array.
  Accepts String and byte array as input
  argument."
  [data]
  (let [data (#?(:clj codecs/to-bytes
                 :cljs codecs/to-string) data)]
    #?(:clj (Base64/decodeBase64 ^bytes data)
       :cljs (base64/decodeStringToByteArray data))))
