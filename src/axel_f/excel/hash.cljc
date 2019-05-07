(ns axel-f.excel.hash
  (:require #?@(:clj [[digest :refer [sha-256]]]
                :cljs [[goog.crypt :as crypt]
                       [goog.crypt.Sha256 :as Sha256]])))

#?(:cljs
   (defn string->bytes [s]
     (crypt/stringToUtf8ByteArray s)))

#?(:cljs
   (defn bytes->hex
     "convert bytes to hex"
     [bytes-in]
     (crypt/byteArrayToHex bytes-in)))

#?(:cljs
   (defn digest [hasher bytes]
     (.update hasher bytes)
     (.digest hasher)))

#?(:cljs
   (defn hash-bytes [s]
     (digest
      (goog.crypt.Sha256.)
      (string->bytes s))))

(defn sha256*
  "Calculates a sha-256 based digest from a String"
  [^{:doc "String to calculate sha256 digest"} msg]
  #?(:clj (sha-256 msg)
     :cljs (bytes->hex (hash-bytes msg))))

(def sha256 #'sha256*)

(def env
  {"HASH" {"SHA256" sha256}})
