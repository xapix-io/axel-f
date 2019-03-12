(ns axel-f.functions.hash
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            #?@(:clj [[digest :refer [sha-256]]]
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

(defn sha256 [msg]
  #?(:clj (sha-256 msg)
     :cljs (bytes->hex (hash-bytes msg))))

(def-excel-fn
  "HASH.SHA256"
  sha256
  {:args [{:desc "String to calculate digest"}]
   :desc "Calculates a sha-256 based digest from a String"})
