(ns axel-f.excel.hash
  (:require #?@(:clj [[digest :as d]]
                :cljs [[goog.crypt :as crypt]
                       [goog.crypt.Sha256]
                       [goog.crypt.Sha224]
                       [goog.crypt.Sha384]
                       [goog.crypt.Sha512]
                       [goog.crypt.Sha1]
                       [goog.crypt.Md5]])))

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
   (defn hash-bytes [alg s]
     (digest
      (case alg
        :sha256 (goog.crypt.Sha256.)
        :sha224 (goog.crypt.Sha224.)
        :sha384 (goog.crypt.Sha384.)
        :sha512 (goog.crypt.Sha512.)
        :sha1 (goog.crypt.Sha1.)
        :md5 (goog.crypt.Md5.))
      (string->bytes s))))

(defn sha256*
  "Calculates a sha-256 based digest from a String"
  [^{:doc "String to calculate sha256 digest"} msg]
  #?(:clj (d/sha-256 msg)
     :cljs (bytes->hex (hash-bytes :sha256 msg))))

(def sha256 #'sha256*)

(defn sha224*
  "Calculates a sha-224 based digest from a String"
  [^{:doc "String to calculate sha224 digest"} msg]
  #?(:clj (d/sha-224 msg)
     :cljs (bytes->hex (hash-bytes :sha224 msg))))

(def sha224 #'sha224*)

(defn sha384*
  "Calculates a sha-384 based digest from a String"
  [^{:doc "String to caclulate sha384 digest"} msg]
  #?(:clj (d/sha-384 msg)
     :cljs (bytes->hex (hash-bytes :sha384 msg))))

(def sha384 sha384*)

(defn sha512*
  "Calculates a sha-512 based digest from a String"
  [^{:doc "String to caclulate sha512 digest"} msg]
  #?(:clj (d/sha-512 msg)
     :cljs (bytes->hex (hash-bytes :sha512 msg))))

(def sha512 sha512*)

(defn sha1*
  "Calculates a sha-1 based digest from a String"
  [^{:doc "String to caclulate sha1 digest"} msg]
  #?(:clj (d/sha-1 msg)
     :cljs (bytes->hex (hash-bytes :sha1 msg))))

(def sha1 sha1*)

(defn md5*
  "Calculates a md5 based digest from a String"
  [^{:doc "String to caclulate md5 digest"} msg]
  #?(:clj (d/md5 msg)
     :cljs (bytes->hex (hash-bytes :md5 msg))))

(def md5 md5*)

(def env
  {"HASH" {"SHA256" sha256
           "SHA224" sha224
           "SHA384" sha384
           "SHA512" sha512
           "SHA1" sha1
           "MD5" md5}})
