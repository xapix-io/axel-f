(ns axel-f.excel.hash
  (:require [axel-f.buddy.codecs :as codecs]
            [axel-f.buddy.hash :as hash]))

(defn sha256*
  "Calculates a sha-256 based digest from a String"
  [^{:doc "String to calculate sha256 digest"} msg]
  (codecs/bytes->hex (hash/sha256 msg)))

(def sha256 #'sha256*)

(defn sha384*
  "Calculates a sha-384 based digest from a String"
  [^{:doc "String to caclulate sha384 digest"} msg]
  (codecs/bytes->hex (hash/sha384 msg)))

(def sha384 sha384*)

(defn sha512*
  "Calculates a sha-512 based digest from a String"
  [^{:doc "String to caclulate sha512 digest"} msg]
  (codecs/bytes->hex (hash/sha512 msg)))

(def sha512 sha512*)

(defn sha1*
  "Calculates a sha-1 based digest from a String"
  [^{:doc "String to caclulate sha1 digest"} msg]
  (codecs/bytes->hex (hash/sha1 msg)))

(def sha1 sha1*)

(defn md5*
  "Calculates a md5 based digest from a String"
  [^{:doc "String to caclulate md5 digest"} msg]
  (codecs/bytes->hex (hash/md5 msg)))

(def md5 md5*)

(def env
  {"HASH" {"SHA256" sha256
           "SHA384" sha384
           "SHA512" sha512
           "SHA1" sha1
           "MD5" md5}})
