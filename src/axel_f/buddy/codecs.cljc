(ns axel-f.buddy.codecs
  #?(:cljs (:require [goog.crypt :as crypt])
     :clj (:import org.apache.commons.codec.binary.Hex)))

(defn str->bytes
  "Convert string to byte array."
  ([^String s]
   (str->bytes s "UTF-8"))
  ([^String s, ^String encoding]
   #?(:clj (.getBytes s encoding)
      :cljs (if (= encoding "UTF-8")
              (crypt/stringToUtf8ByteArray s)
              (crypt/stringToByteArray s)))))

(defn bytes->str
  "Convert byte array to String."
  ([^bytes data]
   (bytes->str data "UTF-8"))
  ([^bytes data, ^String encoding]
   #?(:clj (String. data encoding)
      :cljs (if (= encoding "UTF-8")
              (crypt/utf8ByteArrayToString data)
              (crypt/byteArrayToString data)))))

(defn bytes->hex
  "Convert a byte array to hex encoded string."
  [^bytes data]
  (#?(:clj Hex/encodeHexString :cljs crypt/byteArrayToHex) data))

(defn hex->bytes
  "Convert hexadecimal encoded string to bytes array."
  [^String data]
  (#?(:clj (Hex/decodeHex (.toCharArray data))
      :cljs (crypt/hexToByteArray data))))

(defprotocol IByteArray
  "Facility for convert input parameters
  to bytes array with default implementation
  for string an bytes array itself."
  (-to-bytes [this] "Represent this as byte array."))

(defprotocol IString
  (-to-string [this]))

(defn to-bytes
  "Encode as bytes."
  [v]
  (-to-bytes v))

(defn to-string [v]
  (-to-string v))

(extend-protocol IByteArray
  #?(:clj (Class/forName "[B")
     :cljs array)
  (-to-bytes [it] it)

  nil
  (-to-bytes [_]
    #?(:clj (byte-array 0)
       :cljs #js []))

  #?(:clj String
     :cljs string)
  (-to-bytes [data] (str->bytes data)))

(extend-protocol IString
  #?(:clj (Class/forName "[B")
     :cljs array)
  (-to-string [data] (bytes->str data))

  #?(:clj String
     :cljs string)
  (-to-string [it] it))
