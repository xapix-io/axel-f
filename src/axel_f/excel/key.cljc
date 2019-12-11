(ns axel-f.excel.key
  (:require #?@(:cljs [[axel-f.buddy.hash :as hash]
                       [axel-f.buddy.codecs :as codecs]
                       [axel-f.buddy.codecs.base64 :as base64]])
            #?(:clj [axel-f.buddy.keys :as keys])))

(defn str->pubkey* [st]
  #?(:clj (keys/str->public-key st)
     :cljs (codecs/to-string (base64/encode (hash/sha256 st)))))

(def str->pubkey #'str->pubkey*)

(defn str->privkey* [st & [passphrase]]
  #?(:clj (keys/str->private-key st passphrase)
     :cljs (codecs/to-string (base64/encode (hash/sha256 (str st passphrase))))))

(def str->privkey #'str->privkey*)

(def env
  {"KEY" {"PUB" str->pubkey
          "PRIV" str->privkey}})
