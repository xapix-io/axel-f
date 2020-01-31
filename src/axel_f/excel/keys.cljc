(ns axel-f.excel.keys
  (:require #?@(:cljs [[axel-f.buddy.hash :as hash]
                       [axel-f.buddy.codecs :as codecs]
                       [axel-f.buddy.codecs.base64 :as base64]])
            #?(:clj [axel-f.buddy.keys :as keys])
            [clojure.string :as string]))

(def ^:private pem-parsers
  "(CERTIFICATE REQUEST|NEW CERTIFICATE REQUEST|CERTIFICATE|TRUSTED CERTIFICATE|X509 CERTIFICATE|X509 CRL|PKCS7|CMS|ATTRIBUTE CERTIFICATE|EC PARAMETERS|PUBLIC KEY|RSA PUBLIC KEY|RSA PRIVATE KEY|DSA PRIVATE KEY|EC PRIVATE KEY|ENCRYPTED PRIVATE KEY|PRIVATE KEY)")

(def ^:private x-509-regex
  (re-pattern (str "^-{5}BEGIN " pem-parsers "-{5}\\n(?:[A-Za-z0-9+/]{4}\\n?)*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?\\n-{5}END \\1-{5}$")))

(defn x-509-format? [st]
  (or (re-matches x-509-regex st)
      (throw (ex-info "Key must be X.509 encoded" {}))))

(defn str->pubkey* [st]
  (let [st (string/trim st)]
    (and (x-509-format? st)
         (try
           #?(:clj (keys/str->public-key st)
              :cljs (codecs/to-string (base64/encode (hash/sha256 st))))
           (catch #?(:clj Throwable
                     :cljs js/Error) _
             (throw (ex-info "Can not extract valid public key from given string." {})))))))

(def str->pubkey #'str->pubkey*)

(defn str->privkey* [st & [passphrase]]
  (let [st (string/trim st)]
    (and (x-509-format? st)
         (try
           #?(:clj (keys/str->private-key st passphrase)
              :cljs (codecs/to-string (base64/encode (hash/sha256 (str st passphrase)))))
           (catch #?(:clj Throwable
                     :cljs js/Error) _
             (throw (ex-info "Can not extract valid private key from given string and passphrase." {})))))))

(def str->privkey #'str->privkey*)

(def env
  {"KEY" {"PUB" str->pubkey
          "PRIV" str->privkey}})
