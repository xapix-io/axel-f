(ns axel-f.buddy.keys
  (:require [axel-f.buddy.pem :as pem])
  (:import java.io.StringReader))

(defn str->public-key
  "Public key constructor from string."
  [keydata]
  (with-open [reader (StringReader. ^String keydata)]
    (pem/read-pubkey reader)))

(defn str->private-key
  "Private key constructor from string."
  ([keydata]
   (str->private-key keydata nil))
  ([keydata passphrase]
   (with-open [reader (StringReader. ^String keydata)]
     (pem/read-privkey reader passphrase))))
