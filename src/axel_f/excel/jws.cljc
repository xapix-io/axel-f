(ns axel-f.excel.jws
  (:require [axel-f.excel.base64 :refer [base64-encode-websafe base64-decode-websafe]]
            [clojure.string :as string]
            #?(:clj [buddy.sign.jws :as jws])
            #?@(:cljs [[goog.crypt :as crypt]
                       [goog.crypt.Hmac]
                       [goog.crypt.Sha256]
                       [goog.crypt.Sha512]
                       [goog.crypt.base64]])))

(defn- no-padding-websafe-encode [s]
  (string/replace
   (base64-encode-websafe s)
   #"\." ""))

(defn sign [sign-fn payload]
  ;; TODO prepare payload to been signed
  (let [payload (no-padding-websafe-encode payload)]
    (sign-fn payload)))

(defn unsign
  ""
  [input verify-fn]
  (let [[header payload signature] (string/split input #"\." 3)]
    (if (verify-fn (string/join "." [header payload])
                   (base64-decode-websafe signature))
      (base64-decode-websafe payload)
      (throw (ex-info "Message seems corrupt or manipulated." {:cause :signature})))))

(defn hmac-sign [alg key]
  (fn [payload]
    #?(:cljs
       (let [hmac (goog.crypt.Hmac.
                   (case alg
                     "HS256" (goog.crypt.Sha256.)
                     "HS512" (goog.crypt.Sha512.))
                   (crypt/stringToByteArray key))
             header (-> {"alg" alg}
                        clj->js
                        js/JSON.stringify
                        no-padding-websafe-encode)]
         (string/join "." [header
                           payload
                           (no-padding-websafe-encode
                            (crypt/byteArrayToString
                             (.getHmac hmac (string/join "." [header payload]))))]))
       :clj (jws/sign payload key {:alg (case alg
                                          "HS256" :hs256
                                          "HS512" :hs512)}))))
