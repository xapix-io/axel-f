(ns axel-f.buddy.jws
  (:require [axel-f.buddy.codecs.base64 :as b64]
            [axel-f.buddy.codecs :as codecs]
            [clojure.string :as string]
            #?@(:clj [[buddy.core.mac :as mac]
                      [cheshire.core :as json]])
            #?@(:cljs [[goog.crypt :as crypt]
                       [goog.crypt.Hmac]
                       [goog.crypt.Sha256]
                       [goog.crypt.Sha384]
                       [goog.crypt.Sha512]
                       [goog.crypt.base64]
                       [axel-f.buddy.codecs.json :as json]])))

#?(:cljs
   (defmulti hasher identity))

#?(:cljs
   (defmethod hasher :sha256 [_]
     (goog.crypt.Sha256.)))

#?(:cljs
   (defmethod hasher :sha384 [_]
     (goog.crypt.Sha384.)))

#?(:cljs
   (defmethod hasher :sha512 [_]
     (goog.crypt.Sha512.)))

#?(:cljs
   (defn- sign-fn [hasher]
     (fn [authdata key]
       (let [hmac (goog.crypt.Hmac. hasher (crypt/stringToByteArray key))]
         (.getHmac hmac authdata)))))

#?(:cljs
   (defmulti mac-hash (fn [_authdata {:keys [alg]}] alg)))

#?(:cljs
   (defmethod mac-hash :hmac+sha256 [authdata {:keys [key]}]
     (let [hasher (hasher :sha256)]
       ((sign-fn hasher) authdata key))))

#?(:cljs
   (defmethod mac-hash :hmac+sha384 [authdata {:keys [key]}]
     (let [hasher (hasher :sha384)]
       ((sign-fn hasher) authdata key))))

#?(:cljs
   (defmethod mac-hash :hmac+sha512 [authdata {:keys [key]}]
     (let [hasher (hasher :sha512)]
       ((sign-fn hasher) authdata key))))

#?(:cljs
   (defn equals?
     "Test whether two sequences of characters or bytes are equal in a way that
      protects against timing attacks. Note that this does not prevent an attacker
      from discovering the *length* of the data being compared."
     [a b]
     (let [a (map int a), b (map int b)]
       (if (and a b (= (count a) (count b)))
         (zero? (reduce bit-or 0 (map bit-xor a b)))
         false))))

#?(:cljs
   (defn- verify-fn [hasher]
     (fn [input signature key]
       (let [hmac (goog.crypt.Hmac. hasher (crypt/stringToByteArray key))
             input (codecs/to-bytes input)]
         (.reset hmac)
         (.update hmac (codecs/to-bytes input) (count input))
         (equals? (.digest hmac) signature)))))

#?(:cljs
   (defmulti mac-verify (fn [_ _ {:keys [alg]}] alg)))

#?(:cljs
   (defmethod mac-verify :hmac+sha256 [input signature {:keys [key]}]
     (let [hasher (hasher :sha256)]
       ((verify-fn hasher) input signature key))))

#?(:cljs
   (defmethod mac-verify :hmac+sha384 [input signature {:keys [key]}]
     (let [hasher (hasher :sha384)]
       ((verify-fn hasher) input signature key))))

#?(:cljs
   (defmethod mac-verify :hmac+sha512 [input signature {:keys [key]}]
     (let [hasher (hasher :sha512)]
       ((verify-fn hasher) input signature key))))

(def +signers-map+
  "Supported algorithms"
  {:hs256 {:signer   #(#?(:clj mac/hash :cljs mac-hash) %1 {:alg :hmac+sha256 :key %2})
           :verifier #(#?(:clj mac/verify :cljs mac-verify) %1 %2 {:alg :hmac+sha256 :key %3})}
   :hs384 {:signer   #(#?(:clj mac/hash :cljs mac-hash) %1 {:alg :hmac+sha384 :key %2})
           :verifier #(#?(:clj mac/verify :cljs mac-verify) %1 %2 {:alg :hmac+sha384 :key %3})}
   :hs512 {:signer   #(#?(:clj mac/hash :cljs mac-hash) %1 {:alg :hmac+sha512 :key %2})
           :verifier #(#?(:clj mac/verify :cljs mac-verify) %1 %2 {:alg :hmac+sha512 :key %3})}})

;; --- Implementation

(defn- encode-header
  [header]
  (-> header
      (update :alg #(if (= % :eddsa) "EdDSA" (string/upper-case (name %))))
      (json/generate-string)
      (b64/encode true)
      (codecs/bytes->str)))

(defn- parse-header
  [^String data]
  (try
    (let [header (-> (b64/decode data)
                     (codecs/bytes->str)
                     (json/parse-string true))]
      (when-not (map? header)
        (throw (ex-info "Message seems corrupt or manipulated."
                        {:type :validation :cause :header})))
      (update header :alg #(keyword (string/lower-case %))))
    (catch #?(:clj com.fasterxml.jackson.core.JsonParseException
              :cljs js/Error) _error
      (throw (ex-info "Message seems corrupt or manipulated."
                      {:type :validation :cause :header})))))

(defn- encode-payload
  [input]
  (-> (b64/encode input true)
      (codecs/bytes->str)))

(defn- decode-payload
  [payload]
  (b64/decode payload))

(defn- calculate-signature
  "Given the bunch of bytes, a private key and algorithm,
  return a calculated signature as byte array."
  [{:keys [key alg header payload]}]
  (let [signer (get-in +signers-map+ [alg :signer])
        authdata (string/join "." [header payload])]
    (-> (signer authdata key)
        (b64/encode true)
        (codecs/bytes->str))))

(defn- verify-signature
  "Given a bunch of bytes, a previously generated
  signature, the private key and algorithm, return
  signature matches or not."
  [{:keys [alg signature key header payload]}]
  (let [verifier (get-in +signers-map+ [alg :verifier])
        authdata (string/join "." [header payload])
        signature (b64/decode signature)]
    (verifier authdata signature key)))

(defn- split-jws-message
  [message]
  (string/split message #"\." 3))

;; --- Public Api

(defn decode-header
  "Given a message, decode the header.
  WARNING: This does not perform any signature validation."
  [input]
  (let [[header] (split-jws-message input)]
    (parse-header header)))

(defn sign
  "Sign arbitrary length string/byte array using
  json web token/signature."
  [payload pkey & [{:keys [alg header] :or {alg :hs256}}]]
  {:pre [payload]}
  (let [header (-> (merge {:alg alg} header)
                   (encode-header))
        payload (encode-payload payload)
        signature (calculate-signature {:key pkey
                                        :alg alg
                                        :header header
                                        :payload payload})]
    (string/join "." [header payload signature])))

(defn unsign
  "Given a signed message, verify it and return
  the decoded payload."
  ([input pkey] (unsign input pkey nil))
  ([input pkey {:keys [alg] :or {alg :hs256}}]
   (let [[header payload signature] (split-jws-message input)
         ;; header-data (parse-header header)
         ]
     (when-not
       (try
         (verify-signature {:key       pkey #_(util/resolve-key pkey header-data)
                            :signature signature
                            :alg       alg
                            :header    header
                            :payload   payload})
         (catch #?(:clj java.security.SignatureException
                   :cljs js/Error) se
           (throw (ex-info "Message seems corrupt or manipulated."
                           {:type :validation :cause :signature}
                           se))))
       (throw (ex-info "Message seems corrupt or manipulated."
                       {:type :validation :cause :signature})))
     (decode-payload payload))))
