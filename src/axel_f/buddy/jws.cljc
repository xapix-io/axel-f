(ns axel-f.buddy.jws
  (:require [axel-f.buddy.codecs.base64 :as b64]
            [axel-f.buddy.codecs.json :as json]
            [axel-f.buddy.codecs :as codecs]
            [axel-f.buddy.mac :as mac]
            [axel-f.buddy.dsa :as dsa]
            #?(:clj [axel-f.buddy.util.ecdsa :refer [transcode-to-der transcode-to-concat]])
            [clojure.string :as string]))

(def +signers-map+
  "Supported algorithms"
  {:hs256 {:signer   #(mac/hash %1 {:alg :hmac+sha256 :key %2})
           :verifier #(mac/verify %1 %2 {:alg :hmac+sha256 :key %3})}
   :hs384 {:signer   #(mac/hash %1 {:alg :hmac+sha384 :key %2})
           :verifier #(mac/verify %1 %2 {:alg :hmac+sha384 :key %3})}
   :hs512 {:signer   #(mac/hash %1 {:alg :hmac+sha512 :key %2})
           :verifier #(mac/verify %1 %2 {:alg :hmac+sha512 :key %3})}

   ;; ECDSA with signature conversions
   :es256 {:signer   #?(:clj #(-> (dsa/sign %1 {:alg :ecdsa+sha256 :key %2}) (transcode-to-concat 64))
                        :cljs (fn [& _args] "signing+not+yet+implemented"))
           :verifier #?(:clj #(dsa/verify %1 (transcode-to-der %2) {:alg :ecdsa+sha256 :key %3})
                        :cljs (fn [& _args] true))}
   :es384 {:signer   #?(:clj #(-> (dsa/sign %1 {:alg :ecdsa+sha384 :key %2}) (transcode-to-concat 96))
                        :cljs (fn [& _args] "signing+not+yet+implemented"))
           :verifier #?(:clj #(dsa/verify %1 (transcode-to-der %2) {:alg :ecdsa+sha384 :key %3})
                        :cljs (fn [& _args] true))}
   :es512 {:signer   #?(:clj #(-> (dsa/sign %1 {:alg :ecdsa+sha512 :key %2}) (transcode-to-concat 132))
                        :cljs (fn [& _args] "signing+not+yet+implemented"))
           :verifier #?(:clj #(dsa/verify %1 (transcode-to-der %2) {:alg :ecdsa+sha512 :key %3})
                        :cljs (fn [& _args] true))}

   :rs256 {:signer   #(dsa/sign %1 {:alg :rsassa-pkcs15+sha256 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pkcs15+sha256 :key %3})}
   :rs384 {:signer   #(dsa/sign %1 {:alg :rsassa-pkcs15+sha384 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pkcs15+sha384 :key %3})}
   :rs512 {:signer   #(dsa/sign %1 {:alg :rsassa-pkcs15+sha512 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pkcs15+sha512 :key %3})}
   :ps256 {:signer   #(dsa/sign %1 {:alg :rsassa-pss+sha256 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pss+sha256 :key %3})}
   :ps384 {:signer   #(dsa/sign %1 {:alg :rsassa-pss+sha384 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pss+sha384 :key %3})}
   :ps512 {:signer   #(dsa/sign %1 {:alg :rsassa-pss+sha512 :key %2})
           :verifier #(dsa/verify %1 %2 {:alg :rsassa-pss+sha512 :key %3})}})

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
  [payload pkey & {:keys [alg header] :or {alg :hs256}}]
  {:pre [payload]}
  (let [header (-> (merge {:alg alg} header)
                   (encode-header))
        payload (encode-payload payload)
        signature (calculate-signature {:key pkey
                                        :alg alg
                                        :header header
                                        :payload payload})]
    (string/join "." [header payload signature])))

(defn verify
  [input pkey & {:keys [alg] :or {alg :hs256}}]
  (let [[header payload signature] (split-jws-message input)
        header-data (parse-header header)]
    (cond
      (not= alg (:alg header-data))
      {"error" {"type" 0
                "message" "Algorithm mismatch"}}
      (not (verify-signature {:key pkey
                              :signature signature
                              :alg alg
                              :header header
                              :payload payload}))
      {"error" {"type" 1
                "message" "Message seems corrupt or modified"}}

      ;; TODO check exp

      :else
      {"payload" (decode-payload payload)})))

(defn extract
  "Given a signed message, verify it and return
  the decoded payload."
  [input pkey & {:keys [alg] :or {alg :hs256}}]
  (let [[header payload signature] (split-jws-message input)]
    (when-not
        (try
          (verify-signature {:key       pkey
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
    (decode-payload payload)))
