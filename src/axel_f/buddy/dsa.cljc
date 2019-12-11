(ns axel-f.buddy.dsa
  "Digital Signature Algorithms."
  (:refer-clojure :exclude [resolve])
  (:require [axel-f.buddy.codecs :as codecs])
  #?(:clj (:import java.security.Signature
                   java.security.Security)))

#?(:clj
   (when (nil? (Security/getProvider "BC"))
     (Security/addProvider (org.bouncycastle.jce.provider.BouncyCastleProvider.))))

(defprotocol IEngine
  (-init [_ options] "Initialize signature.")
  (-update [_ input offset length] "Update signature state.")
  (-end [_ signature?] "Returns the computed signature."))

(defprotocol ISignature
  "Default inteterface for all signature
  algorithms supported by `buddy`."
  (-sign [input engine] "Make signature.")
  (-verify [input signature engine] "Verify signature."))

#?(:cljs
   (deftype ECDSASignature [alg ^:mutable data ^:mutable public-key ^:mutable private-key]
     IEngine
     (-init [it {:keys [key verify]}]
       (if verify
         (set! (.-public-key it)
               (js/crypto.subtle.importKey "spki" key alg false ["verify"]))
         (set! (.-private-key it)
               (js/crypto.subtle.importKey "pkcs8" key alg false ["sign"]))))
     (-update [it input _offset _length]
       (set! (.-data it) input))
     (-end [it signature?]
       (if signature?
         (js/crypto.subtle.verify alg public-key signature? data)
         (js/crypto.subtle.sign alg private-key data)))))

(def ^:no-doc
  +algorithms+
  {:ecdsa+sha256 #?(:clj #(Signature/getInstance "SHA256withECDSA" "BC")
                    :cljs #(ECDSASignature. #js {"name" "ECDSA"
                                                 "hash" {"name" "SHA-256"}
                                                 "namedCurve" "P-256"}
                                            nil nil nil))
   :ecdsa+sha384 #?(:clj #(Signature/getInstance "SHA384withECDSA" "BC")
                    :cljs #(ECDSASignature. #js {"name" "ECDSA"
                                                 "hash" {"name" "SHA-384"}
                                                 "namedCurve" "P-384"}
                                            nil nil nil))
   :ecdsa+sha512 #?(:clj #(Signature/getInstance "SHA512withECDSA" "BC")
                    :cljs #(ECDSASignature. #js {"name" "ECDSA"
                                                 "hash" {"name" "SHA-512"}
                                                 "namedCurve" "P-512"}
                                            nil nil nil))})

(defn- resolve
  "Given dynamic type engine, try resolve it to
  valid engine instance. By default accepts keywords
  and functions."
  [signer]
  (cond
    (keyword? signer)
    (when-let [factory (get +algorithms+ signer)]
      (factory))
    (fn? signer)
    (signer)))

#?(:clj
   (extend-protocol IEngine
     Signature
     (-init [^Signature it options]
       (let [verify? (:verify options false)
             key (:key options)]
         (if verify?
           (.initVerify it key)
           (let [prng (or (:prng options)
                          (java.security.SecureRandom.))]
             (.initSign it key prng)))))

     (-update [it input offset length]
       (.update it input offset length))

     (-end [it signature?]
       (if signature?
         (.verify it signature?)
         (.sign it)))))

(defn- make-signature-for-plain-data
  [^bytes input engine]
  (-update engine input 0 (count input))
  (-end engine nil))

(defn- verify-signature-for-plain-data
  [^bytes input, ^bytes signature, engine]
  (-update engine input 0 (count input))
  (-end engine signature))

(extend-protocol ISignature
  #?(:clj (Class/forName "[B")
     :cljs array)
  (-sign [^bytes input engine]
    (make-signature-for-plain-data input engine))
  (-verify [^bytes input, ^bytes signature, engine]
    (verify-signature-for-plain-data input signature engine))

  #?(:clj java.lang.String
     :cljs string)
  (-sign [^String input, engine]
    (make-signature-for-plain-data (codecs/to-bytes input) engine))
  (-verify [^String input, ^bytes signature, engine]
    (verify-signature-for-plain-data (codecs/to-bytes input) signature engine)))

(defn sign
  [input {:keys [alg key prng]}]
  (let [engine (resolve alg)]
    (-init engine {:verify false :key key :prng prng})
    (-sign input engine)))

(defn verify
  [input signature {:keys [key alg]}]
  (let [engine (resolve alg)]
    (-init engine {:verify true :key key})
    (-verify input signature engine)))
