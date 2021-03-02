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
   (deftype StubEngine [alg]
     IEngine
     (-init [_it {:keys [_key _verify]}])
     (-update [_it _input _offset _length])
     (-end [_it signature?]
       (if signature?
         (do
           (js/console.warn (str "Signing for algorithm " (name alg) " is not yet implemented and will return fixed string"))
           "signing+not+yet+implemented")
         (do
           (js/console.warn (str "Verifying for algorithm " (name alg) " is not yet implemented and will pass"))
           true)))))

(def ^:no-doc
  +algorithms+
  {:ecdsa+sha256         #?(:clj #(Signature/getInstance "SHA256withECDSA" "BC")
                            :cljs #(StubEngine. :ecdsa+sha256))
   :ecdsa+sha384         #?(:clj #(Signature/getInstance "SHA384withECDSA" "BC")
                            :cljs #(StubEngine. :ecdsa+sha384))
   :ecdsa+sha512         #?(:clj #(Signature/getInstance "SHA512withECDSA" "BC")
                            :cljs #(StubEngine. :ecdsa+sha512))
   :rsassa-pss+sha256    #?(:clj #(Signature/getInstance "SHA256withRSAandMGF1" "BC")
                            :cljs #(StubEngine. :rsassa-pss+sha256))
   :rsassa-pss+sha384    #?(:clj #(Signature/getInstance "SHA384withRSAandMGF1" "BC")
                            :cljs #(StubEngine. :rsassa-pss+sha384))
   :rsassa-pss+sha512    #?(:clj #(Signature/getInstance "SHA512withRSAandMGF1" "BC")
                            :cljs #(StubEngine. :rsassa-pss+sha512))
   :rsassa-pkcs15+sha256 #?(:clj #(Signature/getInstance "SHA256withRSA" "BC")
                            :cljs #(StubEngine. :rsassa-pkcs15+sha256))
   :rsassa-pkcs15+sha384 #?(:clj #(Signature/getInstance "SHA384withRSA" "BC")
                            :cljs #(StubEngine. :rsassa-pkcs15+sha384))
   :rsassa-pkcs15+sha512 #?(:clj #(Signature/getInstance "SHA512withRSA" "BC")
                            :cljs #(StubEngine. :rsassa-pkcs15+sha512))})

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
