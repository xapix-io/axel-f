(ns axel-f.buddy.mac
  "Message Authentication Code algorithms."
  (:refer-clojure :exclude [hash reset! #?@(:cljs [-reset -hash])])
  (:require [axel-f.buddy.codecs :as codecs]
            [axel-f.buddy.hash :as h]
            [axel-f.buddy.bytes :as bytes]
            #?(:cljs [goog.crypt.Hmac]))
  #?(:clj (:import org.bouncycastle.crypto.Mac
                   org.bouncycastle.crypto.params.KeyParameter
                   org.bouncycastle.crypto.macs.HMac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IMac
  (-hash [_ engine] "Generate the auth message code")
  (-verify [_ sig engine] "Verify the auth message code"))

(defprotocol IEngineInit
  (-init [_ options] "Initialize the mac"))

(defprotocol IEngine
  (-reset [_] "Reset engine state")
  (-update [_ input offset length] "Update the engine state.")
  (-end [_] "Generates the mac"))

(defmulti ^:no-doc engine
  "A engine constructor."
  :alg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (extend-type Mac
     IEngineInit
     (-init [it options]
       (let [key (:key options)
             keyparam (KeyParameter. (codecs/to-bytes key))]
         (.init it keyparam)))

     IEngine
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input offset length))
     (-end [it]
       (let [buffer (byte-array (.getMacSize it))]
              (.doFinal it buffer 0)
              buffer))))

#?(:cljs
   (extend-type goog.crypt.Hmac
     IEngineInit
     (-init [it options])

     IEngine
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input))
     (-end [it]
       (let [buffer (.digest it)]
         (.reset it)
         buffer))))

(defmethod engine :hmac
  [options]
  (let [digest (h/resolve-digest-engine
                (:digest options :sha256))]
    (assert digest "Invalid digest engine.")
    #?(:clj (HMac. digest)
       :cljs (goog.crypt.Hmac. digest (codecs/to-bytes (:key options))))))

(defmethod engine :hmac+sha256
  [options]
  (let [digest (h/resolve-digest-engine :sha256)]
    #?(:clj (HMac. digest)
       :cljs (goog.crypt.Hmac. digest (codecs/str->bytes (:key options))))))

(defmethod engine :hmac+sha384
  [options]
  (let [digest (h/resolve-digest-engine :sha384)]
    #?(:clj (HMac. digest)
       :cljs (goog.crypt.Hmac. digest (codecs/to-bytes (:key options))))))

(defmethod engine :hmac+sha512
  [options]
  (let [digest (h/resolve-digest-engine :sha512)]
    #?(:clj (HMac. digest)
       :cljs (goog.crypt.Hmac. digest (codecs/to-bytes (:key options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation details for different data types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hash-plain-data
  [input engine]
  (-reset engine)
  (-update engine input 0 (count input))
  (-end engine))

(defn- verify-plain-data
  [input signature engine]
  (let [sig (hash-plain-data input engine)]
    (bytes/equals? sig signature)))

(extend-protocol IMac
  #?(:clj (Class/forName "[B")
     :cljs array)
  (-hash [input engine]
    (hash-plain-data input engine))
  (-verify [input signature engine]
    (verify-plain-data input signature engine))

  #?(:clj java.lang.String
     :cljs string)
  (-hash [input engine]
    (hash-plain-data (codecs/to-bytes input) engine))
  (-verify [input signature engine]
    (verify-plain-data (codecs/to-bytes input) signature engine)))

(defn hash
  "Generate hmac digest for arbitrary
  input data, a secret key and hash algorithm.
  If algorithm is not supplied, sha256
  will be used as default value."
  [input engine-or-options]
  (if (satisfies? IEngine engine-or-options)
    (-hash input engine-or-options)
    (let [engine (engine engine-or-options)]
      (-init engine engine-or-options)
      (-hash input engine))))

(defn verify
  "Verify hmac for artbitrary input and signature."
  [input signature engine-or-options]
  (let [signature (codecs/to-bytes signature)]
    (if (satisfies? IEngine engine-or-options)
      (-verify input signature engine-or-options)
      (let [engine (engine engine-or-options)]
        (-init engine engine-or-options)
        (-verify input signature engine)))))
