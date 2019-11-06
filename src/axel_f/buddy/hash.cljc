(ns axel-f.buddy.hash
  "Basic crypto primitives that used for more high
  level abstractions."
  (:refer-clojure :exclude [update reset! #?(:cljs -reset)])
  (:require [axel-f.buddy.codecs :as codecs]
            #?@(:clj [[clojure.java.io :as io]]
                :cljs [[goog.crypt.Sha256]
                       [goog.crypt.Sha224]
                       [goog.crypt.Sha384]
                       [goog.crypt.Sha512]
                       [goog.crypt.Sha1]
                       [goog.crypt.Md5]]))
  #?(:clj (:import org.bouncycastle.crypto.Digest
                   org.bouncycastle.crypto.digests.SHA1Digest
                   org.bouncycastle.crypto.digests.MD5Digest
                   org.bouncycastle.crypto.digests.SHA256Digest
                   org.bouncycastle.crypto.digests.SHA384Digest
                   org.bouncycastle.crypto.digests.SHA512Digest)))

(def ^:no-doc ^:static
  +digest-engines+
  {:sha256   #?(:clj #(SHA256Digest.)
                :cljs #(goog.crypt.Sha256.))
   :sha384   #?(:clj #(SHA384Digest.)
                :cljs #(goog.crypt.Sha384.))
   :sha512   #?(:clj #(SHA512Digest.)
                :cljs #(goog.crypt.Sha512.))
   :sha1     #?(:clj #(SHA1Digest.)
                :cljs #(goog.crypt.Sha1.))
   :md5      #?(:clj #(MD5Digest.)
                :cljs #(goog.crypt.Md5.))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol definitions (abstractions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IDigest
  (-digest [input engine] "Low level interface, always returns bytes"))

(defprotocol IEngine
  "Mac engine common interface definition."
  (-reset [_] "Reset the hash engine to its initial state.")
  (-update [_ input offset length] "Update bytes in a current instance.")
  (-end [_] "Return the computed mac and reset the engine."))

#?(:clj
   (extend-protocol IEngine
     Digest
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input offset length))
     (-end [it]
       (let [buffer (byte-array (.getDigestSize it))]
         (.doFinal it buffer 0)
         buffer))))

#?(:cljs
   (extend-protocol IEngine
     goog.crypt.Sha1
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input))
     (-end [it]
       (let [buffer (.digest it)]
         (.reset it)
         buffer))

     goog.crypt.Sha2
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input))
     (-end [it]
       (let [buffer (.digest it)]
         (.reset it)
         buffer))

     goog.crypt.Sha2_64bit
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input))
     (-end [it]
       (let [buffer (.digest it)]
         (.reset it)
         buffer))

     goog.crypt.Md5
     (-reset [it]
       (.reset it))
     (-update [it input offset length]
       (.update it input))
     (-end [it]
       (let [buffer (.digest it)]
         (.reset it)
         buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset!
  [engine]
  (-reset engine))

(defn update!
  ([engine input]
   (-update engine input 0 (count input)))
  ([engine input offset]
   (-update engine input offset (count input)))
  ([engine input offset length]
   (-update engine input offset length)))

(defn end!
  [engine]
  (-end engine))

(defn resolve-digest-engine
  "Helper function for make Digest instances
  from algorithm parameter."
  {:doc false}
  [engine]
  (cond
   (keyword? engine)
   (when-let [factory (get +digest-engines+ engine)]
     (factory))
   (instance? #?(:clj Digest :cljs goog.crypt.Sha2) engine) engine
   (fn? engine) (engine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation details for different data types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hash-plain-data
  [input engine]
  (-reset engine)
  (-update engine input 0 (count input))
  (-end engine))

(extend-protocol IDigest
  #?(:clj (Class/forName "[B")
     :cljs array)
  (-digest [^bytes input engine]
    (hash-plain-data input engine))

  #?(:clj String
     :cljs string)
  (-digest [^String input engine]
    (hash-plain-data (codecs/str->bytes input) engine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level public api.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn digest
  "Generic function for create cryptographic hash."
  [input alg-or-engine]
  (let [engine (resolve-digest-engine alg-or-engine)]
    (-digest input engine)))

(defn sha256
  [input]
  (digest input :sha256))

(defn sha384
  [input]
  (digest input :sha384))

(defn sha512
  [input]
  (digest input :sha512))

(defn sha1
  [input]
  (digest input :sha1))

(defn md5
  [input]
  (digest input :md5))
