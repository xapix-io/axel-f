(ns axel-f.buddy.pem
  "PEM reading implementation."
  (:require [clojure.java.io :as io])
  (:import org.bouncycastle.openssl.PEMParser
           org.bouncycastle.openssl.PEMEncryptedKeyPair
           org.bouncycastle.openssl.PEMKeyPair
           org.bouncycastle.openssl.jcajce.JcePEMDecryptorProviderBuilder
           org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
           org.bouncycastle.openssl.jcajce.JceOpenSSLPKCS8DecryptorProviderBuilder
           org.bouncycastle.asn1.pkcs.PrivateKeyInfo
           org.bouncycastle.pkcs.PKCS8EncryptedPrivateKeyInfo
           org.bouncycastle.cert.X509CertificateHolder
           java.security.Security))

(when (nil? (Security/getProvider "BC"))
  (Security/addProvider (org.bouncycastle.jce.provider.BouncyCastleProvider.)))

(defn- decryptor
  [builder passphrase]
  (when (nil? passphrase)
    (throw (ex-info "Passphrase is mandatory with encrypted keys." {})))
  (.build builder (.toCharArray passphrase)))

(defn read-privkey
  [path-or-reader ^String passphrase]
  (with-open [reader (io/reader path-or-reader)]
    (let [parser    (PEMParser. reader)
          obj       (.readObject parser)
          converter (doto (JcaPEMKeyConverter.)
                      (.setProvider "BC"))]
      (cond
        (instance? PEMEncryptedKeyPair obj)
        (->> (.decryptKeyPair obj (decryptor (JcePEMDecryptorProviderBuilder.) passphrase))
             (.getKeyPair converter)
             (.getPrivate))
        (instance? PEMKeyPair obj)
        (->> (.getKeyPair converter obj)
             (.getPrivate))
        (instance? PKCS8EncryptedPrivateKeyInfo obj)
        (->> (.decryptPrivateKeyInfo obj (decryptor (JceOpenSSLPKCS8DecryptorProviderBuilder.) passphrase))
             (.getPrivateKey converter))
        (instance? PrivateKeyInfo obj)
        (.getPrivateKey converter obj)
        :else
        (throw (ex-info "Unknown PEM object type" {:kind (class obj)}))))))

(defn read-pubkey
  [path-or-reader]
  (with-open [reader (io/reader path-or-reader)]
    (let [parser    (PEMParser. reader)
          keyinfo   (.readObject parser)
          converter (doto (JcaPEMKeyConverter.)
                      (.setProvider "BC"))]
      (if (instance? X509CertificateHolder keyinfo)
        (.getPublicKey converter (.getSubjectPublicKeyInfo keyinfo))
        (.getPublicKey converter keyinfo)))))
