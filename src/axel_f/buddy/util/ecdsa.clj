(ns axel-f.buddy.util.ecdsa
  "ECDSA related DER encoding decoding helpers."
  (:import axel_f.buddy.util.ECDSA))

(defn transcode-to-der
  [^bytes data]
  (ECDSA/transcodeSignatureToDER data))

(defn transcode-to-concat
  [^bytes data length]
  (ECDSA/transcodeSignatureToConcat data ^int length))
