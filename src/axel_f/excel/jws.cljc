(ns axel-f.excel.jws
  (:require [axel-f.buddy.jws :as jws]
            [clojure.string :as string]
            [axel-f.buddy.codecs :as codecs]))

(defn jws-sign*
  "Sign arbitrary payload as a string using one of supported algorythms: HS256, HS384, HS512"
  [alg payload & opts]
  (jws/sign payload (first opts) :alg (keyword (string/lower-case alg))))

(def jws-sign #'jws-sign*)

(defn jws-extract*
  "Extract payload as a string from Web Signature using one of supported algorythms: HS256, HS384, HS512"
  [alg payload & opts]
  (codecs/to-string (jws/extract payload (first opts) :alg (keyword (string/lower-case alg)))))

(def jws-extract #'jws-extract*)

(defn jws-verify*
  "Verify the signature of the payload and extract its content as a string"
  [alg payload & opts]
  (let [{:strs [error] :as res} (jws/verify payload (first opts) :alg (keyword (string/lower-case alg)))]
    (if error res (update res "payload" codecs/to-string))))

(def jws-verify #'jws-verify*)

(def env
  {"JWS" {"SIGN" jws-sign
          "EXTRACT" jws-extract
          "VERIFY" jws-verify}})
