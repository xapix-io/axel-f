(ns axel-f.excel.jws
  (:require [axel-f.buddy.jws :as jws]
            [clojure.string :as string]
            [axel-f.buddy.codecs :as codecs]))

(defn jws-sign* [alg payload & opts]
  (jws/sign payload (first opts) :alg (keyword (string/lower-case alg))))

(def jws-sign #'jws-sign*)

(defn jws-extract* [alg payload & opts]
  (codecs/to-string (jws/extract payload (first opts) :alg (keyword (string/lower-case alg)))))

(def jws-extract #'jws-extract*)

(defn jws-verify* [alg payload & opts]
  (let [{:strs [error] :as res} (jws/verify payload (first opts) :alg (keyword (string/lower-case alg)))]
    (if error res (update res "payload" codecs/to-string))))

(def jws-verify #'jws-verify*)

(def env
  {"JWS" {"SIGN" jws-sign
          "EXTRACT" jws-extract
          "VERIFY" jws-verify}})
