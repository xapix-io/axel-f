(ns axel-f.excel.jwt
  (:require [axel-f.buddy.jwt :as jwt]
            [clojure.string :as string]))

(defn jwt-sign* [alg payload & opts]
  (jwt/sign payload (first opts) :alg (keyword (string/lower-case alg))))

(def jwt-sign #'jwt-sign*)

(defn jwt-extract* [alg payload & opts]
  (jwt/extract payload (first opts) :alg (keyword (string/lower-case alg))))

(def jwt-extract #'jwt-extract*)

(defn jwt-verify* [alg payload & opts]
  (jwt/verify payload (first opts) :alg (keyword (string/lower-case alg))))

(def jwt-verify #'jwt-verify*)

(def env
  {"JWT" {"SIGN" jwt-sign
          "EXTRACT" jwt-extract
          "VERIFY" jwt-verify}})
