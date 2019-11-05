(ns axel-f.excel.jws
  (:require [axel-f.buddy.jws :as jws]
            [clojure.string :as string]))

(defn jws-sign* [alg payload & opts]
  (cond
    (#{"HS256" "HS384" "HS512"} alg)
    (jws/sign payload (first opts) {:alg (keyword (string/lower-case alg))})))

(def jws-sign #'jws-sign*)

(defn jws-unsign* [alg payload & opts]
  (cond
    (#{"HS256" "HS384" "HS512"} alg)
    (jws/unsign payload (first opts) {:alg (keyword (string/lower-case alg))})))

(def jws-unsign #'jws-unsign*)

(def env
  {"JWS" {"SIGN" jws-sign
          "UNSIGN" jws-unsign}})
