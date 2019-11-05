(ns axel-f.excel.jwt
  (:require [axel-f.buddy.jws :as jws]
            [clojure.string :as string]
            #?(:clj [cheshire.core :as json]
               :cljs [axel-f.buddy.codecs.json :as json])
            [axel-f.buddy.codecs :as codecs])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn jwt-sign* [alg payload & opts]
  (let [payload (json/generate-string payload)]
    (cond
      (#{"HS256" "HS384" "HS512"} alg)
      (jws/sign payload (first opts) {:alg (keyword (string/lower-case alg))}))))

(def jwt-sign #'jwt-sign*)

(defn jwt-unsign* [alg payload & opts]
  (let [payload (codecs/to-string
                 (cond
                   (#{"HS256" "HS384" "HS512"} alg)
                   (jws/unsign payload (first opts) {:alg (keyword (string/lower-case alg))})))]
    (try
      (json/parse-string payload)
      (catch #?(:clj ExceptionInfo
                :cljs js/Error) _e
        (throw (ex-info "Message can not be parsed" {:message payload}))))))

(def jwt-unsign #'jwt-unsign*)

(def env
  {"JWT" {"SIGN" jwt-sign
          "UNSIGN" jwt-unsign}})
