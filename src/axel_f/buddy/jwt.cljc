(ns axel-f.buddy.jwt
  (:require #?(:clj [cheshire.core :as json]
               :cljs [axel-f.buddy.codecs.json :as json])
            [axel-f.buddy.jws :as jws]
            [axel-f.buddy.codecs :as codecs]))

(defn sign [payload pkey & opts]
  (let [payload (json/generate-string payload)]
    (apply (partial jws/sign payload pkey) opts)))

(defn verify [input pkey & opts]
  (let [{:keys [error] :as res} (apply (partial jws/verify input pkey) opts)]
    (try
      (if error res (update res :payload json/parse-string))
      (catch #?(:clj Throwable
                :cljs js/Error) _error
        {:error {:type 3
                 :message "Payload can not be parsed as json"}}))))

(defn extract [input pkey & opts]
  (let [payload (codecs/to-string (apply (partial jws/extract input pkey) opts))]
    (try
      (json/parse-string payload)
      (catch #?(:clj Throwable
                :cljs js/Error) _error
        (throw (ex-info "Massage can not be parsed" {:type :not-a-json}))))))
