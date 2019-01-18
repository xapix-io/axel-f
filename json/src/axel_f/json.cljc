(ns axel-f.json
  (:require [axel-f.functions.core :refer [def-excel-fn]]
            #?(:clj [cheshire.core :as json])))

(def json-encode
  ^{:args [{:desc "Object to be encoded"}]
    :desc "Returns a JSON-encoding String for the given object."}
  (fn [to-encode]
    #?(:clj
       (json/generate-string to-encode)
       :cljs
       (js/JSON.stringify (clj->js to-encode)))))

(def json-decode
  ^{:args [{:desc "JSON-encoded string to be decoded"}]
    :desc "Returns an object corresponding to the given JSON-encoded string."}
  (fn [to-decode]
    #?(:clj
       (json/parse-string to-decode)
       :cljs
       (js->clj (js/JSON.parse to-decode)))))

;; (def-excel-fn json.encode
;;   "Returns a JSON-encoding String for the given object."
;;   {:args [{:desc "Object to be encoded"}]}
;;   [obj]
;;   (json-encode obj))

;; (def-excel-fn jsonencode
;;   "Returns a JSON-encoding String for the given object."
;;   {:args [{:desc "Object to be encoded"}]
;;    :deprecated true}
;;   [obj]
;;   ((m/find-impl "JSON.ENCODE") obj))

;; (def-excel-fn json.decode
;;   "Returns an object corresponding to the given JSON-encoded string."
;;   {:args [{:desc "JSON-encoded string to be decoded"}]}
;;   [s]
;;   (json-decode s))

;; (def-excel-fn jsondecode
;;   "Returns an object corresponding to the given JSON-encoded string."
;;   {:args [{:desc "JSON-encoded string to be decoded"}]
;;    :deprecated true}
;;   [s]
;;   ((m/find-impl "JSON.DECODE") s))

(def-excel-fn
  "JSONENCODE" json-encode
  "JSON.ENCODE" json-encode
  "JSONDECODE" json-decode
  "JSON.DECODE" json-decode)
