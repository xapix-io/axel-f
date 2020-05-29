(ns axel-f.excel
  (:refer-clojure :exclude [compile])
  (:require [axel-f.excel.base64 :as base64]
            [axel-f.excel.hash :as hash]
            [axel-f.excel.json :as json]
            [axel-f.excel.search :as search]
            [axel-f.excel.date :as date]
            [axel-f.excel.jws :as jws]
            [axel-f.excel.jwt :as jwt]
            [axel-f.excel.keys :as keys]
            [axel-f.excel-lite :as excel-lite]
            [axel-f.runtime :as runtime]))

(def base-env
  (merge-with merge
              excel-lite/base-env
              base64/env
              hash/env
              json/env
              search/env
              date/env
              jws/env
              jwt/env
              keys/env))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (runtime/compile formula base-env extra-env)))

(defn suggestions
  ([incomplete-formula context] (suggestions incomplete-formula context nil))
  ([incomplete-formula context extra-env]
   (runtime/suggestions incomplete-formula context base-env extra-env)))
