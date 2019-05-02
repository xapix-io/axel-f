(ns axel-f.excel
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]
            [axel-f.autocomplete :as autocomplete]
            [axel-f.excel.operators :as operators]
            [axel-f.excel.collections :as collections]
            [axel-f.excel.base64 :as base64]
            [axel-f.excel.geo :as geo]
            [axel-f.excel.hash :as hash]
            [axel-f.excel.json :as json]
            [axel-f.excel.logic :as logic]
            [axel-f.excel.math :as math]
            [axel-f.excel.object :as object]
            [axel-f.excel.stat :as stat]
            [axel-f.excel.text :as text]
            [axel-f.excel.special-forms :as special-forms]))

(def env
  (merge
   operators/env
   collections/env
   base64/env
   geo/env
   hash/env
   json/env
   logic/env
   math/env
   object/env
   stat/env
   text/env
   special-forms/env))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (let [ast (-> formula lexer/read parser/parse)
         f (compiler/compile ast)
         env (merge env extra-env)
         fname (gensym)]
     (with-meta
       (fn fname
         ([] (fname nil))
         ([ctx]
          (f (assoc env :axel-f.runtime/context ctx))))
       (update (meta f) :free-variables distinct)))))

(defn suggest-fn
  ([context] (suggest-fn context nil))
  ([context extra-env]
   (let [env (merge env extra-env)]
     (autocomplete/suggest-fn env context))))

(defn ^:deprecated suggestions
  ([incomplete-formula] (suggestions incomplete-formula nil))
  ([incomplete-formula context]
   (let [f (suggest-fn context)]
     (f incomplete-formula))))
