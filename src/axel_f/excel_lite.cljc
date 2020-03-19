(ns axel-f.excel-lite
  (:refer-clojure :exclude [compile])
  (:require [axel-f.runtime :as runtime]
            [axel-f.excel.coerce :as coerce]
            [axel-f.excel.operators :as operators]
            [axel-f.excel.collections :as collections]
            [axel-f.excel.xpath :as xpath]
            [axel-f.excel.geo :as geo]
            [axel-f.excel.logic :as logic]
            [axel-f.excel.math :as math]
            [axel-f.excel.object :as object]
            [axel-f.excel.stat :as stat]
            [axel-f.excel.text :as text]
            [axel-f.excel.special-forms :as special-forms]
            [axel-f.excel.validate :as validate]
            [axel-f.excel.fn :as fn]))

(def base-env
  (merge
   operators/env
   xpath/env
   collections/env
   geo/env
   logic/env
   math/env
   object/env
   stat/env
   text/env
   special-forms/env
   coerce/env
   validate/env
   fn/env))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (runtime/compile formula base-env extra-env)))

(defn suggestions
  ([incomplete-formula context] (suggestions incomplete-formula context nil))
  ([incomplete-formula context extra-env]
   (runtime/suggestions incomplete-formula context base-env extra-env)))
