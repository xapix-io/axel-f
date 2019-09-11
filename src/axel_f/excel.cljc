(ns axel-f.excel
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]
            [axel-f.autocomplete :as autocomplete]
            [axel-f.excel.coerce :as coerce]
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
            [axel-f.excel.date :as date]
            [axel-f.excel.special-forms :as special-forms]
            [axel-f.excel.validate :as validate]
            [axel-f.excel.fn :as fn]
            [clojure.string :as string])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

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
   date/env
   special-forms/env
   coerce/env
   validate/env
   fn/env))

(def base-env-index
  (set (sequence
        (comp
         (map second))
        (autocomplete/flatten env))))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (try
     (let [ast (-> formula lexer/read parser/parse)
           f (compiler/compile special-forms/env ast)
           env (merge env extra-env)]
       (with-meta
         (fn fname
           ([] (fname nil))
           ([ctx]
            (let [res (f (assoc env :axel-f.runtime/context ctx))]
              (when-not (contains? base-env-index res)
                res))))
         (update (meta f) :free-variables distinct)))
     (catch #?(:clj ExceptionInfo
               :cljs js/Error) e
       (throw (ex-info (#?(:clj .getMessage
                           :cljs .-message) e)
                       (assoc (ex-data e)
                              ::formula formula)))))))

(defn suggestions
  ([incomplete-formula context] (suggestions incomplete-formula context nil))
  ([incomplete-formula context extra-env]
   (let [store (atom {})
         index (autocomplete/index (assoc (merge env extra-env) :axel-f.runtime/context context))
         var-cb (fn [var position]
                  (when (not-empty var)
                    (swap! store assoc :suggestions
                           (map (fn [[path desc]]
                                  (-> desc
                                      (assoc :position position
                                             :value (autocomplete/->string (last path)))
                                      (dissoc :distance)))
                                (autocomplete/search-index
                                 index
                                 (map (fn [x]
                                        (if (vector? x)
                                          (cond
                                            (number? (second x)) (second x)
                                            :else "*")
                                          x))
                                      var))))))
         fncall-cb (fn [fn-name current-arg]
                     (when-let [sug (and fn-name (first (autocomplete/search-index index fn-name)))]
                       (swap! store assoc :context
                              (-> (second sug)
                                  (assoc :value (string/join "." (first sug))
                                         :current-arg current-arg
                                         :type :FNCALL)
                                  (dissoc :distance)))))]
     (try
       (-> incomplete-formula
           lexer/read
           (parser/parse :var-cb var-cb :fncall-cb fncall-cb))
       @store
       (catch #?(:clj ExceptionInfo :cljs js/Error) _
         @store)))))
