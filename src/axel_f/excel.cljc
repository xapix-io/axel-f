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
            [axel-f.excel.special-forms :as special-forms]
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
   special-forms/env))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (let [ast (-> formula lexer/read parser/parse)
         f (compiler/compile special-forms/env ast)
         env (merge env extra-env)
         fname (gensym)]
     (with-meta
       (fn fname
         ([] (fname nil))
         ([ctx]
          (f (assoc env :axel-f.runtime/context ctx))))
       (update (meta f) :free-variables distinct)))))

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
       (do
         (-> incomplete-formula
             lexer/read
             (parser/parse :var-cb var-cb :fncall-cb fncall-cb))
         @store)
       (catch #?(:clj ExceptionInfo :cljs js/Error) e
         @store)))))
