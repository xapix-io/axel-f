(ns axel-f.runtime
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as string]
            [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]
            [axel-f.autocomplete :as autocomplete]
            [axel-f.excel.special-forms :as special-forms])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn compile
  ([formula env] (compile formula env nil))
  ([formula env extra-env]
   (let [base-env-index
         (set (sequence
               (comp
                (map second))
               (autocomplete/flatten env)))]
     (try
       (let [ast (-> formula lexer/read parser/parse)
             f (compiler/compile special-forms/env ast)
             env (merge env extra-env)]
         (with-meta
           (fn fname
             ([] (fname nil))
             ([ctx]
              (try
                (let [res (f (assoc env :axel-f.runtime/context ctx))]
                  (when-not (contains? base-env-index res)
                    res))
                (catch #?(:clj Exception
                          :cljs js/Error) e
                  (throw (ex-info (#?(:clj .getMessage :cljs .-message) e)
                                  (merge (ex-data e)
                                         {:axel-f.excel/formula formula
                                          :axel-f.excel/context ctx})
                                  e))))))
           (update (meta f) :free-variables distinct)))
       (catch #?(:clj ExceptionInfo
                 :cljs js/Error) e
         (throw (ex-info (#?(:clj .getMessage
                             :cljs .-message) e)
                         (assoc (ex-data e)
                                :axel-f.excel/formula formula))))))))

(defn suggestions
  ([incomplete-formula context env] (suggestions incomplete-formula context env nil))
  ([incomplete-formula context env extra-env]
   (let [store (atom {})
         index (autocomplete/index (assoc (merge env extra-env) :axel-f.runtime/context context))
         var-cb (fn [var position]
                  (when (not-empty var)
                    (swap! store assoc :suggestions
                           (map (fn [[path desc]]
                                  (-> desc
                                      (assoc :position position
                                             :to-replace (last var)
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
