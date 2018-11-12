(ns axel-f.macros
  (:require [clojure.string :as string]))

(def ^:dynamic  functions-store (atom {}))

(defmulti find-impl identity)

(defmethod find-impl :default [unknown-function]
  (throw (ex-info (str "Unknown function " unknown-function) {})))

(defmacro def-excel-fn [& s]
  (let [fn-sym (gensym)
        fn-name (-> s first str string/upper-case)]
    `(do
       (defn- ~fn-sym ~@(rest s))
       (swap! functions-store assoc ~fn-name (select-keys (meta #'~fn-sym) [:doc :args]))
       (defmethod find-impl ~fn-name [~'_]
         (with-meta ~fn-sym
           (merge (meta #'~fn-sym)
                  {:name ~fn-name}))))))
