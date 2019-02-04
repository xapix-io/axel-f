(ns axel-f.core
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.runtime :as runtime]
            [axel-f.analyzer :as analyzer]
            [axel-f.functions :as core]))

(defn- str->ast [formula]
  (-> formula
      lexer/read-formula
      parser/parse))

(defn compile [formula & _]
  (let [ast (str->ast formula)]
    (runtime/eval ast)))

(defn analyze [formula]
  (let [ast (str->ast formula)]
    (analyzer/report ast)))

(defn ^:deprecated run [formula & [context]]
  (let [f (cond
            (string? formula)
            (compile formula)

            (fn? formula)
            formula

            :otherwise
            (throw (ex-info "Formula must be a string or precompiled expression." {})))]
    (f context)))
