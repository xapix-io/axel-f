(ns axel-f.core
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.runtime :as runtime]
            [axel-f.functions :as core]))

(defn compile [formula & _]
  (-> formula
      lexer/read-formula
      parser/parse
      runtime/eval))

(defn ^:deprecated run [formula & [context]]
  (let [f (cond
            (string? formula)
            (compile formula)

            (fn? formula)
            formula

            :otherwise
            (throw (ex-info "Formula must be a string or precompiled expression." {})))]
    (f context)))

(comment

  ((compile "(1+1)^SUM(1,2)"))

  )
