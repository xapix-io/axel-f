(ns axel-f.core
  (:require [axel-f.v2.parser :as parser]
            [axel-f.v2.runtime :as runtime])
  (:refer-clojure :exclude [compile]))

(defn compile [formula & _]
  (parser/parse formula))

(defn run [formula & [context]]
  (let [ast (cond
              (string? formula)
              (compile formula)

              (map? formula)
              formula

              :otherwise
              (throw (ex-info (str "`formula` must be a string or precompiled string, got `" (type formula) "` instead.")
                              {:formula formula})))]
    ((runtime/eval ast) context)))
