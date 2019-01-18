(ns axel-f.core
  (:require [axel-f.v2.parser :as parser])
  (:refer-clojure :exclude [compile]))

(defn compile [formula & custom-transforms]
  (parser/parse formula))

(defn run [formula & [context]]
  (let [formula-fn (cond
                     (string? formula)
                     (compile formula)

                     (fn? formula)
                     formula

                     :otherwise
                     (throw (ex-info (str "`formula` must be a string or precompiled string, got `" (type formula) "` instead.")
                                     {:formula formula})))]
    (formula-fn context)))
