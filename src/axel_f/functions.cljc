(ns axel-f.functions
  (:require [axel-f.functions.core :refer [*functions-store*]]
            axel-f.functions.math
            axel-f.functions.text
            axel-f.functions.stat
            axel-f.functions.logic
            axel-f.functions.geo
            axel-f.functions.json
            axel-f.functions.base64
            axel-f.functions.hash)
  #?(:clj (:import clojure.lang.ExceptionInfo)))

(defn find-impl [fname]
  (when-let [f (:impl (get @*functions-store* fname))]
    (fn [& args]
      (try
        (apply f args)
        (catch ExceptionInfo e
          (throw (ex-info "Error in function call"
                          {:cause {:message (#?(:clj .getMessage
                                                :cljs .-message) e)
                                   :data (ex-data e)}})))))))

(defn find-meta [fname]
  (:meta (get @*functions-store* fname)))
