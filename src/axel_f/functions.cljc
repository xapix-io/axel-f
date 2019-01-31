(ns axel-f.functions
  (:require [axel-f.functions.core :refer [*functions-store*]]
            axel-f.functions.math
            axel-f.functions.text
            axel-f.functions.stat
            axel-f.functions.logic
            axel-f.functions.geo
            axel-f.functions.json
            axel-f.functions.base64))

(defn find-impl [fname]
  (:impl (get @*functions-store* fname)))

(defn find-meta [fname]
  (:meta (get @*functions-store* fname)))
