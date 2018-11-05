(ns axel-f.functions.logic
  (:require [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn]]))

(def-excel-fn and [& args]
  (every? identity args))

(def-excel-fn not [logical-expression]
  (not logical-expression))

(def-excel-fn or [& args]
  (boolean (some identity args)))
