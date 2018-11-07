(ns axel-f.functions.logic
  (:require [axel-f.macros #?(:clj :refer :cljs :refer-macros) [def-excel-fn]]))

(def-excel-fn and
  "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."
  [& args]
  (every? identity args))

(def-excel-fn not
  "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`."
  [logical-expression]
  (not logical-expression))

(def-excel-fn or
  "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false."
  [& args]
  (boolean (some identity args)))
