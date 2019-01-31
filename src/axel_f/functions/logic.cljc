(ns axel-f.functions.logic
  (:require [axel-f.functions.core :refer [def-excel-fn]]))

(defn and*
  [& args]
  (every? identity args))

(def and*-meta
  {:desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."
   :args [{:desc "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false."}
          {:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."
           :opt true
           :repeatable true}]})

(defn not*
  [logical-expression]
  (not logical-expression))

(def not*-meta
  {:desc "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`."
   :args [{:desc "An expression or reference holding an expression that represents some logical value, i.e. TRUE or FALSE."}]})

(defn or*
  [& args]
  (boolean (some identity args)))

(def or*-meta
  {:desc "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false."
   :args [{:desc "An expression or reference to some logical value, i.e. TRUE or FALSE, or can be coerced to a logical value."}
          {:desc "More expressions that evaluate to logical values."
           :opt true
           :repeatable true}]})

(def-excel-fn
  "AND"
  and*
  and*-meta

  "OR"
  or*
  or*-meta

  "NOT"
  not*
  not*-meta)
