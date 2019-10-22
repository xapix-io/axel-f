(ns axel-f.excel.math
  (:require [axel-f.excel.coerce :as coerce]))

(defn add [x y]
  (+ (coerce/excel-number x)
     (coerce/excel-number y)))

(defn sub [x y]
  (- (coerce/excel-number x)
     (coerce/excel-number y)))

(defn ROUND*
  "Rounds a number to a certain number of decimal places according to standard rules."
  [^{:doc "The value to round to places number of places."} value
   & [^{:doc "The number of decimal places to which to round. (zero by default)"} places]]
  (let [places (coerce/excel-number (or places 0))
        value (coerce/excel-number value)
        factor (Math/pow 10 places)
        res (/ (Math/round (* value factor)) factor)]
    (if (> places 0)
      res
      (long res))))

(def ROUND #'ROUND*)

(defn sum-fn [& items]
  (reduce + (map coerce/excel-number (flatten items))))

(defn SUM*
  "Returns the sum of a series of numbers and/or references."
  [^{:doc "The first number or range to add together."} number
   & ^{:doc "Additional numbers or ranges to add to arg1."} numbers]
  (apply (partial sum-fn number) numbers))

(def SUM #'SUM*)

(defn INC*
  "Increment number by 1"
  [^{:doc "The value to increment by 1"} value]
  (add value 1))

(def INC #'INC*)

(defn DEC*
  "Decrement number by 1"
  [^{:doc "The value to decrement by 1"} value]
  (sub value 1))

(def DEC #'DEC*)

(def env
  {"SUM" SUM
   "ROUND" ROUND
   "INC" INC
   "DEC" DEC})
