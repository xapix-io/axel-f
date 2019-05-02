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
  [d & [precision]]
  (let [precision (coerce/excel-number (or precision 0))
        d (coerce/excel-number d)]
    (let [factor (Math/pow 10 precision)
          res (/ (Math/round (* d factor)) factor)]
      (if (> precision 0)
        res
        (int res)))))

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
  [x]
  (add x 1))

(def INC #'INC*)

(defn DEC*
  "Decrement number by 1"
  [x]
  (sub x 1))

(def DEC #'DEC*)

(def env
  {"SUM" SUM
   "ROUND" ROUND
   "INC" INC
   "DEC" DEC})
