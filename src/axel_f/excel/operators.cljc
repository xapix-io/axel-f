(ns axel-f.excel.operators
  (:require [axel-f.excel.coerce :as coerce]))

(defn add*
  "Returns the sum of given nums."
  [^{:doc "Number or string that can be coerced to number."} x
   & [^{:doc "Number or string that can be coerced to number."} y]]
  (let [x (coerce/excel-number x)]
    (if (some? y)
      (+ x (coerce/excel-number y))
      x)))

(def add #'add*)

(defn sub*
  "If no ys are supplied, returns the negation of x, else subtracts the y from x and returns the result."
  [^{:doc "Number or string that can be coerced to number."} x
   & [^{:doc "Number or string that can be coerced to number."} y]]
  (if (some? y)
    (- (coerce/excel-number x)
       (coerce/excel-number y))
    (* -1 (coerce/excel-number x))))

(def sub #'sub*)

(defn mult*
  "Returns the product of nums."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (* (coerce/excel-number x)
     (coerce/excel-number y)))

(def mult #'mult*)

(defn div*
  "Returns numerator divided by the denominator"
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (/ (coerce/excel-number x)
     (coerce/excel-number y)))

(def div #'div*)

(defn less*
  "Returns non-nil if nums are in monotonically increasing order, otherwise false."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (< (coerce/excel-number x)
     (coerce/excel-number y)))

(def less #'less*)

(defn more*
  "Returns non-nil if nums are in monotonically decreasing order, otherwise false."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (> (coerce/excel-number x)
     (coerce/excel-number y)))

(def more #'more*)

(defn less-or-eq*
  "Returns non-nil if nums are in monotonically non-decreasing order, otherwise false."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (<= (coerce/excel-number x)
      (coerce/excel-number y)))

(def less-or-eq #'less-or-eq*)

(defn more-or-eq*
  "Returns non-nil if nums are in monotonically non-increasing order, otherwise false."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (>= (coerce/excel-number x)
      (coerce/excel-number y)))

(def more-or-eq #'more-or-eq*)

(defn not-eq*
  "Returns true if object are not equal, otherwise false."
  [^{:doc "Object to check."} x
   ^{:doc "Object to check."} y]
  (not= x y))

(def not-eq #'not-eq*)

(defn eq*
  "Returns true if objects are equal, otherwise false."
  [^{:doc "Object to check."} x
   ^{:doc "Object to check."} y]
  (= x y))

(def eq #'eq*)

(defn concatenate*
  "Concatenate two strings."
  [^{:doc "String or number that can be coerced to string."} x
   ^{:doc "String or number that can be coerced to string."} y]
  (str (coerce/excel-string x)
       (coerce/excel-string y)))

(def concatenate #'concatenate*)

(defn pow*
  "Returns the value of the first argument raised to the power of the second argument."
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (Math/pow (coerce/excel-number x)
            (coerce/excel-number y)))

(def pow #'pow*)

(defn negate*
  "Returns true if x is logical false, false otherwise."
  [^{:doc "Any object."} x]
  (not x))

(def negate #'negate*)

(defn percent*
  "Calculate product of given number and 0.01"
  [^{:doc "Number or string that can be coerced to number."} x]
  (* 0.01 (coerce/excel-number x)))

(def percent #'percent*)

(defn range**
  "Returns a collection of integers starting from x and end at y (exclusive)"
  [^{:doc "Number or string that can be coerced to number."} x
   ^{:doc "Number or string that can be coerced to number."} y]
  (range (coerce/excel-number x)
         (coerce/excel-number y)))

(def range* #'range**)

(def env
  {":"  range*
   "+"  add
   "-"  sub
   "*"  mult
   "/"  div
   "<"  less
   ">"  more
   "<=" less-or-eq
   ">=" more-or-eq
   "<>" not-eq
   "="  eq
   "&"  concatenate
   "^"  pow
   "!"  negate
   "%"  percent})
