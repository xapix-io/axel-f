(ns axel-f.excel.operators
  (:require [axel-f.excel.coerce :as coerce]))

(defn add*
  ([x] (coerce/excel-number x))
  ([x y]
   (+ (coerce/excel-number x)
      (coerce/excel-number y))))

(def add #'add*)

(defn sub*
  ([x] (* -1 (coerce/excel-number x)))
  ([x y]
   (- (coerce/excel-number x)
      (coerce/excel-number y))))

(def sub #'sub*)

(defn mult* [x y]
  (* (coerce/excel-number x)
     (coerce/excel-number y)))

(def mult #'mult*)

(defn div* [x y]
  (/ (coerce/excel-number x)
     (coerce/excel-number y)))

(def div #'div*)

(defn less* [x y]
  (< (coerce/excel-number x)
     (coerce/excel-number y)))

(def less #'less*)

(defn more* [x y]
  (> (coerce/excel-number x)
     (coerce/excel-number y)))

(def more #'more*)

(defn less-or-eq* [x y]
  (<= (coerce/excel-number x)
      (coerce/excel-number y)))

(def less-or-eq #'less-or-eq*)

(defn more-or-eq* [x y]
  (>= (coerce/excel-number x)
      (coerce/excel-number y)))

(def more-or-eq #'more-or-eq*)

(defn not-eq* [x y]
  (not= x y))

(def not-eq #'not-eq*)

(defn eq* [x y]
  (= x y))

(def eq #'eq*)

(defn concatenate* [x y]
  (str (coerce/excel-str x)
       (coerce/excel-str y)))

(def concatenate #'concatenate*)

(defn pow* [x y]
  (Math/pow (coerce/excel-number x)
            (coerce/excel-number y)))

(def pow #'pow*)

(defn negate* [x]
  (not x))

(def negate #'negate*)

(defn percent* [x]
  (* 0.01 (coerce/excel-number x)))

(def percent #'percent*)

(defn range** [x y]
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
