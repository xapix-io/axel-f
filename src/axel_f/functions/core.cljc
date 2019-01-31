(ns axel-f.functions.core
  (:require [axel-f.functions.coercion :as coerce]
            [clojure.string :as string]))

(defonce ^:dynamic *functions-store* (atom {}))

(defn def-excel-fn* [fn-name fn-impl]
  (let [f (gensym)]
    (swap! *functions-store* assoc fn-name
           (with-meta fn-impl (meta fn-impl))))
  nil)

(defn def-excel-fn [fn-name fn-impl & fn-name-impls]
  (def-excel-fn* fn-name fn-impl)
  (if fn-name-impls
    (when (next fn-name-impls)
      (recur (first fn-name-impls) (second fn-name-impls) (nnext fn-name-impls)))))

(defn add
  ([x] (coerce/excel-number x))
  ([x y] (+ (coerce/excel-number x)
            (coerce/excel-number y))))

(defn sub
  ([x] (* -1 (coerce/excel-number x)))
  ([x y] (- (coerce/excel-number x)
            (coerce/excel-number y))))

(defn mult [x y]
  (* (coerce/excel-number x)
     (coerce/excel-number y)))

(defn div [x y]
  (/ (coerce/excel-number x)
     (coerce/excel-number y)))

(defn less [x y]
  (< (coerce/excel-number x)
     (coerce/excel-number y)))

(defn more [x y]
  (> (coerce/excel-number x)
     (coerce/excel-number y)))

(defn less-or-eq [x y]
  (<= (coerce/excel-number x)
      (coerce/excel-number y)))

(defn more-or-eq [x y]
  (>= (coerce/excel-number x)
      (coerce/excel-number y)))

(defn not-eq [x y]
  (not= x y))

(defn eq [x y]
  (= x y))

(defn concatenate [x y]
  (str (coerce/excel-str x)
       (coerce/excel-str y)))

(defn pow [x y]
  (Math/pow (coerce/excel-number x)
            (coerce/excel-number y)))

(defn negate [x]
  (not x))

(defn percent [x]
  (* 0.01 (coerce/excel-number x)))

(defn flexy-get [m k]
  (cond
    (string? k)
    (or (get m k)
        (get m (keyword k)))

    (keyword? k)
    (or (get m k)
        (get m (string/join "/" (filter identity ((juxt namespace name) k)))))))

(defn flexy-nth [m i]
  (if (and (sequential? m)
           (integer? i))
    (nth m i nil)
    (flexy-get m i)))

(def-excel-fn
  "+" add
  "-" sub
  "*" mult
  "/" div
  "<" less
  ">" more
  "<=" less-or-eq
  ">=" more-or-eq
  "<>" not-eq
  "=" eq
  "&" concatenate
  "^" pow
  "!" negate
  "%" percent
  "flexy-get" flexy-get
  "flexy-nth" flexy-nth)
