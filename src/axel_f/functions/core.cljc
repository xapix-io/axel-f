(ns axel-f.functions.core
  (:require [axel-f.functions.coercion :as coerce]
            [clojure.string :as string])
  #?(:clj (:import clojure.lang.ExceptionInfo)))

(defonce ^:dynamic *functions-store* (atom {}))

(defn def-excel-fn* [fn-name fn-impl fn-meta]
  (swap! *functions-store* assoc fn-name
         {:meta fn-meta
          :impl fn-impl}))

(defn def-excel-fn [& fn-name-impls]
  (loop [[fn-name fn-impl fn-meta & fn-name-impls] fn-name-impls]
    (def-excel-fn* fn-name fn-impl fn-meta)
    (when (not-empty fn-name-impls)
      (recur fn-name-impls))))

(defn add
  ([x] (coerce/excel-number x))
  ([x y]
   (+ (coerce/excel-number x)
      (coerce/excel-number y))))

(defn sub
  ([x] (* -1 (coerce/excel-number x)))
  ([x y]
   (- (coerce/excel-number x)
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
  "+" add nil
  "-" sub nil
  "*" mult nil
  "/" div nil
  "<" less nil
  ">" more nil
  "<=" less-or-eq nil
  ">=" more-or-eq nil
  "<>" not-eq nil
  "=" eq nil
  "&" concatenate nil
  "^" pow nil
  "!" negate nil
  "%" percent nil
  "flexy-get" flexy-get nil
  "flexy-nth" flexy-nth nil)
