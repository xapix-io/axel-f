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

(defn range* [x y]
  (range x y))

(defn flexy-get [m k]
  (cond
    (string? k)
    (or (get m k)
        (get m (keyword k)))

    (keyword? k)
    (or (get m k)
        (get m (string/join "/" (filter identity ((juxt namespace name) k)))))))

(defn flexy-nth [m i]
  (cond
    (and (sequential? m)
           (integer? i))
    (nth m i nil)

    (and (sequential? m)
         (sequential? i))
    (if (empty? i)
      [(first m)]
      (subvec (vec m) (first i) (inc (last i))))

    :otherwise
    (flexy-get m i)))

(def-excel-fn
  ":" range* nil
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
  "flexy-nth" flexy-nth nil
  "MAP" nil {:args [{:desc "Partialy defined formula with free variables to apply to the collection"}
                    {:desc "Collection of elements"}]
             :desc "Applies partiualy defined formula to every element in a collection and returns an array."}
  "FILTER" nil {:args [{:desc "Condition which will be applied to members of collection"}
                       {:desc "Collection of elements"}]
                :desc "Returns an array of elements that have been filtered based on a condition."}
  "SORT" nil {:args [{:desc "Sorting function that will be applied to each element of the collection"}
                     {:desc "Collection of elements"}]
              :desc "Sorts a collection by the values returned from applying a sorting function to each element in said collection."}
  "IF" nil {}
  "IFS" nil {})
