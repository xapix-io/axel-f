(ns axel-f.excel.coerce
  #?(:clj (:require [clojure.edn :as edn])))

(defn excel-number [maybe-number]
  (cond
    (number? maybe-number)
    maybe-number

    (string? maybe-number)
    (try
      (let [n (when (not-empty maybe-number)
                (#?(:clj edn/read-string
                    :cljs js/Number) maybe-number))]
        (if (and (number? n) #?@(:cljs [(not (js/isNaN n)) (not= js/Infinity n) (not= (* -1 js/Infinity) n)]))
          n
          (throw (ex-info (str "Fail to coerce `" maybe-number "` to number.")
                          {:type :argument-type}))))
      (catch #?(:clj Throwable
                :cljs js/Error) e
        (throw (ex-info (str "Fail to coerce `" maybe-number "` to number.")
                        {:type :argument-type}))))

    (boolean? maybe-number)
    (if maybe-number 1 0)))

(defn excel-str [item]
  (case item
    true "TRUE"
    false "FALSE"
    (str item)))

(defn to-string*
  "Try to coerce given value to a string type. Returns null for empty value."
  [^{:doc "Any object to coerce to a string."} obj]
  (when obj (str obj)))

(def to-string #'to-string*)

(defn to-integer*
  "Try to coerce given value to an integer type. Returns null for empty or not reducible to an integer type value."
  [^{:doc "Any object to coerce to an integer."} obj]
  (try
    (if (number? obj)
      (long obj)
      #?(:clj (Long/parseLong obj)
         :cljs (let [n (when (not-empty obj)
                         (js/Number obj))]
                 (if (or (js/isNaN n) (= js/Infinity n) (= (* -1 js/Infinity) n) (not= 0 (rem n 1)))
                   nil
                   n))))
    (catch #?(:clj java.lang.NumberFormatException
              :cljs js/Error) _
      nil)))

(def to-integer #'to-integer*)

(defn to-float*
  "Try to coerce given value to float type. Returns null for empty or not reducible to a float type value."
  [^{:doc "Any object to coerce to a float."} obj]
  (try
    (if (float? obj)
      (double obj)
      #?(:clj (Double/parseDouble obj)
         :cljs (let [n (when (not-empty obj)
                         (js/Number obj))]
                 (if (or (js/isNaN n) (= js/Infinity n) (= (* -1 js/Infinity) n) (= 0 (rem n 1)))
                   nil
                   n))))
    (catch #?(:clj java.lang.NumberFormatException
              :cljs js/Error) _
      nil)))

(def to-float #'to-float*)

(defn to-boolean*
  "Try to coerce given value to a boolean type. Returns null for empty or not reducible to a boolean type value."
  [^{:doc "Any object to coerce to a boolean type."} obj]
  (case obj
    "true" true
    "false" false
    nil))

(def to-boolean #'to-boolean*)

(def env
  {"coerce" {"to-string" to-string
             "to-integer" to-integer
             "to-float" to-float
             "to-boolean" to-boolean}})
