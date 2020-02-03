(ns axel-f.excel.coerce
  #?(:clj (:require [clojure.edn :as edn])))

(defn excel-type [x]
  (cond
    (number? x) :number
    (string? x) :string
    (boolean? x) :boolean
    (nil? x) :null
    :else (type x)))

(defmulti excel-number excel-type)

(defmethod excel-number :default [value]
  (throw (ex-info (str "Fail to coerce `" value "` to number.")
                  {:type :argument-type})))

(defmethod excel-number :number [n] n)

(defmethod excel-number :string [s]
  (try
    (let [n (when (not-empty s)
              (#?(:clj edn/read-string
                  :cljs js/Number) s))]
      (if (and (number? n) #?@(:cljs [(not (js/isNaN n)) (not= js/Infinity n) (not= (* -1 js/Infinity) n)]))
        n
        (throw (ex-info (str "Fail to coerce `" s "` to number.")
                        {:type :argument-type}))))
    (catch #?(:clj Throwable
              :cljs js/Error) _
      (throw (ex-info (str "Fail to coerce `" s "` to number.")
                      {:type :argument-type})))))

(defmethod excel-number :boolean [b]
  (if b 1 0))

(defmethod excel-number :null [_] 0)

(defn excel-str [item]
  (case item
    true "TRUE"
    false "FALSE"
    (str item)))

(defn to-string*
  "Tries to coerce given value to a string type. Returns null for empty value."
  [^{:doc "Any object to coerce to a string."} obj]
  (when obj (str obj)))

(def to-string #'to-string*)

(defn to-integer*
  "Tries to coerce given value to an integer type. Returns null for empty value or values not reducible to integer."
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
  "Tries to coerce given value to float type. Returns null for empty value or values not reducible to float."
  [^{:doc "Any object to coerce to a float."} obj]
  (try
    (cond
      (number? obj)
      (double obj)

      (string? obj)
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
  "Tries to coerce given value to a boolean type. Returns null for empty value or values not reducible to boolean."
  [^{:doc "Any object to coerce to a boolean type."} obj]
  (cond
    (string? obj)
    (case obj
      "true" true
      "false" false
      nil)

    (boolean? obj) obj))

(def to-boolean #'to-boolean*)

(def env
  {"coerce" {"to-string" to-string
             "to-integer" to-integer
             "to-float" to-float
             "to-boolean" to-boolean}})
