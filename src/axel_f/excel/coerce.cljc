(ns axel-f.excel.coerce
  #?(:clj (:require [clojure.edn :as edn])))

(defmulti inst (fn [[t & _args]] t))

(defn excel-type [x & _]
  (cond
    #?@(:clj [(ratio? x) :ratio])
    (number? x) :number
    (string? x) :string
    (boolean? x) :boolean
    (nil? x) :null
    (sequential? x) (first x)
    :else (type x)))

(defmulti excel-number excel-type)

(defmethod excel-number :default [value]
  (throw (ex-info (str "Fail to coerce `" value "` to number.")
                  {:type :argument-type})))

(defmethod excel-number :number [n] n)

#?(:clj
   (defmethod excel-number :ratio [r] r))

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

(defmulti excel-string excel-type)

(defmethod excel-string :default [x & _]
  (str x))

(defmethod excel-string :boolean [b & _]
  (if b "TRUE" "FALSE"))

(defmethod excel-string :null [_ & _]
  "NULL")

#?(:clj
   (defmethod excel-string :ratio [r & _]
     (str (double r))))

(defn to-string*
  "Tries to coerce given value to a string type. Returns null for empty value."
  [^{:doc "Any object to coerce to a string."} obj
   & ^{:doc "Optional arguments"} opts]
  (when obj (apply (partial excel-string obj) opts)))

(def to-string #'to-string*)

(defn to-integer*
  "Tries to coerce given value to an integer type. Returns null for empty value or values not reducible to integer."
  [^{:doc "Any object to coerce to an integer."} obj]
  (when obj
    (try
      (long (excel-number obj))
      (catch #?(:clj Throwable
                :cljs js/Error) _
        nil))))

(def to-integer #'to-integer*)

(defn to-float*
  "Tries to coerce given value to float type. Returns null for empty value or values not reducible to float."
  [^{:doc "Any object to coerce to a float."} obj]
  (when obj
    (try
      (double (excel-number obj))
      (catch #?(:clj Throwable
                :cljs js/Error) _
        nil))))

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
