(ns axel-f.excel.coerce
  #?(:clj (:require [clojure.edn :as edn])))

(defn excel-number [maybe-number]
  (cond
    (number? maybe-number)
    maybe-number

    (string? maybe-number)
    (try
      (let [n (#?(:clj edn/read-string
                  :cljs js/parseFloat) maybe-number)]
        (if (and (number? n) #?(:cljs (not (js/isNaN n))))
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
