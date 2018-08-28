(ns axel-f.functions.math
  (:require [axel-f.error :as error]))

(defn- excel-number [maybe-number]
  (cond
    (number? maybe-number)
    maybe-number

    (string? maybe-number)
    (try
      (let [n (#?(:clj read-string
                  :cljs js/parseFloat) maybe-number)]
        (when (and (number? n) #?(:cljs (not (js/isNaN n))))
          n))
      (catch Throwable e
        nil))

    (boolean? maybe-number)
    (if maybe-number 1 0)

    :otherwise nil))

(defn- format-not-a-number-error [fnname arg-position value]
  (str "Function " fnname " parameter "
       arg-position
       (when arg-position " ")
       "expects number values. But '" value "' is a text and cannot be coerced to a number."))

(defn round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (if-let [d (excel-number d)]
     (if-let [precision (excel-number precision)]
       (let [factor (Math/pow 10 precision)
             res (/ (Math/round (* d factor)) factor)]
         (if (> precision 0)
           res
           (int res)))
       (throw (error/error "#VALUE!" (format-not-a-number-error "ROUND" 2 precision))))
     (throw (error/error "#VALUE!" (format-not-a-number-error "ROUND" 1 d))))))

(defn sum-fn [& items]
  (reduce #?(:clj +' :cljs +)
          (map (fn [n]
                 (if-let [n (excel-number n)]
                   n
                   (throw (error/error "#VALUE!" (format-not-a-number-error "SUM" nil n)))))
               (flatten items))))
