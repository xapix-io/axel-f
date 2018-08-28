(ns axel-f.functions.math
  (:require [axel-f.error :as error]
            [axel-f.functions.coercion :as coercion]))

(defn round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (if-let [d (coercion/excel-number d)]
     (if-let [precision (coercion/excel-number precision)]
       (let [factor (Math/pow 10 precision)
             res (/ (Math/round (* d factor)) factor)]
         (if (> precision 0)
           res
           (int res)))
       (throw (error/error "#VALUE!" (error/format-not-a-number-error "ROUND" 2 precision))))
     (throw (error/error "#VALUE!" (error/format-not-a-number-error "ROUND" 1 d))))))

(defn sum-fn [& items]
  (reduce #?(:clj +' :cljs +)
          (map (fn [n]
                 (if-let [n (coercion/excel-number n)]
                   n
                   (throw (error/error "#VALUE!" (error/format-not-a-number-error "SUM" nil n)))))
               (flatten items))))
