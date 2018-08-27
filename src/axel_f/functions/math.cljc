(ns axel-f.functions.math
  (:require [axel-f.error :as error]))

(defn round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (let [factor (Math/pow 10 precision)
         res (/ (Math/round (* d factor)) factor)]
     (if (> precision 0)
       res
       (int res)))))

(defn sum-fn [& items]
  (let [items (flatten items)]
    (if (every? number? items)
      (reduce #?(:clj +' :cljs +) items)
      (throw (error/error "#VALUE!" "Function SUM parameters expects number values.")))))
