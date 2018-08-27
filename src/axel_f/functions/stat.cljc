(ns axel-f.functions.stat
  (:require [axel-f.functions.math :as math]
            [axel-f.error :as error]))

(defn- average-fn [& items]
  (let [items (filter number? (mapcat identity items))
        len (count items)]
    (when-not (zero? len)
      (/ (apply math/sum-fn items)
         len))))

(defn count-fn [& args]
  (->> args
       (mapcat (fn [item]
                 (cond
                   (sequential? item) item
                   :otherwise [item])))
       (filter number?)
       count))

(defn max-fn [& items]
  (let [items (flatten items)]
    (if (every? number? items)
      (reduce max items)
      (throw (error/error "#VALUE!" "Function MAX parameters expects number values.")))))

(defn min-fn [& items]
  (let [items (flatten items)]
    (if (every? number? items)
      (reduce min items)
      (throw (error/error "#VALUE!" "Function MIN parameters expects number values.")))))
