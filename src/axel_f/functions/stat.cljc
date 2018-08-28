(ns axel-f.functions.stat
  (:require [axel-f.functions.math :as math]
            [axel-f.functions.coercion :as coercion]
            [axel-f.error :as error]))

(defn average-fn [& items]
  (let [items (filter number?
                      (map coercion/excel-number
                           (mapcat (fn [item]
                                     (cond
                                       (string? item) [item]
                                       (sequential? item) item
                                       (boolean? item) [item]
                                       :otherwise nil)) items)))
        len (count items)]
    (when-not (zero? len)
      (/ (apply math/sum-fn items)
         len))))

(defn count-fn [& args]
  (->> args
       (mapcat (fn [item]
                 (cond
                   (string? item) [item]
                   (sequential? item) item
                   (boolean? item) [item]
                   (number? item) [item]
                   :otherwise nil)))
       (map coercion/excel-number)
       (filter number?)
       count))

(defn max-fn [& items]
  (reduce max
          (map (fn [n]
                 (if-let [n (coercion/excel-number n)]
                   n
                   (throw (error/error "#VALUE!" (error/format-not-a-number-error "MAX" nil n)))))
               (flatten items))))

(defn min-fn [& items]
  (reduce min
          (map (fn [n]
                 (if-let [n (coercion/excel-number n)]
                   n
                   (throw (error/error "#VALUE!" (error/format-not-a-number-error "MIN" nil n)))))
               (flatten items))))
