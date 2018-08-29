(ns axel-f.functions.stat
  (:require [axel-f.functions.math :as math]
            [axel-f.functions.coercion :as coercion]
            [axel-f.error :as error]))

(defn- flatten-numbers [tr-coercer]
  (comp (mapcat #(cond
                   (string? %)     [(not-empty %)]
                   (sequential? %) (not-empty %)
                   (boolean? %)    [%]
                   (number? %)     [%]
                   :otherwise      nil))
        (filter identity)
        tr-coercer
        (filter number?)))

(defn average-fn [& items]
  (let [tr-flatten-numbers (flatten-numbers (map coercion/excel-number))
        items (sequence tr-flatten-numbers items)
        len (count items)]
    (when-not (zero? len)
      (/ (apply math/sum-fn items)
         len))))

(defn count-fn [& items]
  (let [tr-flatten-numbers (flatten-numbers (map coercion/excel-number))]
    (count (sequence tr-flatten-numbers items))))

(defn max-fn [& items]
  (let [tr-coercer (map (fn [n]
                          (if-let [n (coercion/excel-number n)]
                            n
                            (throw (error/error "#VALUE!" (error/format-not-a-number-error "MAX" nil n))))))
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply max (into [] tr-flatten-numbers items))))

(defn min-fn [& items]
  (let [tr-coercer (map (fn [n]
                          (if-let [n (coercion/excel-number n)]
                            n
                            (throw (error/error "#VALUE!" (error/format-not-a-number-error "MIN" nil n))))))
        tr-flatten-numbers (flatten-numbers tr-coercer)]
    (apply min (into [] tr-flatten-numbers items))))
