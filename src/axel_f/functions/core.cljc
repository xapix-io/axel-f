(ns axel-f.functions.core
  (:require [clojure.string :as string]))

(def ^:dynamic *functions-store* (atom {}))

(defn def-excel-fn* [fn-name fn-impl]
  (swap! *functions-store* assoc fn-name
         (with-meta fn-impl (meta fn-impl)))
  nil)

(defn def-excel-fn
  ([fn-name fn-impl]
   (def-excel-fn* fn-name fn-impl))
  ([fn-name fn-impl & fn-name-impls]
   (def-excel-fn* fn-name fn-impl)
   (if fn-name-impls
     (if (next fn-name-impls)
       (recur (first fn-name-impls) (second fn-name-impls) (nnext fn-name-impls))
       (throw (IllegalArgumentException.
               "def-excel-fn expects even number of arguments after fn-impl, found odd number"))))))

(defn boolean?->number [x]
  (case x
    true 1
    false 0
    x))

(def +*
  (fn [& args]
    (reduce (fn [x y]
              (+ (boolean?->number x)
                 (boolean?->number y)))
            (first args)
            (rest args))))

(def -*
  (fn [& args]
    (let [single-arg? (= 1 (count args))]
      (reduce (fn [x y]
                (- (boolean?->number x)
                   (boolean?->number y)))
              (if single-arg?
                  0
                  (first args))
              (if single-arg?
                args
                (rest args))))))

(def-excel-fn
  "+" 'axel-f.functions.core/+*
  "-" 'axel-f.functions.core/-*)
