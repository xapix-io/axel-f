(ns axel-f.functions.logic)

(defn and-fn [& args]
  (every? identity args))

(defn not-fn [logical-expression]
  (not logical-expression))

(defn or-fn [& args]
  (boolean (some identity args)))
