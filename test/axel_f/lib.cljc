(ns axel-f.lib)

(defn fuzzy= [tolerance x y]
  (let [diff (#?(:clj Math/abs
                 :cljs js/Math.abs) (- x y))]
    (< diff tolerance)))
