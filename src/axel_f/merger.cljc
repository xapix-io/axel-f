(ns axel-f.merger)

(defn decorator-by-path [path]
  (fn [v]
    (letfn [(decorate [[k & path] x]
              (cond
                (= "*" k) (mapv (partial decorate path) x)
                k {k (decorate path x)}
                :else x))]
      (decorate path v))))

(defn map-exhaustive [f a b]
  (let [a-c (count a)
        b-c (count b)
        [a' b'] (cond
                  (< a-c b-c) [(concat a (repeat nil)) b]
                  (< b-c a-c) [a (concat b (repeat nil))]
                  :else [a b])]
    (map f a' b')))

(defn deep-merge [a b]
  (cond
    (and (map? a) (map? b))
    (merge-with deep-merge a b)

    (and (sequential? a) (sequential? b))
    (map-exhaustive (fn [a b]
                      (cond
                        (and (some? a) (some? b)) (deep-merge a b)
                        (some? a) a
                        :else b))
                    a b)

    :else b))

(defn merger [compiler attributes]
  (if attributes
    (let [extractors (for [{:strs [formula name]} attributes
                           :let [path (first (:free-variables (meta (compiler name))))
                                 extractor (compiler formula)
                                 decorator (decorator-by-path path)]]
                       (comp decorator extractor))]
      (fn [request]
        (reduce deep-merge {} (map #(% request) extractors))))
    (constantly nil)))
