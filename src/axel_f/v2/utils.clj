(ns axel-f.v2.utils)

(defn constant? [e]
  ((some-fn string? number? boolean? keyword? set? nil?) e))
