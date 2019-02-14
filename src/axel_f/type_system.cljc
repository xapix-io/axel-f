(ns axel-f.type-system)

(defn simple-type? [T]
  (and (map? T)
       (#{::number
          ::string
          ::boolean
          ::list
          ::free}
        (:t T))))

(defn free-type? [T]
  (case (:t T)
    ::free true
    ::union (some free-type?
                  (:ts T))
    false))

(defn union [& TS]
  (let [TS (filter identity TS)]
    (loop [U #{} T (first TS) TS (rest TS)]
      (if T
        (recur (into U (if (simple-type? T)
                         #{T}
                         (:ts T)))
               (first TS)
               (rest TS))
        (if (and (= 1 (count U))
                 (simple-type? (first U)))
          (first U)
          {:t ::union
           :ts U})))))

(defn list-type [TS]
  {:t ::list
   :lt (apply union TS)
   :ts TS})

(defn simple-type [T]
  {:t T})

(defn free-type [v]
  {:t ::free
   :path v})
