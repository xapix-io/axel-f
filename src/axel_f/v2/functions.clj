(ns axel-f.v2.functions)

(def ^:dynamic *registry* (atom {}))

(defn add-op [& args]
  (reduce +' args))

(defn sub-op [& args]
  (reduce -' 0 args))

(defn mult-op [& args]
  (reduce *' args))

(defn div-op [& args]
  (reduce / 1 args))

(defn concat-op [& args]
  (reduce str args))

(defn more-op [arg1 & args]
  (apply > arg1 args))

(defn less-op [arg1 & args]
  (apply < arg1 args))

(defn more-or-eq-op [arg1 & args]
  (apply >= arg1 args))

(defn less-or-eq-op [arg1 & args]
  (apply <= arg1 args))

(defn not-eq-op [arg1 & args]
  (apply not= arg1 args))

(defn eq-op [arg1 & args]
  (apply = arg1 args))

(defn not-op [arg]
  (not arg))

(defn- ->keyword [k]
  (if (keyword? k) k (keyword k)))

(defn- ->string [k]
  (if (string? k) k (name k)))

(defn- ->symbol [k]
  (if (symbol? k) k (symbol (->string k))))

(defn- get* [m k]
  (or (get m (->keyword k))
      (get m (->string k))
      (get m (->symbol k))
      (get m k)))

(defn- wildcart? [k]
  (= :WILDCART k))

(defn get-in* [m ks]
  (loop [m m k (first ks) ks (rest ks)]
    (if k
      (if (wildcart? k)
        (when (coll? m)
          (map #(get-in* % ks) m))
        (recur (get* m k) (first ks) (rest ks)))
      m)))

(def default-registry
  {:ADD_OP        {:impl add-op}
   :SUB_OP        {:impl sub-op}
   :MULT_OP       {:impl mult-op}
   :DIV_OP        {:impl div-op}
   :CONCAT_OP     {:impl concat-op}
   :MORE_OP       {:impl more-op}
   :LESS_OP       {:impl less-op}
   :MORE_OR_EQ_OP {:impl more-or-eq-op}
   :LESS_OR_EQ_OP {:impl less-or-eq-op}
   :NOT_EQ_OP     {:impl not-eq-op}
   :EQ_OP         {:impl eq-op}
   :NOT_OP        {:impl not-op}})
