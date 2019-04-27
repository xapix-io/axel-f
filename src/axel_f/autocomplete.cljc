(ns axel-f.autocomplete
  (:require [clj-fuzzy.metrics :as fuzzy]
            [clojure.string :as string]
            [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]))

(defn- fl*
  ([xs] (fl* '() xs))
  ([acc [x & xs]]
   (if x
     (if (vector? x)
       (fl* (cons x acc) xs)
       (fl* (concat acc (fl* x)) xs))
     acc)))

(defn flatten-map-entry
  ([wrapper m]
   (into {} (fl* (flatten-map-entry wrapper m '()))))
  ([wrapper m pre]
   (cond
     (map? m)
     (concat
      (list (vector pre (wrapper m)))
      (map (fn [[k v]]
             (flatten-map-entry wrapper v (concat pre (list k))))
           m))

     (vector? m)
     (concat
      (list (vector (concat pre (list "*")) (wrapper m)))
      (apply concat
             (map-indexed (fn [i m']
                            (let [x (flatten-map-entry wrapper m' (concat pre (list i)))]
                              (if (vector? x)
                                (list x)
                                x)))
                          m))
      (mapcat (fn [m']
                (let [x (flatten-map-entry wrapper m' (concat pre (list "*")))]
                  (if (vector? x)
                    (list x)
                    x)))
              (filter map? m)))

     :else
     [pre (wrapper m)])))

(defn suggestion-wrapper [v]
  (cond
    ((some-fn var? fn?) v)
    (merge {:type :function}
           (meta v))

    (map? v)
    v

    :else
    {:type :value
     :value v}))

(defn ->string [s]
  (if (keyword? s)
    (string/join "/" (filter identity ((juxt namespace name) s)))
    (str s)))

(defn distance [s1 s2]
  (cond
    (empty? s2)
    1

    (string/starts-with?
     (string/lower-case s2)
     (string/lower-case s1))
    0

    :else (fuzzy/jaccard s1 s2)))

(defn substract-var
  ([xs] (let [xs (reverse xs)]
          (substract-var '() (first xs) (rest xs))))
  ([acc {::lexer/keys [type value] :as x} xs]
   (if x
     (case type
       ::lexer/punct
       (case value
         "," acc
         "(" acc
         ")" acc
         (substract-var (cons x acc) (first xs) (rest xs)))

       ::lexer/operator
       (case value
         "*" (if (and (= ::lexer/punct (::lexer/type (first xs)))
                      (= "[" (::lexer/value (first xs))))
               (substract-var (cons x acc) (first xs) (rest xs))
               acc)
         acc)

       (substract-var (cons x acc) (first xs) (rest xs)))
     acc)))

(declare substract-symbol substract-dot)

(defn remove-trailing-symbols [f]
  (let [{::lexer/keys [type value]} (last f)]
    (if (or (= ::lexer/eof type)
            (and (= ::lexer/punct type)
                 (= "." value)))
      (remove-trailing-symbols (butlast f))
      f)))

(defn substract-dot [acc {::lexer/keys [type value] :as x} xs]
  (if (and (= ::lexer/punct type)
           (= "." value))
    (substract-symbol (cons x acc) (first xs) (rest xs))
    acc))

(defn substract-symbol [acc {::lexer/keys [type] :as x} xs]
  (if (= ::lexer/symbol type)
    (substract-dot (cons x acc) (first xs) (rest xs))
    acc))

(defn substract-fncall
  ([xs] (let [xs (reverse xs)]
          (substract-fncall '() 0 (first xs) (rest xs))))
  ([acc c {::lexer/keys [type value] :as x} xs]
   (if x
     (case type
       ::lexer/punct
       (case value
         "(" (if (= 0 c)
               (substract-symbol (cons x acc) (first xs) (rest xs))
               (substract-fncall (cons x acc) (dec c) (first xs) (rest xs)))
         ")" (substract-fncall (cons x acc) (inc c) (first xs) (rest xs))
         (substract-fncall (cons x acc) c (first xs) (rest xs)))

       (substract-fncall (cons x acc) c (first xs) (rest xs)))
     nil)))

(defn suggest-fn [env context]
  (let [indexer (partial flatten-map-entry suggestion-wrapper)
        index-env (indexer env)
        index-context (indexer context)]
    (letfn [(match [path]
              (filter (fn [[p _]]
                        (or (= p path)
                            (and (= (map ->string (butlast p))
                                    (map ->string (butlast path)))
                                 (< (distance (->string (last path))
                                              (->string (last p))) (/ 2 3)))))
                      (concat index-env index-context)))
            (extract-paths [formula]
              (let [formula (lexer/read formula)
                    var-formula (substract-var formula)
                    fncall-formula (concat (remove-trailing-symbols (substract-fncall formula))
                                           '({::lexer/type ::lexer/punct ::lexer/value ")"}
                                             {::lexer/type ::lexer/eof}))
                    var (try
                          (-> var-formula parser/parse compiler/compile)
                          (catch #?(:clj Throwable :cljs js/Error) _
                            nil))
                    fncall (try
                             (-> fncall-formula parser/parse compiler/compile)
                             (catch #?(:clj Throwable :cljs js/Error) _
                               nil))]
                [(:fn-name (meta fncall)) (:free-variables (meta var))]))]
      (fn [formula]
        (let [[f v] (extract-paths formula)]
          (merge (when-let [x (when-let [f (first f)]
                                (match f))]
                   {:fn x})
                 (when-let [v (when (not-empty v)
                                (mapcat #(match %) v))]
                   {:var v})))))))
