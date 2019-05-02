(ns axel-f.autocomplete
  (:require [clj-fuzzy.metrics :as fuzzy]
            [clojure.string :as string]
            [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]))

(defn arg->doc [arg opts]
  (let [{:keys [doc]} (meta arg)]
    (merge {:desc doc} opts)))

(defn arglist->doc [arglist]
  (loop [acc [] opts {} arg (first arglist) arglist (rest arglist)]
    (if arg
      (if (= arg '&)
        (recur acc (assoc opts :opt true :repeatable true) (first arglist) (rest arglist))
        (if (vector? arg)
          (recur acc (dissoc opts :repeatable) (first arg) (concat (rest arg) (rest arglist)))
          (recur (conj acc (arg->doc arg opts)) {} (first arglist) (rest arglist))))
      acc)))

(defn ref-meta->doc [{:keys [doc arglists]}]
  {:desc doc
   :args (arglist->doc (first arglists))})

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
      ;; (map (fn [[p v]]
      ;;        (vector (concat pre p) v))
      ;;      (wrapper m))
      (map (fn [[k v]]
             (flatten-map-entry wrapper v (concat pre (list k))))
           m))

     (vector? m)
     (concat
      (list (vector (concat pre (list "*")) (wrapper m)))
      ;; (map (fn [[p v]]
      ;;        (vector (concat pre p) v))
      ;;      (wrapper m))
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

     (nil? m)
     '()

     :else
     [pre (wrapper m)])))

(defn ->string [s]
  (if (keyword? s)
    (string/join "/" (filter identity ((juxt namespace name) s)))
    (str s)))

(defn suggestion-wrapper [v]
  (cond
    ((some-fn var? fn?) v)
    (merge {:type :FNCALL}
           (ref-meta->doc (meta v)))

    ;; (map? v)
    ;; (map #(list (list (->string %)) {:type :REF :desc "Field in the context"})
    ;;      (keys v))

    :else
    {:type :REF
     :desc "Field in the context"}))

(defn distance [s1 s2]
  (if (empty? s2)
    1 (fuzzy/jaccard s1 s2)))

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

(defn wrap-suggestion [[n sug]]
  (assoc sug :value (last n)))

(defn suggest-fn [env context]
  (let [indexer (partial flatten-map-entry suggestion-wrapper)
        index-env (indexer env)
        index-context (indexer context)]
    (letfn [(dist [path [p _]]
              (distance (->string (last path))
                        (->string (last p))))
            (match [path]
              (sort-by (partial dist path)
                       (filter (fn [[p _]]
                                 (or (= p path)
                                     (and (= (count p)
                                             (inc (count path)))
                                          (= path (butlast p)))
                                     (and (= (map ->string (butlast p))
                                             (map ->string (butlast path)))
                                          (< (dist path [p nil]) 1))))
                               (concat index-env index-context))))
            (extract-paths [formula]
              (let [formula (lexer/read formula)
                    var-formula (remove-trailing-symbols (substract-var formula))
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
                [(meta fncall) (meta var)]))]
      (fn [formula]
        (let [[f v] (extract-paths formula)
              f' (-> f :fn-name first)
              v' (-> v :free-variables)]
          (merge (when-let [x (when f' (match f'))]
                   {:context (assoc (wrap-suggestion (first x))
                                    :current-arg (:args-count f))})
                 (when-let [v (when (not-empty v')
                                (mapcat #(match %) v'))]
                   {:suggestions (map wrap-suggestion v)})))))))
