(ns axel-f.runtime
  (:refer-clojure :exclude [eval type])
  (:require [axel-f.functions :as core]
            [axel-f.lexer :as lexer])
  #?(:clj (:import clojure.lang.ExceptionInfo)))

(defn- operator-precedence [value]
  (cond
    ;; Logic operators - Lowest precedence
    (contains? #{"<" ">" "<=" ">=" "=" "<>"} value) 0
    ;; Concatenate operator
    (= "&" value) 1
    ;; Additive operators
    (contains? #{"+" "-"} value) 2
    ;; Multiplicative operators
    (contains? #{"*" "/"} value) 3
    ;; Exponential operator
    (= "^" value) 4
    :otherwise -1))

(defn type [{::keys [type]} & _]
  type)

(defn precedence [{::keys [precedence]}]
  precedence)

(defn operator [{::keys [operator]}]
  operator)

(defn left [{::keys [left-expr]}]
  left-expr)

(defn right [{::keys [right-expr]}]
  right-expr)

(defn expr? [{::keys [type]}]
  (boolean type))

(defn operator? [{::keys [type]}]
  (= type ::operator))

(defn binary? [{::keys [type]}]
  (= type ::binary-expr))

(declare eval)

(defmulti function-name type)

(defmethod function-name :default [_]
  (throw (ex-info "Not implemented" {})))

(defmethod function-name ::root-reference-expr [{::keys [field-expr]}]
  (eval field-expr nil nil))

(defmethod function-name ::reference-expr [{::keys [ctx-expr field-expr]}]
  (str (function-name ctx-expr) "." (eval field-expr nil nil)))

(defmulti position type)

(defmulti eval type)

(defmethod position ::constant-expr [{::keys [token]}]
  {:begin (::lexer/begin token)
   :end (::lexer/end token)})

(defmethod eval ::constant-expr [{::keys [value]} & [_ _ _]] value)

(defmethod position ::operator [{::keys [token]}]
  {:begin (::lexer/begin token)
   :end (::lexer/end token)})

(defmethod eval ::operator [{::keys [operator]} & [_ _ _]] operator)

(defmethod position ::unary-expr [{::keys [operator expr]}]
  (if (lexer/prefix-operator? (::token operator))
    {:begin (:begin (position operator))
     :end (:end (position expr))}
    {:end (:end (position operator))
     :begin (:begin (position expr))}))

(defmethod eval ::unary-expr [{::keys [operator expr] :as ue} & [g l c]]
  (let [op (eval operator g l c)
        arg (eval expr g l c)]
    (try
      (op arg)
      (catch ExceptionInfo e
        (throw (ex-info "Formula error"
                        {:position (position ue)}))))))

(defmethod position ::binary-expr [{::keys [left-expr right-expr]}]
  {:begin (:begin (position left-expr))
   :end (:end (position right-expr))})

(defmethod eval ::binary-expr [{::keys [operator left-expr right-expr] :as be} & [g l c]]
  (let [op (eval operator g l c)
        arg1 (eval left-expr g l c)
        arg2 (eval right-expr g l c)]
    (try
      (op arg1 arg2)
      (catch ExceptionInfo e
        (throw (ex-info "Formula error"
                        {:position (position be)}))))))

(defmethod position ::list-expr [{::keys [open-token close-token]}]
  {:begin (::lexer/begin open-token)
   :end (::lexer/end close-token)})

(defmethod eval ::list-expr [{::keys [exprs]} & [g l c]]
  (map #(eval % g l c) exprs))

(defmethod position ::root-reference-expr [{::keys [field-expr]}]
  {:begin (:begin (position field-expr))
   :end (:end (position field-expr))})

(defmethod eval ::root-reference-expr [{::keys [field-expr]} & [g l c]]
  (if (= ::application-expr (::type field-expr))
    (eval field-expr g l c)
    (let [f (eval field-expr g l c)]
      (if (= "_" f)
        (if (= ::no-ctx l) g l)
        (or (get c f)
            ((core/find-impl "flexy-get") g f))))))

(defmethod position ::reference-expr [{::keys [ctx-expr field-expr]}]
  (merge (position ctx-expr)
         {:end (:end (position field-expr))}))

(defmethod eval ::reference-expr [{::keys [ctx-expr field-expr]} & [g l c]]
  (let [m (eval ctx-expr g l c)
        f (eval field-expr g l c)]
    (cond
      (map? m)
      ((core/find-impl "flexy-get") m f)

      (sequential? m)
      (map #((core/find-impl "flexy-get") % f) m))))

(defmethod position ::index-expr [{::keys [ctx-expr open-token close-token]}]
  {:begin (if ctx-expr
            (:begin (position ctx-expr))
            (::lexer/begin open-token))
   :end (::lexer/end close-token)})

(defmethod eval ::index-expr [{::keys [ctx-expr ref-expr]} & [g l c]]
  (let [m (if ctx-expr (eval ctx-expr g l c) g)]
    (if (or (nil? ref-expr)
            (operator? ref-expr))
      (when (sequential? m) m)
      (let [i (eval ref-expr g l)]
        ((core/find-impl "flexy-nth") m i)))))

(defmethod position ::application-expr [{::keys [ref-expr arg-list]}]
  {:begin (:begin (position ref-expr))
   :end (:end (position arg-list))})

(defn- min-args [{:keys [args]}]
  (count (filter #(not (:opt %)) args)))

(defn- max-args [{:keys [args]}]
  (if (:repeatable (last args))
    ##Inf
    (count args)))

(defn- check-arguments [f meta arg-list]
  (let [args-count (count (::exprs arg-list))
        min (min-args meta)
        max (max-args meta)]
    (when-not (<= min args-count max)
      (throw (ex-info (str "Wrong number of arguments passed to `" f "` function.")
                      {:position (position arg-list)})))))

(defn- eval-special-fn [special-f fn arg-exprs arg-list ref-expr g l c]
  (when-not (= (count arg-exprs) 2)
    (throw (ex-info (str "Wrong number of arguments passed to `" fn "` function.")
                    {:position {:begin (:begin (position ref-expr))
                                :end (:end (position arg-list))}})))
  (let [f (first arg-exprs)
        args (second arg-exprs)]
    (let [f' (and (= ::root-reference-expr (::type f)) (get c (function-name f)))]
      (special-f (or f' #(eval f g % c))
                 (eval args g l c)))))

(defmethod eval ::application-expr [{::keys [fs arg-list ref-expr]} & [g l c]]
  (let [arg-exprs (::exprs arg-list)]
    (case fs
      "MAP" (eval-special-fn map fs arg-exprs arg-list ref-expr g l c)
      "FILTER" (eval-special-fn filter fs arg-exprs arg-list ref-expr g l c)
      "SORT" (eval-special-fn sort-by fs arg-exprs arg-list ref-expr g l c)

      "IF" (do
             (when-not (<= 2 (count arg-exprs) 3)
               (throw (ex-info "Wrong number of arguments passed to `IF` function."
                               {:position {:begin (:begin (position ref-expr))
                                           :end (:end (position arg-list))}})))
             (let [condition (eval (first arg-exprs) g l c)
                   condition (if (fn? condition) (condition g l) condition)]
               (if condition
                   (eval (second arg-exprs) g l c)
                 (when-let [arg-expr (nth arg-exprs 2 nil)]
                   (eval arg-expr g l c)))))

      "IFS" (do
              (when-not (even? (count arg-exprs))
                (throw (ex-info "Function `IFS` expecting even number of arguments"
                                {:position {:begin (:begin (position ref-expr))
                                            :end (:end (position arg-list))}})))
              (loop [[[if-expr then-expr] & clauses] (partition 2 2 arg-exprs)]
                (when (some? if-expr)
                  (let [condition (eval if-expr g l c)
                        condition (if (fn? condition) (condition g l) condition)]
                    (if condition
                      (eval then-expr g l c)
                      (recur clauses))))))

      "WITH" (let [arg-exprs (partition-all 2 arg-exprs)]
               (loop [[binding & bindings] arg-exprs closures c]
                 (if (= 1 (count binding))
                   (eval (first binding) g l closures)
                   (let [[name-expr val-expr] binding]
                     (recur bindings (assoc closures
                                            (function-name name-expr)
                                            (eval val-expr g l closures)))))))

      "FN" ((get-method eval ::formula) {::expr (first arg-exprs)} g l c)

      (if-let [f-impl (core/find-impl fs)]
        (do
          (check-arguments fs (core/find-meta fs) arg-list)
          (let [args (map #(eval % g l c)
                          arg-exprs)]
            (try
              (apply f-impl args)
              (catch ExceptionInfo e
                (if (:position (ex-data e))
                  (throw e)
                  (throw (ex-info (str "Error in function `" fs "`")
                                  {:position {:begin (:begin (position ref-expr))
                                              :end (:end (position arg-list))}
                                   :arguments args
                                   :cause (:cause (ex-data e))})))))))
        (throw (ex-info (str "Unknown function `" fs "`")
                        {:position (position ref-expr)}))))))

(defmethod position ::formula [{::keys [expr]}]
  (position expr))

(defmethod eval nil [_] nil)

(defmethod eval ::formula [{::keys [expr]} & [g l c]]
  (let [f (gensym)]
    (fn f
      ([] (f nil))
      ([ctx] (f ctx nil))
      ([global-ctx local-ctx]
       (eval expr (or global-ctx g) (or local-ctx l ::no-ctx) (or c {}))))))

(defn constant-expr
  "Convert `token` into constant expression"
  [{::lexer/keys [value] :as token}]
  {::type ::constant-expr
   ::token token
   ::value value})

(defn operator-expr
  "Convert `token` into operator expression"
  [{::lexer/keys [value] :as token}]
  (let [p (operator-precedence value)
        op (core/find-impl value)]
    {::type ::operator
     ::operator op
     ::precedence p
     ::token token}))

(defn unary-expr
  "Construct unary expression from operator and another expression as operand"
  [operator expr]
  {::type ::unary-expr
   ::operator operator
   ::expr expr})

(defn binary-expr
  "Construct binary expression from operator and two expressions"
  [operator left-expr right-expr]
  (if (nil? right-expr)
    (throw (ex-info "Missing operand for binary expression"
                    {:position (position operator)}))
    {::type ::binary-expr
     ::left-expr left-expr
     ::right-expr right-expr
     ::operator operator}))

(defn list-expr
  "Build an expression to be a list of `exps`"
  [exprs open-token close-token]
  {::type ::list-expr
   ::exprs exprs
   ::open-token open-token
   ::close-token close-token})

(defn root-reference-expr
  "Build root reference expression"
  [field-expr]
  {::type ::root-reference-expr
   ::field-expr field-expr})

(defn reference-expr
  "Reference expression of `ctx-expr` (can be root expression or another reference expression)
  and `field-expr`"
  [ctx-expr field-expr]
  {::type ::reference-expr
   ::ctx-expr ctx-expr
   ::field-expr field-expr})

(defn index-expr
  "Additional type of `field-expr` for [[root-reference-expr]] or [[reference-expr]]
  Semantic - 'get from array by index'"
  [ctx-expr ref-expr open-token close-token]
  {::type ::index-expr
   ::ctx-expr ctx-expr
   ::ref-expr ref-expr
   ::open-token open-token
   ::close-token close-token})

(defn application-expr
  "Function call expression"
  [ref-expr arg-list]
  {::type ::application-expr
   ::fs (function-name ref-expr)
   ::ref-expr ref-expr
   ::arg-list arg-list})

(defn formula-expr [expr]
  {::type ::formula
   ::expr expr})
