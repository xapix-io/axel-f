(ns axel-f.runtime
  (:refer-clojure :exclude [eval type])
  (:require [axel-f.functions :as core]
            [axel-f.lexer :as lexer]))

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

(defn type [{::keys [type]}]
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

(declare ^{:arglists '([x & [y z]])} eval)

(defmulti function-name (fn [{::keys [type]}] type))

(defmethod function-name ::root-reference-expr [{::keys [field-expr]}]
  (eval field-expr nil nil))

(defmethod function-name ::reference-expr [{::keys [ctx-expr field-expr]}]
  (str (function-name ctx-expr) "." (eval field-expr nil nil)))

(defmulti eval (fn [{::keys [type]} & _] type))

(defmethod eval ::constant-expr [{::keys [value]} & _] value)

(defmethod eval ::operator [{::keys [operator]} & _] operator)

(defmethod eval ::unary-expr [{::keys [operator expr]} & [g l]]
  ((eval operator g l) (eval expr g l)))

(defmethod eval ::binary-expr [{::keys [operator left-expr right-expr]} & [g l]]
  ((eval operator g l)
   (eval left-expr g l)
   (eval right-expr g l)))

(defmethod eval ::list-expr [{::keys [exprs]} & [g l]]
  (map #(eval % g l) exprs))

(defmethod eval ::root-reference-expr [{::keys [field-expr]} & [g l]]
  (let [f (eval field-expr g l)]
    (if (= "_" f)
      (if (= ::no-ctx l) g l)
      ((core/find-impl "flexy-get") g f))))

(defmethod eval ::reference-expr [{::keys [ctx-expr field-expr]} & [g l]]
  (let [m (eval ctx-expr g l)
        f (eval field-expr g l)]
    (cond
      (map? m)
      ((core/find-impl "flexy-get") m f)

      (sequential? m)
      (map #((core/find-impl "flexy-get") % f) m))))

(defmethod eval ::index-expr [{::keys [ctx-expr ref-expr]} & [g l]]
  (let [m (if ctx-expr (eval ctx-expr g l) g)]
    (if (operator? ref-expr)
      (when (sequential? m) m)
      (let [i (eval ref-expr g l)]
        ((core/find-impl "flexy-nth") m i)))))

(defmethod eval ::application-expr [{::keys [fs arg-exprs]} & [g l]]
  (case fs
        "MAP" (map (fn [*ctx*]
                     (eval (first arg-exprs) g *ctx*))
                   (eval (second arg-exprs) g l))

        "FILTER" (filter (fn [*ctx*]
                           (eval (first arg-exprs) g *ctx*))
                         (eval (second arg-exprs) g l))

        "SORT" (sort-by (fn [*ctx*]
                          (eval (first arg-exprs) g *ctx*))
                        (eval (second arg-exprs) g l))

        "IF" (if (eval (first arg-exprs) g l)
               (eval (second arg-exprs) g l)
               (when-let [arg-expr (nth arg-exprs 2 nil)]
                 (eval arg-expr g l)))

        "IFS" (loop [[[if-expr then-expr] & clauses] (partition 2 2 arg-exprs)]
                (when (some? if-expr)
                  (if (eval if-expr g l)
                    (eval then-expr g l)
                    (recur clauses))))

        (apply (core/find-impl fs)
               (map #(eval % g l)
                    arg-exprs))))

(defmethod eval ::formula [{::keys [expr]}]
  (let [f (gensym)]
    (fn f
      ([] (f nil))
      ([ctx] (eval expr ctx ::no-ctx)))))

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
     ::precedence p}))

(defn unary-expr
  "Construct unary expression from operator and another expression as operand"
  [operator expr]
  {::type ::unary-expr
   ::operator operator
   ::expr expr})

(defn binary-expr
  "Construct binary expression from operator and two expressions"
  [operator left-expr right-expr]
  {::type ::binary-expr
   ::left-expr left-expr
   ::right-expr right-expr
   ::operator operator})

(defn list-expr
  "Build an expression to be a list of `exps`"
  [exprs]
  {::type ::list-expr
   ::exprs exprs})

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
  [ctx-expr ref-expr]
  {::type ::index-expr
   ::ctx-expr ctx-expr
   ::ref-expr ref-expr})

(defn application-expr
  "Function call expression"
  [fs arg-exprs]
  {::type ::application-expr
   ::fs fs
   ::arg-exprs arg-exprs})

(defn formula-expr [expr]
  {::type ::formula
   ::expr expr})
