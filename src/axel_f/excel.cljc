(ns axel-f.excel
  (:refer-clojure :exclude [eval])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.runtime :as runtime]
            [axel-f.excel.operators :as operators]
            [axel-f.excel.base64 :as base64]
            [axel-f.excel.geo :as geo]
            [axel-f.excel.hash :as hash]
            [axel-f.excel.json :as json]
            [axel-f.excel.logic :as logic]
            [axel-f.excel.math :as math]
            [axel-f.excel.object :as object]
            [axel-f.excel.stat :as stat]
            [axel-f.excel.text :as text]))

;; ======================== Helpers: ========================

(defn- function? [{::parser/keys [type]
                   {::parser/keys [parts]} ::parser/function
                   :as appl}]
  (and (= ::parser/application type)
       (= "FN" (::lexer/value (first parts)))))

(defn infer-var-name [{::parser/keys [type parts]}]
  (when (= ::parser/var type)
    (loop [acc [] [{::lexer/keys [value] :as p} & parts] parts]
      (if (some? p)
        (recur (conj acc value) parts)
        acc))))

(defn assign [env value var-path]
  (if-let [p (first var-path)]
    (update env p (fn [e]
                    (assign (or e {}) value (rest var-path))))
    value))

(defn- wrap-special-fn [f]
  (fn [env & args]
    (let [f' (if (function? (first args))
               (runtime/eval* env (first args))
               ((get env "FN") env (first args)))
          args (map (partial runtime/eval* env) (next args))]
      (apply f f' args))))

(defn special-if [env & args]
  (loop [[[if-expr then-expr :as if-then-exprs] & clauses] (partition-all 2 2 args)]
    (cond
      (empty? if-then-exprs) nil
      (= 1 (count if-then-exprs)) (runtime/eval* env if-expr)
      :otherwise
      (if (runtime/eval* env if-expr)
        (runtime/eval* env then-expr)
        (recur clauses)))))

(defn special-with [env & args]
  (loop [env env [[var-expr expr :as binding] & bindings] (partition-all 2 2 args)]
    (cond
      (empty? binding) nil
      (= 1 (count binding)) (runtime/eval* env var-expr)
      :otherwise
      (let [var-name (infer-var-name var-expr)
            value (runtime/eval* env expr)]
        (recur (assign env value var-name) bindings)))))

;; ================= Base runtime functions =================

(defn ^:special? FN*
  "Defines lambda function"
  [env & args]
  (fn [ctx]
    (runtime/eval* (assoc env "_" ctx)
                   (last args))))

(def FN #'FN*)

(def ^:special? ^{:arglists '([f coll])}
  MAP*
  "Applies partiualy defined formula to every element in a collection and returns an array."
  (wrap-special-fn map))

(def MAP #'MAP*)

(def ^:special? ^{:arglists '([f coll])}
  FILTER*
  "Returns an array of elements that have been filtered based on a condition."
  (wrap-special-fn filter))

(def FILTER #'FILTER*)

(def ^:special? ^{:arglists '([f coll])}
  SORT*
  "Sorts a collection by the values returned from applying a sorting function to each element in said collection."
  (wrap-special-fn sort-by))

(def SORT #'SORT*)

(def ^:special? ^{:arglists '([if test then & [else]])}
  IF*
  "Evaluates test. If not the singular values nil or false, evaluates and yields then, otherwise, evaluates and yields else. If else is not supplied it defaults to nil."
  special-if)

(def IF #'IF*)

(def ^:special? ^{:arglists '([& clauses])}
  IFS*
  "Takes a set of test/expr pairs. It evaluates each test one at a time.  If a test returns logical true, cond evaluates and returns the value of the corresponding expr and doesn't evaluate any of the other tests or exprs. (IFS) returns nil."
  special-if)

(def IFS #'IFS*)

(def ^:special? ^{:arglists '([bindings body])}
  WITH*
  "Extends context"
  special-with)

(def WITH #'WITH*)

(def env
  (merge
   {"FN"     FN
    "MAP"    MAP
    "FILTER" FILTER
    "SORT"   SORT
    "IF"     IF
    "IFS"    IFS
    "WITH"   WITH}
   operators/env
   base64/env
   geo/env
   hash/env
   json/env
   logic/env
   math/env
   object/env
   stat/env
   text/env))

;; ==========================================================

(defn str->ast [s]
  (-> s lexer/read parser/parse))

(defn eval
  ([ast] (eval ast nil))
  ([ast extra-env]
   (let [ast (if (string? ast)
               (str->ast ast)
               ast)]
     (runtime/eval* (merge env extra-env) ast))))
