(ns axel-f.excel
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]
            [axel-f.excel.operators :as operators]
            [axel-f.excel.collections :as collections]
            [axel-f.excel.base64 :as base64]
            [axel-f.excel.geo :as geo]
            [axel-f.excel.hash :as hash]
            [axel-f.excel.json :as json]
            [axel-f.excel.logic :as logic]
            [axel-f.excel.math :as math]
            [axel-f.excel.object :as object]
            [axel-f.excel.stat :as stat]
            [axel-f.excel.text :as text]))

(defn ^:special? FN*
  "Defines lambda function"
  [forms ast-forms]
  (let [body (last forms)
        arglist (map (fn [{::parser/keys [parts]}]
                       ;; TODO throw if not one part
                       (first parts))
                     (butlast ast-forms))]
    (fn [ctx]
      (fn [& args]
        (body (apply assoc ctx (mapcat identity (zipmap arglist args))))))))

(def FN #'FN*)

(defn ^:special? IF*
  "Evaluates test. If not the singular values nil or false, evaluates and yields then, otherwise, evaluates and yields else. If else is not supplied it defaults to nil."
  [[test then else] _]
  (fn [ctx]
    (if (test ctx)
      (then ctx)
      (when else (else ctx)))))

(def IF #'IF*)

(defn ^:special? IFS*
  "Takes a set of test/expr pairs. It evaluates each test one at a time.  If a test returns logical true, cond evaluates and returns the value of the corresponding expr and doesn't evaluate any of the other tests or exprs. (IFS) returns nil."
  [forms _]
  (fn [ctx]
    (loop [[[test then :as test-then] & clauses] (partition-all 2 2 forms)]
      (cond
        (empty? test-then)
        nil

        (= 1 (count test-then))
        (test ctx)

        :else
        (if (test ctx)
          (then ctx)
          (recur clauses))))))

(def IFS #'IFS*)

(defn ^:special? WITH*
  "Extends context"
  [forms ast-forms]
  (fn [ctx]
    (loop [ctx ctx
           [[_ form :as binding] & bindings] (partition-all 2 2 forms)
           [[{::parser/keys [parts] :as var} _] & ast-bindings] (partition-all 2 2 ast-forms)]
      (cond
        (empty? binding)
        nil

        (= 1 (count binding))
        ((first binding) ctx)

        :else
        (let [value (form ctx)]
          (recur (assoc ctx (first parts) value) bindings ast-bindings))))))

(def WITH #'WITH*)

(def env
  (merge
   {"FN"     FN
    "IF"     IF
    "IFS"    IFS
    "WITH"   WITH}
   operators/env
   collections/env
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

(defn- str->ast [s]
  (-> s lexer/read parser/parse))

(defn compile
  ([formula] (compile formula nil))
  ([formula extra-env]
   (let [ast (str->ast formula)
         f (compiler/compile ast)]
     (partial f (merge env extra-env)))))
