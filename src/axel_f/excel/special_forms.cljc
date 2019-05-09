(ns axel-f.excel.special-forms
  (:refer-clojure :exclude [compile])
  (:require [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.compiler :as compiler]
            [clojure.string :as string]))

(declare env)

(defn FN [args]
  (let [body (compiler/compile env (last args))
        arglist (mapv (fn [{::parser/keys [parts] ::lexer/keys [begin] :as arg}]
                        (if (> (count parts) 1)
                          (throw (ex-info (str "Wrong argument symbol: `" (string/join "." parts) "`")
                                          {:begin begin}))
                          (first parts)))
                      (butlast args))]
    (with-meta
      (fn [ctx]
        (fn [& args]
          (body (if-let [args (not-empty (mapcat identity (zipmap arglist args)))]
                  (apply assoc ctx args)
                  ctx))))
      {:free-variables (filter (fn [[v & _]]
                                 (not (contains? (set arglist) v)))
                               (:free-variables (meta body)))
       :fn-name '(("FN"))})))

(defn IF [args]
  (let [[test then else] args
        test (compiler/compile env test)
        then (compiler/compile env then)
        else (if else (compiler/compile env else) (constantly nil))]
    (with-meta
      (fn [ctx]
        (if (test ctx)
          (then ctx)
          (else ctx)))
      {:free-variables (mapcat #(:free-variables (meta %)) [test then else])
       :fn-name '(("IF"))})))

(defn IFS [args]
  (let [args (mapv (partial compiler/compile env) args)]
    (with-meta
      (fn [ctx]
        (loop [[[test then :as test-then] & clauses] (partition-all 2 2 args)]
          (case (count test-then)
            0 nil
            1 (test ctx)
            (if (test ctx)
              (then ctx)
              (recur clauses)))))
      {:free-variables (mapcat #(:free-variables (meta %)) args)
       :fn-name '(("IFS"))})))

(defn WITH [args]
  (let [bindings
        (loop [bindings []
               binding-var (first args)
               binding-form (second args)
               args (nnext args)]
          (if binding-form
            (recur (conj bindings [(first (::parser/parts binding-var))
                                   (compiler/compile env binding-form)])
                   (first args)
                   (second args)
                   (nnext args))
            (conj bindings (compiler/compile env binding-var))))
        free-variables
        (loop [free-vars [] closures []
               binding* (first bindings)
               bindings (next bindings)]
          (if (vector? binding*)
            (recur (concat free-vars (filter (fn [[v & _]]
                                               (not (contains? (set closures) v)))
                                             (:free-variables (meta (second binding*)))))
                   (cons (first binding*) closures)
                   (first bindings)
                   (next bindings))
            (concat free-vars (filter (fn [[v & _]]
                                        (not (contains? (set closures) v)))
                                      (:free-variables (meta binding*))))))]
    (with-meta
      (fn [ctx]
        (loop [ctx ctx binding* (first bindings) bindings (next bindings)]
          (if (vector? binding*)
            (recur (assoc ctx (first binding*) ((second binding*) ctx))
                   (first bindings)
                   (next bindings))
            (binding* ctx))))
      {:free-variables free-variables
       :fn-name '(("WITH"))})))

(defn FILTER [[pred-ast coll-ast]]
  (let [coll (compiler/compile env coll-ast)
        pred (if (and (= :axel-f.parser/application (:axel-f.parser/type pred-ast))
                      (= '("FN") (-> pred-ast :axel-f.parser/function :axel-f.parser/parts)))
               (compiler/compile env pred-ast)
               (FN (cons {:axel-f.parser/parts '(:axel-f.runtime/context)
                          :axel-f.parser/type :axel-f.parser/var}
                         (list pred-ast))))]
    (with-meta
      (fn [ctx]
        (filter (pred ctx) (coll ctx)))
      {:free-variables (apply concat (map (comp :free-variables meta) [pred coll]))
       :fn-name '(("FILTER"))})))

(def env
  {"IF" (with-meta IF
          {:doc "Evaluates test. If not the singular values nil or false, evaluates and yields then, otherwise, evaluates and yields else. If else is not supplied it defaults to nil."
           :arglists '([^{:doc "Test expression."} test
                        ^{:doc "Then expression. Eval and yield if test expression is not `null` or `false`"} then
                        & [^{:doc "Else expression."} else]])})
   "IFS" (with-meta IFS
           {:doc "Takes a set of test/expr pairs. It evaluates each test one at a time.  If a test returns logical true, cond evaluates and returns the value of the corresponding expr and doesn't evaluate any of the other tests or exprs. (IFS) returns nil."
            :arglists '([& ^{:doc "Test expression."} test
                         & ^{:doc "Then expression."} then
                         & [^{:doc "Else expression."} else]])})
   "WITH" (with-meta WITH
            {:doc "Evaluates the exprs in a lexical context in which the symbols in the binding-forms are bound to their respective init-exprs or parts therein."
             :arglists '([& ^{:doc "Local variable name."} var-name
                          & ^{:doc "Binding value"} var-value
                          ^{:doc "Expression."} body-expr])})
   "FN" (with-meta FN
          {:doc "Defines a function"
           :arglists '([& ^{:doc "Variable name."} var-name
                        ^{:doc "Body expression."} body-expr])})
   "FILTER" (with-meta FILTER
              {:doc "Returns an array of elements that have been filtered based on a condition."
               :arglists '([^{:doc "Condition predicate which will be applied to members of collection"} pred
                            ^{:doc "Collection of elements"} coll])})})
