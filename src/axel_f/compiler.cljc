(ns axel-f.compiler
  (:refer-clojure :exclude [compile])
  (:require [axel-f.parser :as parser]
            [axel-f.lexer :as lexer]
            [clojure.string :as string]))

(declare compile)

(defn switch-type [p]
  (cond
    (string? p) (keyword p)
    (keyword? p) (string/join "/" (filter identity ((juxt namespace name) p)))))

(defn index-lookup [idx ctxs]
  (reduce
   (fn [_ x]
     (when (sequential? x)
       (reduced (cond
                  (= ::parser/select-all idx) x
                  (number? idx) (nth x idx nil)
                  :else (throw (ex-info "Index lookup failed."))))))
   nil ctxs))

(defn map-lookup [idx ctxs]
  (reduce
   (fn [_ x]
     (when (map? x)
       (when-let [v (or (get x idx)
                        (get x (switch-type idx)))]
         (reduced v))))
   nil ctxs))

(defn lookup [ctx [p & path :as px]]
  (if-not p
    ctx
    (let [ctxs (filter identity (list ctx (:axel-f.runtime/context ctx)))]
      (if (vector? p)
        (if (sequential? (second p))
          (map #(lookup (index-lookup % ctxs) path) (second p))
          (lookup (index-lookup (second p) ctxs) path))
        (lookup (map-lookup p ctxs) path)))))

(defn compile-constant [ast]
  (let [{::lexer/keys [value type]} ast
        v (if (= ::lexer/symbol type)
            (case (string/lower-case value)
              "true" true
              "false" false
              "null" nil)
            value)]
    (constantly v)))

(defn compile-operator [ast]
  (let [op-sym (::lexer/value ast)]
    (fn [ctx]
      (get ctx op-sym))))

(defn compile-fn [args]
  (let [body (compile (last args))
        arglist (mapv #(first (::parser/parts %))
                      (butlast args))]
    (with-meta
      (fn [ctx]
        (fn [& args]
          (body (if-let [args (not-empty (mapcat identity (zipmap arglist args)))]
                  (apply assoc ctx args)
                  ctx))))
      {:free-variables (filter (fn [[v & _]]
                                 (not (contains? (set arglist) v)))
                               (:free-variables (meta body)))})))

(defn compile-if [args]
  (let [[test then else] args
        test (compile test)
        then (compile then)
        else (if else (compile else) (constantly nil))]
    (with-meta
      (fn [ctx]
        (if (test ctx)
          (then ctx)
          (else ctx)))
      {:free-variables (mapcat #(:free-variables (meta %)) [test then else])})))

(defn compile-ifs [args]
  (let [args (mapv compile args)]
    (with-meta
      (fn [ctx]
        (loop [[[test then :as test-then] & clauses] (partition-all 2 2 args)]
          (case (count test-then)
            0 nil
            1 (test ctx)
            (if (test ctx)
              (then ctx)
              (recur clauses)))))
      {:free-variables (mapcat #(:free-variables (meta %)) args)})))

(defn compile-with [args]
  (let [bindings
        (loop [bindings []
               binding-var (first args)
               binding-form (second args)
               args (nnext args)]
          (if binding-form
            (recur (conj bindings [(first (::parser/parts binding-var))
                                   (compile binding-form)])
                   (first args)
                   (second args)
                   (nnext args))
            (conj bindings (compile binding-var))))
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
      {:free-variables free-variables})))

(defn compile-application [{::parser/keys [parts] :as f} args]
  (case (string/join "." parts)
    "FN" (compile-fn args)
    "IF" (compile-if args)
    "IFS" (compile-ifs args)
    "WITH" (compile-with args)

    (let [f (compile f)
          args (mapv compile args)]
      (with-meta
        (fn [ctx]
          (apply (f ctx)
                 (for [x args]
                   (x ctx))))
        {:free-variables (mapcat #(:free-variables (meta %)) args)
         :fn-name (:free-variables (meta f))}))))

(defn compile-var-part [p]
  (cond
    (::parser/type p)
    (let [path-f (compile p)]
      (with-meta
        path-f
        {:var-part ::dynamic
         :free-variables (:free-variables (meta path-f))}))

    (and (vector? p)
         (= ::parser/list-ref (first p)))
    (let [[_ path-f] p
          path-f (if (or (= ::parser/select-all path-f)
                         (nil? (::parser/type path-f)))
                   (constantly path-f)
                   (compile path-f))]
      (with-meta
        (fn [ctx]
          [::parser/list-ref (path-f ctx)])
        {:var-part "*"
         :free-variables (:free-variables (meta path-f))}))

    :else
    (with-meta
      (constantly p)
      {:var-part p})))

(defn compile-var [ast]
  (let [path-fs (mapv compile-var-part
                      (::parser/parts ast))
        free-variables (mapcat #(:free-variables (meta %)) path-fs)
        free-variable (map #(:var-part (meta %)) path-fs)]
    (with-meta
      (fn [ctx]
        (lookup
         ctx
         (for [x path-fs]
           (x ctx))))
      {:free-variables (cons free-variable free-variables)})))

(defn compile-primary [{::parser/keys [args operator]}]
  (let [args (mapv compile args)
        op (compile operator)]
    (with-meta
      (fn [ctx]
        (apply (op ctx)
               (for [f args]
                 (f ctx))))
      {:free-variables (mapcat #(:free-variables (meta %)) args)})))

(defn compile-list [{::parser/keys [entries]}]
  (let [entries (mapv compile entries)]
    (with-meta
      (fn [ctx]
        (for [x entries]
          (x ctx)))
      {:free-variables (mapcat #(:free-variables (meta %)) entries)})))

(defn compile [ast]
  (case (::parser/type ast)
    ::parser/formula
    (compile (::parser/body ast))

    ::parser/constant
    (compile-constant ast)

    ::parser/operator
    (compile-operator ast)

    ::parser/application
    (compile-application
     (::parser/function ast)
     (::parser/args ast))

    ::parser/var
    (compile-var ast)

    ::parser/primary
    (compile-primary ast)

    ::parser/list
    (compile-list ast)))
