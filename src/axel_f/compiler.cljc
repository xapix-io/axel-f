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
                  (number? idx) (nth x idx nil)))))
   nil ctxs))

(defn map-lookup [idx ctxs]
  (reduce
   (fn [_ x]
     (when (map? x)
       (when-let [v (or (get x idx)
                        (get x (switch-type idx)))]
         (reduced v))))
   nil ctxs))

(defn lookup [ctx [p & path]]
  (if-not p
    ctx
    (let [ctxs (filter identity (list ctx (:axel-f.runtime/context ctx)))]
      (if (vector? p)
        (if (sequential? (second p))
          (map #(lookup (index-lookup % ctxs) path) (second p))
          (if (= ::parser/select-all (second p))
            (let [ctxs (index-lookup (second p) ctxs)]
              (when (and (sequential? ctxs)
                         (indexed? ctxs))
                (map #(lookup % path) ctxs)))
            (lookup (or (index-lookup (second p) ctxs)
                        (map-lookup (second p) ctxs))
                    path)))
        (lookup (map-lookup p ctxs) path)))))

(defn compile-constant [ast]
  (let [{::lexer/keys [value type]} ast
        v (if (= ::lexer/symbol type)
            (case (string/lower-case value)
              "true" true
              "false" false
              "null" nil)
            value)]
    (with-meta
      (constantly v)
      (select-keys ast [::lexer/begin ::lexer/end]))))

(defn compile-operator [{::lexer/keys [value begin] :as op-ast}]
  (with-meta
    (fn [ctx]
      (if-let [op (get ctx value)]
        op
        (throw (ex-info (str "Operator '" value "' doesn't have implementation.")
                        {:begin begin}))))
    (select-keys op-ast [::lexer/begin ::lexer/end])))

(defn compile-application [env {{::parser/keys [parts] :as f} ::parser/function
                                args ::parser/args
                                :as ast}]
  (let [f' (get env (apply str parts))
        f (if f'
            (f' args)
            (let [f (compile env f)
                  args (mapv (partial compile env) args)]
              (with-meta
                (fn [ctx]
                  (if-let [f' (f ctx)]
                    (apply f' (for [x args]
                                (x ctx)))
                    (throw (ex-info (str "Unknown function " (string/join "." (first (:free-variables (meta f))))) {}))))
                {:free-variables (mapcat #(:free-variables (meta %)) args)
                 :fn-name (:free-variables (meta f))})))
        fm (meta f)]
    (with-meta
      f
      (merge fm (select-keys ast [::lexer/begin ::lexer/end])
             {:args-count (count args)}))))

(defn compile-var-part [env p]
  (cond
    (::parser/type p)
    (let [path-f (compile env p)]
      (if (= ::parser/constant (::parser/type p))
        (let [var-part (path-f)]
          (with-meta
            (constantly var-part)
            {:var-part var-part}))
        (with-meta
          path-f
          {:var-part ::dynamic
           :free-variables (:free-variables (meta path-f))})))

    (and (vector? p)
         (= ::parser/list-ref (first p)))
    (let [[_ path-f] p
          path-f (if (or (= ::parser/select-all path-f)
                         (nil? (::parser/type path-f)))
                   (constantly path-f)
                   (compile env path-f))]
      (with-meta
        (fn [ctx]
          [::parser/list-ref (path-f ctx)])
        {:var-part "*"
         :free-variables (:free-variables (meta path-f))}))

    :else
    (with-meta
      (constantly p)
      {:var-part p})))

(defn compile-var [env ast]
  (let [path-fs (mapv (partial compile-var-part env)
                      (::parser/parts ast))
        free-variables (mapcat #(:free-variables (meta %)) path-fs)
        free-variable (map #(:var-part (meta %)) path-fs)]
    (with-meta
      (fn [ctx]
        (if (= ::dynamic (:var-part (meta (first path-fs))))
          (lookup ((first path-fs) ctx)
                  (for [x (rest path-fs)]
                    (x ctx)))
          (lookup
           ctx
           (for [x path-fs]
             (x ctx)))))
      (merge {:free-variables (cons free-variable free-variables)}
             (select-keys ast [::lexer/begin ::lexer/end])))))

(defn compile-primary [env {::parser/keys [args operator] :as p}]
  (let [args (mapv (partial compile env) args)
        op (compile-operator operator)]
    (with-meta
      (fn [ctx]
        (apply (op ctx)
               (for [f args]
                 (f ctx))))
      (merge {:free-variables (mapcat #(:free-variables (meta %)) args)}
             (select-keys p [::lexer/begin ::lexer/end])))))

(defn compile-list [env {::parser/keys [entries] :as l}]
  (let [entries (mapv (partial compile env) entries)]
    (with-meta
      (fn [ctx]
        (vec (for [x entries]
               (x ctx))))
      (merge {:free-variables (mapcat #(:free-variables (meta %)) entries)}
             (select-keys l [::lexer/begin ::lexer/end])))))

(defn compile
  "Compile ast to executable function

  `env` is a compile time environment with implementation for special forms
  `ast` abstract syntax tree"
  [env ast]
  (case (::parser/type ast)
    ::parser/formula
    (compile env (::parser/body ast))

    ::parser/constant
    (compile-constant ast)

    ::parser/application
    (compile-application env ast)

    ::parser/var
    (compile-var env ast)

    ::parser/primary
    (compile-primary env ast)

    ::parser/list
    (compile-list env ast)

    nil (constantly nil)))
