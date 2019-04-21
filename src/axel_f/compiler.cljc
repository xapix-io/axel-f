(ns axel-f.compiler
  (:refer-clojure :exclude [compile])
  (:require [axel-f.parser :as parser]
            [axel-f.lexer :as lexer]
            [clojure.string :as string]))

(defn switch-type [p]
  (cond
    (string? p) (keyword p)
    (keyword? p) (string/join "/" ((juxt namespace name) p))
    :else p))

(defn lookup [ctx [p & path :as px]]
  (if-not p
    ctx
    (let [idx? (and (vector? p) (first p))
          idx (and (vector? p) (second p))]
      (cond
        (and idx? (sequential? idx) (sequential? ctx))
        (map #(lookup (nth ctx % nil) path) idx)

        (and idx? (number? idx) (sequential? ctx))
        (lookup (nth ctx idx nil) path)

        (and idx? (= ::parser/select-all idx) (sequential? ctx))
        (map #(lookup % path) ctx)

        (and idx? (map? ctx))
        (lookup (get ctx idx) path)

        :else
        (lookup (or (get ctx p)
                    (get ctx (switch-type p))
                    (get-in ctx [:axel-f.runtime/context p])
                    (get-in ctx [:axel-f.runtime/context (switch-type p)]))
                path)))))

(defn compile [ast]
  (case (::parser/type ast)
    ::parser/formula
    (let [body (compile (::parser/body ast))
          fs (gensym)]
      (fn fs
        ([base-ctx] (fs base-ctx nil))
        ([base-ctx ctx]
         (body (assoc base-ctx :axel-f.runtime/context ctx)))))

    ::parser/constant
    (let [{::lexer/keys [value type]} ast
          v (if (= ::lexer/symbol type)
              (case (string/lower-case value)
                "true" true
                "false" false
                "null" nil)
              value)]
      (constantly v))

    ::parser/operator
    (fn [ctx] (get ctx (::lexer/value ast)))

    ::parser/application
    (let [f (compile (::parser/function ast))
          args (map compile (::parser/args ast))]
      (fn [ctx]
        (let [f (f ctx)]
          (if (:special? (meta f))
            ((f args (::parser/args ast)) ctx)
            (apply f (map #(% ctx) args))))))

    ::parser/var
    (let [path-fs (map (fn [p]
                         (cond
                           (::parser/type p)
                           (compile p)

                           (and (vector? p)
                                (= ::parser/list-ref (first p)))
                           (update-in p [1] (fn [x]
                                              (if (::parser/type x)
                                                (compile x)
                                                (constantly x))))

                           :else
                           (constantly p)))
                       (::parser/parts ast))]
      (fn [ctx] (lookup ctx (map (fn [p]
                                   (if (fn? p)
                                     (p ctx)
                                     (update-in p [1] #(% ctx))))
                                 path-fs))))

    ::parser/primary
    (let [args' (map compile (::parser/args ast))
          op (compile (::parser/operator ast))]
      (fn [ctx] (apply (op ctx) (map #(% ctx) args'))))

    ::parser/list
    (let [entries (map compile (::parser/entries ast))]
      (fn [ctx] (map #(% ctx) entries)))))
