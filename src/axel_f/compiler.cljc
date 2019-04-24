(ns axel-f.compiler
  (:refer-clojure :exclude [compile])
  (:require [axel-f.parser :as parser]
            [axel-f.lexer :as lexer]
            [clojure.string :as string]))

(defn switch-type [p]
  (cond
    (string? p) (keyword p)
    (keyword? p) (string/join "/" ((juxt namespace name) p))))

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
    (compile (::parser/body ast))

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
    (let [op-sym (::lexer/value ast)]
      (fn [ctx]
        (get ctx op-sym)))

    ::parser/application
    (let [f (compile (::parser/function ast))
          args-ast (::parser/args ast)
          args (map compile args-ast)]
      (fn [ctx]
        (let [f (f ctx)]
          (if (:special? (meta f))
            ((f args args-ast) ctx)
            (apply f
                   (for [x args]
                     (x ctx)))))))

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
      (fn [ctx]
        (lookup ctx
                (for [x path-fs]
                  (if (fn? x)
                    (x ctx)
                    [(first x)
                     ((second x) ctx)])))))

    ::parser/primary
    (let [args (map compile (::parser/args ast))
          op (compile (::parser/operator ast))]
      (fn [ctx]
        (apply (op ctx)
               (for [f args]
                 (f ctx)))))

    ::parser/list
    (let [entries (map compile (::parser/entries ast))]
      (fn [ctx]
        (for [x entries]
          (x ctx))))))
