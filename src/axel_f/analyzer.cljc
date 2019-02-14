(ns axel-f.analyzer
  (:require [axel-f.functions :as core]
            [axel-f.runtime :as runtime]
            [axel-f.type-system :as type-system]))

(defmulti free-variables runtime/type)

(defmethod free-variables :default [_ & _])

(defmethod free-variables ::runtime/unary-expr [{::runtime/keys [expr]} & [vars]]
  (free-variables expr vars))

(defmethod free-variables ::runtime/binary-expr [{::runtime/keys [left-expr right-expr]} & [vars]]
  (concat
   (free-variables left-expr vars)
   (free-variables right-expr vars)))

(defmethod free-variables ::runtime/root-reference-expr [{::runtime/keys [field-expr]} & [vars]]
  (let [field (runtime/eval field-expr)]
    (cons []
          (cons (cons field (first vars))
                (rest vars)))))

(defmethod free-variables ::runtime/reference-expr [{::runtime/keys [ctx-expr field-expr]} & [vars]]
  (let [field (runtime/eval field-expr nil nil)]
    (free-variables ctx-expr (cons (cons field (first vars))
                                   (rest vars)))))

(defmethod free-variables ::runtime/index-expr [{::runtime/keys [ctx-expr ref-expr]} & [vars]]
  (if ctx-expr
    (concat (free-variables ctx-expr (cons (cons "*" (first vars))
                                           (rest vars)))
            (free-variables ref-expr []))
    (cons []
          (cons (cons "*" (first vars))
                (rest vars)))))

(defmethod free-variables ::runtime/list-expr [{::runtime/keys [exprs]} & [vars]]
  (apply concat
         (map #(free-variables % vars) exprs)))

(defmethod free-variables ::runtime/application-expr [{::runtime/keys [arg-list]} & [vars]]
  (free-variables arg-list vars))

(defmethod free-variables ::runtime/formula [{::runtime/keys [expr]}]
  (free-variables expr []))

(defmulti infer-type runtime/type)

(defmethod infer-type ::runtime/constant-expr [{::runtime/keys [value]}]
  (cond
    (number? value)
    (type-system/simple-type ::type-system/number)

    (string? value)
    (type-system/simple-type ::type-system/string)

    (boolean? value)
    (type-system/simple-type ::type-system/boolean)))

(defmethod infer-type ::runtime/unary-expr [{::runtime/keys [operator expr]}]
  (let [arg-type (infer-type expr)]
    ((:return (::runtime/meta operator)) arg-type)))

(defmethod infer-type ::runtime/binary-expr [{::runtime/keys [operator left-expr right-expr]}]
  (let [left-arg-type (infer-type left-expr)
        right-arg-type (infer-type right-expr)]
    ((:return (::runtime/meta operator)) left-arg-type right-arg-type)))

(defmethod infer-type ::runtime/list-expr [{::runtime/keys [exprs]}]
  (type-system/list-type (map infer-type exprs)))

(defmethod infer-type ::runtime/reference-expr [ref-expr]
  (let [[_ var] (free-variables ref-expr [])]
    (type-system/free-type var)))

(defmethod infer-type ::runtime/index-expr [index-expr]
  (let [[_ var] (free-variables index-expr [])]
    (type-system/free-type var)))

(defmethod infer-type ::runtime/application-expr [{::runtime/keys [fs arg-list]}]
  (let [arg-exprs (::runtime/exprs arg-list)]
    ;; TODO implementation for MAP, FILTER, SORT, IF and IFS
    (case fs
      "FILTER" (infer-type (second arg-exprs))
      "SORT" (infer-type (second arg-exprs))
      ((:return (core/find-meta fs)) nil))))

(defmethod infer-type ::runtime/formula [{::runtime/keys [expr]}]
  (infer-type expr))

(defn report [ast]
  {:vars (filter not-empty (distinct (free-variables ast)))})
