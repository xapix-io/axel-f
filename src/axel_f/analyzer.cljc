(ns axel-f.analyzer
  (:require [axel-f.runtime :as runtime]))

(defmulti free-variables runtime/type)

(defmethod free-variables :default [_ & _])

(defmethod free-variables ::runtime/unary-expr [{::runtime/keys [expr]} & [vars]]
  (free-variables expr vars))

(defmethod free-variables ::runtime/binary-expr [{::runtime/keys [left-expr right-expr]} & [vars]]
  (concat
   (free-variables left-expr vars)
   (free-variables right-expr vars)))

(defmethod free-variables ::runtime/root-reference-expr [{::runtime/keys [field-expr]} & [vars]]
  (let [field (runtime/eval* field-expr nil nil nil)]
    (cons []
          (cons (cons field (first vars))
                (rest vars)))))

(defmethod free-variables ::runtime/reference-expr [{::runtime/keys [ctx-expr field-expr]} & [vars]]
  (let [field (runtime/eval* field-expr nil nil nil)]
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

(defn report [ast]
  {:vars (filter (fn [path]
                   (and (not-empty path)
                        (every? not-empty path)))
                 (distinct (free-variables ast)))})
